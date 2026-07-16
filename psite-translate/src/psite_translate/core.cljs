(ns psite-translate.core
  "Site-wide translation backend: fills EMPTY *_translations fields from the
   source-language reference row by calling the Ollama/Babashka LLM gateway.

   Site-agnostic — every site-specific input arrives through a `cfg` map, so this
   module has no dependency on any particular site's config or db namespace:

     {:translatables {<coll> {:table :<coll>_translations :fk :<coll>_id
                              :label-field :.. :status? <bool>
                              :fields {<field> :plain|:html ...}}}
      :site-context  {:site_name .. :site_description .. ...} ; handed to gateway
      :source-lang   \"de\"        ; reference language, default \"de\"
      :max-chars     30000        ; per-unit source cap, default 30000
      :gateway-url   \"https://...\"
      :gateway-key   \"...\"        ; server-side gateway secret
      :query         (fn [honeysql] -> js/Promise)} ; the site's db query fn

   The browser (psite-translate.admin) drives the work field-by-field so runs
   stay short and interruptible. These handlers are stateless except for the
   in-memory job registry (`jobs`), which is per app-server process.

   The three handlers are plain (cfg req) -> response|promise functions; a site
   wraps each with its own macchiato-async `defhandler` and supplies `cfg`.

   NOTE: `:translatables` is the single source of truth for what gets translated.
   The Directus schema snapshot is not shipped to prod, so this cannot be derived
   at runtime — the site keeps it in sync by hand when its translation schema
   changes (add/remove a translated field, or a new translated collection)."
  {:clj-kondo/config '{:lint-as {kitchen-async.promise/let clojure.core/let}}}
  (:require [clojure.string :as str]
            [kitchen-async.promise :as p]))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- blank? [v]
  (or (nil? v) (and (string? v) (str/blank? v))))

(defn- strip-html [s]
  (when s (-> s (str/replace #"<[^>]+>" " ") (str/replace #"\s+" " ") str/trim)))

(defn- preview [s]
  (let [t (or (strip-html s) "")]
    (if (> (count t) 90) (str (subs t 0 90) "…") t)))

(defn- json-response
  "Return Clojure data as the response body; the site's wrap-restful-format
   serializes it to JSON based on the request's Accept header."
  [status data]
  {:status status :body data})

(defn- unauthorized? [req] (not (:directus-user req)))

(defn- source-lang [cfg] (or (:source-lang cfg) "de"))

(defn site-context-from-settings
  "Assemble the gateway :site-context map from a config `setting` fn, reading the
   same LLM_SITE_* values the Directus extension uses (LLM_SITE_NAME →
   :llm-site-name, etc.). Blank/missing values are dropped, matching the
   extension's behaviour. Sites use this to keep the context identical on both
   the extension and site-wide paths from one env source."
  [setting]
  (into {}
        (remove (comp blank? val))
        {:site_name        (setting :llm-site-name)
         :site_description (setting :llm-site-description)
         :domain           (setting :llm-site-domain)
         :audience         (setting :llm-site-audience)
         :tone             (setting :llm-site-tone)}))

;; ---------------------------------------------------------------------------
;; Gateway call
;; ---------------------------------------------------------------------------

;; The gateway does the slow work off-request: POST /jobs starts an async job and
;; returns an id; GET /jobs/:id (no key) reports its state. Neither call is held
;; open for the translation, so no proxy (nginx in front of this app, or undici
;; here) can time it out. The browser drives start→poll via these two handlers.

(defn- gateway-base [cfg]
  (str/replace (:gateway-url cfg) #"/+$" ""))

(defn- gateway-start-job
  "POST /jobs — start an async translation. Resolves to the job-id string."
  [cfg text langs fmt]
  (let [key  (:gateway-key cfg)
        body (js/JSON.stringify
              (clj->js {:text             text
                        :target_languages langs
                        :source_language  (source-lang cfg)
                        :format           (if (= fmt :html) "html" "plain text")
                        :context          (:site-context cfg)}))]
    (p/let [resp (js/fetch (str (gateway-base cfg) "/jobs")
                           #js {:method  "POST"
                                :headers #js {"Content-Type"  "application/json"
                                              "Authorization" (str "Bearer " key)}
                                :body    body})]
      (if-not (.-ok resp)
        (p/let [t (.text resp)]
          (throw (js/Error. (str "gateway " (.-status resp) ": " (subs (or t "") 0 200)))))
        (p/let [json (.json resp)]
          (.-jobId json))))))

(defn- gateway-poll-job
  "GET /jobs/:id (no key needed). Resolves to {:status :translations :error}."
  [cfg job-id]
  (p/let [resp (js/fetch (str (gateway-base cfg) "/jobs/" (js/encodeURIComponent job-id)))]
    (if-not (.-ok resp)
      (p/let [t (.text resp)]
        (throw (js/Error. (str "gateway job " (.-status resp) ": " (subs (or t "") 0 200)))))
      (p/let [json (.json resp)]
        {:status       (.-status json)
         :translations (js->clj (.-translations json))
         :error        (.-error json)}))))

;; ---------------------------------------------------------------------------
;; Target languages (present in the languages table, minus the source)
;; ---------------------------------------------------------------------------

(defn- available-target-langs [cfg]
  (p/let [rows ((:query cfg) {:select [:code] :from [:languages]})]
    (->> rows (map :code) (remove #(= % (source-lang cfg))) set)))

;; ---------------------------------------------------------------------------
;; GET gaps
;; ---------------------------------------------------------------------------

(defn- collection-gaps
  "Work units for one collection given the requested target langs."
  [cfg coll {:keys [table fk label-field fields]} target-langs]
  (p/let [cols (into [fk :languages_code] (keys fields))
          rows ((:query cfg) {:select cols :from [table]})]
    (let [by-parent (group-by #(get % fk) rows)
          src       (source-lang cfg)]
      (for [[pk group] by-parent
            :let  [de (some #(when (= (:languages_code %) src) %) group)]
            :when de
            [field _fmt] fields
            :when (not (blank? (get de field)))
            :let  [missing (for [lang target-langs
                                 :let [trow (some #(when (= (:languages_code %) lang) %) group)]
                                 :when (blank? (get trow field))]
                             lang)]
            :when (seq missing)]
        {:collection (name coll)
         :field      (name field)
         :format     (name (get fields field))
         :pk         pk
         :langs      (vec missing)
         :label      (or (get de label-field) (str pk))
         :preview    (preview (get de field))}))))

(defn gaps
  "GET gaps handler body. Needs `req` with :directus-user and query-param `langs`
   (comma-separated target codes; empty → all available). Returns
   {:targets [...] :units [...]} or an error response."
  [cfg req]
  (if (unauthorized? req)
    (json-response 401 {:error "unauthorized"})
    (p/let [requested (some-> (get-in req [:query-params "langs"])
                              (str/split #",")
                              (->> (map str/trim) (remove str/blank?) set))
            available (available-target-langs cfg)
            targets   (if (seq requested)
                        (filter available requested)
                        available)]
      (if (empty? targets)
        (json-response 200 {:units [] :targets []})
        (p/let [per-coll (p/all (for [[coll ccfg] (:translatables cfg)]
                                  (collection-gaps cfg coll ccfg targets)))]
          (json-response 200 {:targets (vec targets)
                              :units   (vec (apply concat per-coll))}))))))

;; ---------------------------------------------------------------------------
;; Async translate: start a job, poll it, save on completion
;; ---------------------------------------------------------------------------

;; jobId -> the upsert context needed to write the result when the job finishes.
;; In-memory (single app-server instance); lost on restart, in which case the
;; browser's poll 404s and it re-runs the unit.
(defonce ^:private jobs (atom {}))

(defn- upsert-translation!
  "Write `value` into `field` of the (pk,lang) translation row: UPDATE if the
   row exists, else INSERT (copying status from the source row when present)."
  [cfg {:keys [table fk status?]} pk field lang value de-status]
  (p/let [existing ((:query cfg) {:select [:id] :from [table]
                                  :where [:and [:= fk pk] [:= :languages_code lang]]
                                  :limit 1})]
    (if-let [id (:id (first existing))]
      ((:query cfg) {:update table :set {field value} :where [:= :id id]})
      ((:query cfg) {:insert-into table
                     :values [(cond-> {fk pk :languages_code lang field value}
                                status? (assoc :status (or de-status "published")))]}))))

(defn start
  "POST /start body handler. Reads the source row, guards the length, kicks off a
   gateway job, and remembers what to write when it finishes. `req` needs
   :directus-user and :body {:collection :field :pk :langs}."
  [cfg req]
  (if (unauthorized? req)
    (json-response 401 {:error "unauthorized"})
    (let [{:keys [collection field pk langs]} (:body req)
          coll-cfg  (get (:translatables cfg) (keyword collection))
          field-kw  (keyword field)
          fmt       (get-in coll-cfg [:fields field-kw])
          max-chars (or (:max-chars cfg) 30000)
          src       (source-lang cfg)]
      (cond
        (or (nil? coll-cfg) (nil? fmt))
        (json-response 400 {:error (str "unknown collection/field: " collection "/" field)})

        (empty? langs)
        (json-response 400 {:error "no target languages"})

        :else
        (p/let [de-rows ((:query cfg) {:select (cond-> [field-kw]
                                                 (:status? coll-cfg) (conj :status))
                                       :from  [(:table coll-cfg)]
                                       :where [:and [:= (:fk coll-cfg) pk] [:= :languages_code src]]
                                       :limit 1})
                de      (first de-rows)
                text    (get de field-kw)]
          (cond
            (blank? text)
            (json-response 400 {:error "no source content"})

            (> (count text) max-chars)
            (json-response 400 {:error (str "Quelltext zu lang (" (count text) " Zeichen, max. "
                                            max-chars ") — bitte manuell übersetzen")})

            :else
            (p/let [job-id (gateway-start-job cfg text (vec langs) fmt)]
              (swap! jobs assoc job-id {:coll-cfg coll-cfg :pk pk :field field-kw
                                        :langs (vec langs) :de-status (:status de)})
              (json-response 202 {:jobId job-id}))))))))

(defn job
  "GET /job?id=<jobId> handler. Polls the gateway; on completion, writes the rows
   and drops the job. `req` needs :directus-user and query-param `id`. Returns
   {:status running|done|error}."
  [cfg req]
  (if (unauthorized? req)
    (json-response 401 {:error "unauthorized"})
    (let [job-id (get-in req [:query-params "id"])
          ctx    (get @jobs job-id)]
      (if (nil? ctx)
        (json-response 404 {:error "unknown or expired job"})
        (p/let [{:keys [status translations error]} (gateway-poll-job cfg job-id)]
          (cond
            (= status "done")
            (p/let [{:keys [coll-cfg pk field langs de-status]} ctx
                    _ (p/all (for [lang langs
                                   :let [val (get translations lang)]
                                   :when (not (blank? val))]
                               (upsert-translation! cfg coll-cfg pk field lang val de-status)))]
              (swap! jobs dissoc job-id)
              (json-response 200 {:status "done"}))

            (= status "error")
            (do (swap! jobs dissoc job-id)
                (json-response 200 {:status "error" :error error}))

            :else
            (json-response 200 {:status "running"})))))))

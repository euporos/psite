(ns psite-translate.admin
  "Site-wide translation dashboard (served by the site at /<locale>/admin).

   Flow: pick target languages → scan for empty fields → review the grouped list
   of gap fields (all checked by default) → translate the checked ones one at a
   time, with a progress bar and an interrupt button. Only empty fields are ever
   filled; the source-language reference is never touched.

   The browser drives the loop field-by-field: for each unit it POSTs /start to
   kick off an async gateway job, then polls /job until it finishes (the gateway
   does the slow work off-request, so no proxy read-timeout applies). Cancelling
   is trivial — `::cancel` just stops the loop from scheduling the next unit.

   Site-agnostic: the site calls `main` with a data map holding :locale,
   :csrf-token and :candidate-langs (the non-source target codes, which the site
   derives from its own config). The API base path is /<locale>/admin/api/... ."
  (:require
   [clojure.string :as str]
   [re-frame.core :as rf]
   [reagent.dom :as rdom]))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(def lang-names {"de" "Deutsch" "en" "Englisch" "uk" "Ukrainisch"
                 "nl" "Niederländisch" "it" "Italienisch" "fr" "Französisch"
                 "es" "Spanisch"})

(defn lang-label [code] (get lang-names code code))

(defn- unit-id [{:keys [collection pk field]}]
  (str collection "|" pk "|" field))

(defn- api-url [db path]
  (str "/" (:locale db) "/admin/api/translation" path))

;; ---------------------------------------------------------------------------
;; Effects (raw fetch → dispatch back)
;; ---------------------------------------------------------------------------

(rf/reg-fx
 :translate/fetch-gaps
 (fn [{:keys [url]}]
   (-> (js/fetch url #js {:headers     #js {"Accept" "application/json"}
                          :credentials "same-origin"})
       (.then (fn [r]
                (if (.-ok r)
                  (.then (.json r) #(rf/dispatch [::scan-done (js->clj % :keywordize-keys true)]))
                  (.then (.text r) #(rf/dispatch [::scan-error (str "HTTP " (.-status r) " " %)])))))
       (.catch (fn [e] (rf/dispatch [::scan-error (.-message e)]))))))

;; Run one unit: POST /start to kick off a gateway job, then poll /job until it
;; finishes. Both requests are short, so no proxy times out however long the
;; translation takes. Dispatches ::step-done exactly once (as before), so the
;; event flow is unchanged.
(rf/reg-fx
 :translate/run-unit
 (fn [{:keys [start-url job-url csrf unit]}]
   (let [id   (unit-id unit)
         ;; :invalid (target codes whose HTML the gateway rejected and withheld)
         ;; turns an otherwise-ok unit into a :warn — the other langs were saved,
         ;; these were not, so the field stays a gap for a manual fix/retry.
         done (fn [invalid]
                (rf/dispatch [::step-done id (if (seq invalid) [:warn (vec invalid)] :ok)]))
         err  (fn [msg] (rf/dispatch [::step-done id [:error msg]]))
         poll (fn poll [job-id]
                (-> (js/fetch (str job-url "?id=" (js/encodeURIComponent job-id))
                              #js {:headers     #js {"Accept" "application/json"}
                                   :credentials "same-origin"})
                    (.then (fn [r]
                             (if-not (.-ok r)
                               (.then (.text r) (fn [t] (err (str "HTTP " (.-status r) " " t))))
                               (.then (.json r)
                                      (fn [j]
                                        (let [m (js->clj j :keywordize-keys true)]
                                          (case (:status m)
                                            "done"  (done (:invalid m))
                                            "error" (err (or (:error m) "Fehler"))
                                            (js/setTimeout #(poll job-id) 2000))))))))
                    (.catch (fn [e] (err (.-message e))))))]
     (-> (js/fetch start-url
                   #js {:method      "POST"
                        :credentials "same-origin"
                        :headers     #js {"Content-Type" "application/json"
                                          "Accept"       "application/json"
                                          "X-CSRF-Token" csrf}
                        :body        (js/JSON.stringify
                                      (clj->js {:collection (:collection unit)
                                                :field      (:field unit)
                                                :pk         (:pk unit)
                                                :langs      (:langs unit)}))})
         (.then (fn [r]
                  (if-not (.-ok r)
                    (.then (.text r) (fn [t] (err (str "HTTP " (.-status r) " " t))))
                    (.then (.json r) (fn [j] (poll (:jobId (js->clj j :keywordize-keys true))))))))
         (.catch (fn [e] (err (.-message e))))))))

;; ---------------------------------------------------------------------------
;; Events
;; ---------------------------------------------------------------------------

(rf/reg-event-db
 ::init
 (fn [db [_ {:keys [locale csrf-token candidate-langs]}]]
   (let [cands (vec candidate-langs)]
     (assoc db :translate {:locale     (or locale "de")
                           :csrf       csrf-token
                           :candidates cands
                           :phase      :select
                           :langs      (set cands)
                           :units      []
                           :selected   #{}
                           :results    {}
                           :queue      []
                           :idx        0
                           :cancel?    false
                           :error      nil}))))

(rf/reg-event-db
 ::toggle-lang
 (fn [db [_ lang]]
   (update-in db [:translate :langs] (fn [s] (if (s lang) (disj s lang) (conj s lang))))))

(rf/reg-event-fx
 ::scan
 (fn [{:keys [db]} _]
   (let [langs (get-in db [:translate :langs])]
     {:db (update db :translate assoc :phase :scanning :error nil :results {})
      :translate/fetch-gaps
      {:url (api-url (:translate db) (str "/gaps?langs=" (str/join "," langs)))}})))

(rf/reg-event-db
 ::scan-done
 (fn [db [_ {:keys [units]}]]
   (update db :translate assoc
           :phase    :list
           :units    (vec units)
           :selected (set (map unit-id units))
           :results  {})))

(rf/reg-event-db
 ::scan-error
 (fn [db [_ msg]]
   (update db :translate assoc :phase :select :error msg)))

(rf/reg-event-db
 ::back
 (fn [db _] (update db :translate assoc :phase :select :units [] :selected #{} :results {})))

(rf/reg-event-db
 ::toggle-unit
 (fn [db [_ id]]
   (update-in db [:translate :selected] (fn [s] (if (s id) (disj s id) (conj s id))))))

(rf/reg-event-db
 ::toggle-collection
 (fn [db [_ coll]]
   (let [ids  (->> (get-in db [:translate :units])
                   (filter #(= (:collection %) coll))
                   (map unit-id) set)
         sel  (get-in db [:translate :selected])
         all? (every? sel ids)]
     (update-in db [:translate :selected]
                (fn [s] (if all? (apply disj s ids) (into s ids)))))))

(rf/reg-event-fx
 ::translate
 (fn [{:keys [db]} _]
   (let [{:keys [units selected]} (:translate db)
         queue (vec (filter #(selected (unit-id %)) units))]
     (if (empty? queue)
       {}
       {:db (update db :translate assoc
                    :phase :running :queue queue :idx 0 :cancel? false :results {})
        :dispatch [::step]}))))

(rf/reg-event-fx
 ::step
 (fn [{:keys [db]} _]
   (let [{:keys [queue idx cancel? csrf] :as t} (:translate db)]
     (if (or cancel? (>= idx (count queue)))
       {:dispatch [::finish]}
       {:translate/run-unit
        {:start-url (api-url t "/start")
         :job-url   (api-url t "/job")
         :csrf      csrf
         :unit      (nth queue idx)}}))))

(rf/reg-event-fx
 ::step-done
 (fn [{:keys [db]} [_ id result]]
   {:db (-> db
            (assoc-in [:translate :results id] result)
            (update-in [:translate :idx] inc))
    :dispatch [::step]}))

(rf/reg-event-db
 ::cancel
 (fn [db _] (assoc-in db [:translate :cancel?] true)))

(rf/reg-event-db
 ::finish
 (fn [db _] (assoc-in db [:translate :phase] :done)))

;; ---------------------------------------------------------------------------
;; Subs
;; ---------------------------------------------------------------------------

(rf/reg-sub ::t (fn [db _] (:translate db)))
(rf/reg-sub ::phase      :<- [::t] (fn [t _] (:phase t)))
(rf/reg-sub ::langs      :<- [::t] (fn [t _] (:langs t)))
(rf/reg-sub ::candidates :<- [::t] (fn [t _] (:candidates t)))
(rf/reg-sub ::units      :<- [::t] (fn [t _] (:units t)))
(rf/reg-sub ::selected   :<- [::t] (fn [t _] (:selected t)))
(rf/reg-sub ::results    :<- [::t] (fn [t _] (:results t)))
(rf/reg-sub ::error      :<- [::t] (fn [t _] (:error t)))

(rf/reg-sub ::progress :<- [::t]
            (fn [t _] {:done (count (:results t)) :total (count (:queue t))}))

(rf/reg-sub ::grouped :<- [::units]
            (fn [units _]
              (->> units (group-by :collection) (sort-by key))))

;; ---------------------------------------------------------------------------
;; Views
;; ---------------------------------------------------------------------------

(defn- language-picker []
  (let [langs @(rf/subscribe [::langs])
        cands @(rf/subscribe [::candidates])
        phase @(rf/subscribe [::phase])
        err   @(rf/subscribe [::error])]
    [:div.admin__section
     [:h2.admin__subtitle "1. Zielsprachen wählen"]
     (when err [:div.admin__note.admin__note--error err])
     [:div.admin__langs
      (for [l cands]
        ^{:key l}
        [:label
         [:input {:type "checkbox" :checked (boolean (langs l))
                  :on-change #(rf/dispatch [::toggle-lang l])}]
         (str " " (lang-label l))])]
     [:div.admin__actions
      [:button.kaufbutton
       {:disabled (or (empty? langs) (= phase :scanning))
        :on-click #(rf/dispatch [::scan])}
       (if (= phase :scanning) "Suche …" "Leere Felder suchen")]]]))

(defn- status-tag [result]
  (cond
    (nil? result)  [:span.admin__status "…"]
    (= result :ok) [:span.admin__status.admin__status--ok "✓"]
    (vector? result)
    (case (first result)
      ;; withheld invalid HTML — inline colour so no site-side CSS is required.
      :warn [:span.admin__status
             {:style {:color "#b45309" :font-weight "bold"}
              :title (str "Ungültiges HTML, nicht gespeichert: "
                          (str/join ", " (map lang-label (second result))))}
             "HTML ⚠"]
      [:span.admin__status.admin__status--error {:title (second result)} "Fehler"])
    :else [:span.admin__status "?"]))

(defn- unit-row [unit rod?]
  (let [selected @(rf/subscribe [::selected])
        results  @(rf/subscribe [::results])
        id       (unit-id unit)]
    [:tr
     [:td [:input {:type "checkbox" :checked (boolean (selected id))
                   :disabled rod?
                   :on-change #(rf/dispatch [::toggle-unit id])}]]
     [:td [:strong (:label unit)]]
     [:td [:code (:field unit)]]
     [:td (for [l (:langs unit)]
            ^{:key l} [:span.admin__tag (lang-label l)])]
     [:td.admin__preview (:preview unit)]
     (when rod?
       [:td (status-tag (get results id))])]))

(defn- collection-group [coll units rod?]
  (let [selected @(rf/subscribe [::selected])
        ids      (map unit-id units)
        all?     (every? selected ids)]
    [:<>
     [:tr.admin__group
      [:td [:input {:type "checkbox" :checked all?
                    :disabled rod?
                    :on-change #(rf/dispatch [::toggle-collection coll])}]]
      [:td {:col-span (if rod? 5 4)}
       [:strong coll] (str " (" (count units) ")")]]
     (for [u units] ^{:key (unit-id u)} [unit-row u rod?])]))

(defn- gap-list []
  (let [phase    @(rf/subscribe [::phase])
        grouped  @(rf/subscribe [::grouped])
        selected @(rf/subscribe [::selected])
        units    @(rf/subscribe [::units])
        progress @(rf/subscribe [::progress])
        running? (= phase :running)
        done?    (= phase :done)
        rod?     (or running? done?)]
    [:div.admin__section
     [:h2.admin__subtitle
      (str "2. Felder übersetzen — " (count units) " gefunden, "
           (count selected) " ausgewählt")
      (when-not rod?
        [:button.admin__back {:on-click #(rf/dispatch [::back])} "← Sprachen"])]

     (when (empty? units)
       [:div.admin__note "Keine leeren Felder in den gewählten Sprachen. 🎉"])

     (when rod?
       [:progress.admin__progress {:value (:done progress) :max (max 1 (:total progress))}
        (str (:done progress) "/" (:total progress))])

     (when (seq units)
       [:div.admin__tablewrap
        [:table.admin__table
         [:thead [:tr [:th] [:th "Eintrag"] [:th "Feld"] [:th "Sprachen"] [:th "Quelle (DE)"]
                  (when rod? [:th "Status"])]]
         [:tbody
          (for [[coll us] grouped]
            ^{:key coll} [collection-group coll us rod?])]]])

     [:div.admin__actions
      (if running?
        [:button.kaufbutton.kaufbutton--ghost {:on-click #(rf/dispatch [::cancel])} "Abbrechen"]
        [:button.kaufbutton
         {:disabled (empty? selected)
          :on-click #(rf/dispatch [::translate])}
         (str "Übersetzen (" (count selected) ")")])

      (when done?
        [:button.admin__back {:on-click #(rf/dispatch [::scan])} "Erneut scannen"])]]))

(defn admin []
  (let [phase @(rf/subscribe [::phase])]
    [:div.admin
     (if (= phase :select)
       [language-picker]
       [gap-list])]))

(defn ^:dev/after-load mount []
  (rdom/render [admin] (js/document.getElementById "admin")))

;; Entry point. `data` = {:locale :csrf-token :candidate-langs}. The site's own
;; ^:export main (referenced by the page's onload) computes :candidate-langs and
;; forwards the map here.
(defn main [data]
  (rf/dispatch-sync [::init data])
  (mount))

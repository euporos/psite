(ns psite-middleware.core
  "Ring-style middleware for Macchiato/Node.js applications.

   Contract: requests must carry a :config map (attached by wrap-config
   middleware in serving.core) containing the keys declared by
   `config-schema` here and by `psite-routing.core/config-schema`."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [macchiato-async.errors :as errors]
            [macchiato.util.response :as r]))

(def config-schema
  [:map
   [:psite-middleware/show-errors? {:optional true} :boolean]])

;; ---------------------------------------------------------------------------
;; Local helpers
;; ---------------------------------------------------------------------------

(defn- deep-merge [v & vs]
  (letfn [(rec-merge [v1 v2]
            (if (and (map? v1) (map? v2))
              (merge-with deep-merge v1 v2)
              v2))]
    (if (some identity vs) (reduce #(rec-merge %1 %2) v vs) (last vs))))

(defn wrap-synchronous-exceptions
  "Catches synchronous exceptions and converts them to ring error responses.
   Optional error-handler fn [e request] → response; defaults to built-in handler."
  ([handler] (wrap-synchronous-exceptions errors/default-error-response handler))
  ([error-handler handler]
   (fn [request respond _]
     (try
       (handler request respond _)
       (catch :default e
         (respond (error-handler e request)))))))

(defn wrap-error-response-for-user
  "Wraps handler so that error responses (4xx/5xx) pass through converter.
   Also catches synchronous exceptions via wrap-synchronous-exceptions.
   Marks converted responses with :converted-error? true to prevent
   double-conversion by outer wrappers."
  [converter handler]
  (fn [req res raise]
    ((wrap-synchronous-exceptions handler) req
     (fn [response]
       (res (if (and (errors/error-response? response)
                     (not (:converted-error? response)))
              (-> response
                  (assoc :original-request req)
                  converter
                  (assoc :converted-error? true))
              response)))
     raise)))

(defn wrap-cache
  "Sets Cache-Control headers based on response status and content-type.
   Error responses always get no-cache/no-store.
   config keys: :default (string), :status-codes (set), plus content-type strings."
  [handler {:keys [default status-codes]
            :or   {status-codes #{200 203 206}}
            :as   config}]
  (fn [req res raise]
    (handler req
             (fn [{:keys [status] :as response}]
               (let [content-type (re-find #"^[a-z/]+"
                                           (r/get-header response "Content-type"))]
                 (res (if (errors/error-response? response)
                        (update response :headers assoc
                                "Cache-Control" "no-cache, no-store, must-revalidate"
                                "Expires" 0
                                "Pragma" "no-cache")
                        (r/header response "Cache-Control"
                                  (when (status-codes status)
                                    (get config content-type default)))))))
             raise)))

(defn- stringify-if-edn [res]
  (fn [{:keys [headers] :as response-map}]
    (if (= "application/edn" (get headers "Content-Type"))
      (res (update response-map :body str))
      (res response-map))))

(defn wrap-edn-params
  "Parses EDN body from POST requests with content-type application/edn.
   Attaches parsed value as :edn-params on the request."
  [handler]
  (fn [req res raise]
    (if-not (and (= :post (:request-method req))
                 (= "application/edn" (get-in req [:headers "content-type"])))
      (handler req (stringify-if-edn res) raise)
      (try
        (let [data    (volatile! "")
              request (:node/request req)]
          ^js (.on request "data" (fn [chunk] (vswap! data #(str % chunk))))
          ^js (.on request "end"
                   (fn []
                     (handler (assoc req :edn-params (edn/read-string @data))
                              (stringify-if-edn res) raise))))
        (catch js/Error e
          (let [data (js->clj (.-data e))]
            (if (= :reader-exception (:type data))
              (-> (r/bad-request)
                  (r/content-type "application/edn")
                  (assoc :body (assoc data :message (.-message e)))
                  res)
              e)))))))

;; ---------------------------------------------------------------------------
;; Content Security Policy
;; ---------------------------------------------------------------------------

(defn- csp-nonce []
  (.toString (.randomBytes (js/require "crypto") 16) "base64url"))

(defn- render-csp [directives nonce]
  (->> directives
       (map (fn [[k vs]]
              (str (name k) " "
                   (str/join " " (map #(if (= % "'nonce'")
                                         (str "'nonce-" nonce "'")
                                         %)
                                      vs)))))
       (str/join "; ")))

(defn wrap-csp
  "Adds Content-Security-Policy header to all responses.
   directives is a map of directive keyword → seq of source strings.
   Use the literal string \"'nonce'\" as a source value to have it replaced
   with the per-request 'nonce-<value>' token. The raw nonce string is also
   attached as :csp-nonce on the request so templates can inject it via
   the nonce= attribute once inline scripts are migrated."
  [handler directives]
  (fn [req res raise]
    (let [nonce (csp-nonce)]
      (handler (assoc req :csp-nonce nonce)
               (fn [response]
                 (res (r/header response "Content-Security-Policy"
                                (render-csp directives nonce))))
               raise))))

(def default-messages
  {404 {:en "Not found"           :de "Nicht gefunden"}
   500 {:en "Something went wrong" :de "Etwas ist schief gelaufen"}})

(defn json-converter
  "Returns a converter fn that renders error responses as JSON.
   Reads :psite-middleware/show-errors? from :original-request :config.

   Options:
     :messages  {status {:locale \"text\"}} — deep-merged over default-messages,
                so partial overrides are fine (e.g. just change :en for 404)."
  ([]
   (json-converter {}))
  ([{:keys [messages]}]
   (let [msgs (merge-with merge default-messages messages)]
     (fn [response]
       (let [status      (:status response)
             show-errors? (get-in response [:original-request :config :psite-middleware/show-errors?])
             message     (or (get-in response [:body :message])
                             (get msgs status))]
         (if-not message
           response
           (cond-> {:body {:message message} :status status}
             show-errors? (assoc-in [:body :error]
                                         (-> response
                                             (get-in [:body :error])
                                             (select-keys [:message :name])))
             true (r/content-type "application/json"))))))))

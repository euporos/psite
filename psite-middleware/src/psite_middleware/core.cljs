(ns psite-middleware.core
  "Ring-style middleware for Macchiato/Node.js applications.

   Contract: requests must carry a :config map (attached by wrap-config
   middleware in serving.core) containing the keys declared by
   `config-schema` here and by `psite-routing.core/config-schema`."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.walk :as w]
            [macchiato.util.response :as r]))

(def config-schema
  [:map
   [:psite-middleware/hide-errors? {:optional true} :boolean]])

;; ---------------------------------------------------------------------------
;; Error handling (inlined from macchiato-async.errors)
;; ---------------------------------------------------------------------------

(defn- error-response?
  [response]
  (and (map? response)
       (let [s (:status response)]
         (and (number? s) (<= 400 s 599)))))

(defn- js-error? [x] (instance? js/Error x))

(defn- make-circular-safe [structure]
  (w/postwalk
   (fn [item]
     (if-not (or (map? item) (vector? item) (list? item)
                 (seq? item) (set? item) (number? item) (keyword? item))
       (str item)
       item))
   structure))

(defn- deep-merge [v & vs]
  (letfn [(rec-merge [v1 v2]
            (if (and (map? v1) (map? v2))
              (merge-with deep-merge v1 v2)
              v2))]
    (if (some identity vs) (reduce #(rec-merge %1 %2) v vs) (last vs))))

(defn- error->map [e]
  (let [exception-type (:type (.-data e))
        http-status    (:status (.-data e))]
    {:status (cond
               (= exception-type :reitit.coercion/request-coercion) 404
               http-status                                          http-status
               :else                                                500)
     :body   {:error {:name           (.-name e)
                      :exception-type exception-type
                      :message        (.-message e)
                      :data           (make-circular-safe (.-data e))
                      :stacktrace     (when (.-stack e)
                                        (vec (str/split (.-stack e) "\n")))}}}))

(defn- default-error-handler [e request]
  (let [e-map (if (js-error? e) (error->map e) e)]
    (-> {:status 500
         :body   {:error   {:message "Something went wrong (500)"}
                  :request (-> request
                               (dissoc :reitit.core/match :reitit.core/router)
                               make-circular-safe)}}
        (deep-merge e-map)
        (assoc :error? true))))

(defn wrap-synchronous-exceptions
  "Catches synchronous exceptions and converts them to ring error responses.
   Optional error-handler fn [e request] → response; defaults to built-in handler."
  ([handler] (wrap-synchronous-exceptions default-error-handler handler))
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
       (res (if (and (error-response? response)
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
                 (res (if (error-response? response)
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

(def default-messages
  {404 {:en "Not found"           :de "Nicht gefunden"}
   500 {:en "Something went wrong" :de "Etwas ist schief gelaufen"}})

(defn json-converter
  "Returns a converter fn that renders error responses as JSON.
   Reads :psite-middleware/hide-errors? from :original-request :config.

   Options:
     :messages  {status {:locale \"text\"}} — deep-merged over default-messages,
                so partial overrides are fine (e.g. just change :en for 404)."
  ([]
   (json-converter {}))
  ([{:keys [messages]}]
   (let [msgs (merge-with merge default-messages messages)]
     (fn [response]
       (let [status      (:status response)
             hide-errors? (get-in response [:original-request :config :psite-middleware/hide-errors?])
             message     (or (get-in response [:body :message])
                             (get msgs status))]
         (if-not message
           response
           (cond-> {:body {:message message} :status status}
             (not hide-errors?) (assoc-in [:body :error]
                                         (-> response
                                             (get-in [:body :error])
                                             (select-keys [:message :name])))
             true (r/content-type "application/json"))))))))

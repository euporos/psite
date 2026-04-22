(ns macchiato-async.middleware
  "Error handling middleware for Macchiato.
   Replaces wrap-synchronous-exceptions and wrap-error-response-for-user
   from psite.middleware."
  (:require
   [macchiato.util.response :as r]
   [macchiato-async.errors :as errors])
  (:require-macros
   [hiccups.core :refer [html5]]))

;; ---------------------------------------------------------------------------
;; Sync error boundary
;; ---------------------------------------------------------------------------

(defn wrap-sync-errors
  "Catches synchronous exceptions in the middleware chain
   (e.g. reitit coercion errors) and converts them to error responses."
  [handler]
  (fn [request respond _]
    (try
      (handler request respond _)
      (catch :default e
        (respond (errors/error->response e request))))))

;; ---------------------------------------------------------------------------
;; Error converters
;; ---------------------------------------------------------------------------

(defn html-converter
  "Returns a converter that renders error responses as simple HTML pages.
   This is a bare-bones default; projects should provide their own converter
   with locale support and site templates for production use."
  []
  (fn [response]
    (let [status  (:status response)
          message (get {404 "Not found"
                        403 "Forbidden"
                        500 "Something went wrong"}
                       status
                       "Something went wrong")]
      (-> {:body   (html5 [:body [:h1 (str message " (" status ")")]])
           :status status}
          (r/content-type "text/html")))))

(defn json-converter
  "Returns a converter that renders error responses as JSON.
   When expose-details? is true, includes error name and message."
  ([] (json-converter false))
  ([expose-details?]
   (fn [response]
     (let [status  (:status response)
           message (or (get-in response [:body :message])
                       (get {404 "Not found"
                             500 "Something went wrong"}
                            status))]
       (cond-> {:body   {:message (or message "Error")}
                :status status}
         expose-details? (assoc-in [:body :error]
                                   (-> response
                                       (get-in [:body :error])
                                       (select-keys [:message :name])))
         true (r/content-type "application/json"))))))

;; ---------------------------------------------------------------------------
;; Error response converter middleware
;; ---------------------------------------------------------------------------

(defn wrap-error-converter
  "Wraps a handler to convert error responses to user-friendly format.
   converter is a (fn [response] -> converted-response).
   Also catches synchronous exceptions via wrap-sync-errors."
  ([handler] (wrap-error-converter (html-converter) handler))
  ([converter handler]
   (fn [req res raise]
     ((wrap-sync-errors handler)
      req
      (fn [response]
        (res (if (and (errors/error-response? response)
                      (not (:converted-error? response)))
               (-> response
                   (assoc :original-request req)
                   converter
                   (assoc :converted-error? true))
               response)))
      raise))))

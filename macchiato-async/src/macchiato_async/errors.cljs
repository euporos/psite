(ns macchiato-async.errors
  "Consolidated error handling utilities for Macchiato handlers.
   Replaces error-related functions from psite.helpers and putils.helpers."
  (:require
   [clojure.string :as str]
   [clojure.walk :as w]))

;; ---------------------------------------------------------------------------
;; Predicates
;; ---------------------------------------------------------------------------

(defn error?
  "Returns true if x is a JavaScript Error instance."
  [x]
  (instance? js/Error x))

(defn error-response?
  "Returns true if response is a ring response map with error status (4xx/5xx)."
  [response]
  (and (map? response)
       (let [s (:status response)]
         (and (number? s) (<= 400 s 599)))))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- deep-merge [v & vs]
  (letfn [(rec-merge [v1 v2]
            (if (and (map? v1) (map? v2))
              (merge-with deep-merge v1 v2)
              v2))]
    (if (some identity vs)
      (reduce #(rec-merge %1 %2) v vs)
      (last vs))))

(defn make-circular-safe
  "Walks a data structure and stringifies any values that are not
   JSON-serializable Clojure types (maps, vectors, numbers, keywords, etc.)."
  [structure]
  (w/postwalk
   (fn [item]
     (if-not
      (or (map? item)
          (vector? item)
          (list? item)
          (seq? item)
          (set? item)
          (number? item)
          (keyword? item))
       (str item)
       item))
   structure))

(defn- string-to-vector [s]
  (when s (vec (str/split s "\n"))))

;; ---------------------------------------------------------------------------
;; Error conversion
;; ---------------------------------------------------------------------------

(defn error->map
  "Converts a js/Error to a ring-style error response map.
   Optional context map adds handler/route info at [:body :error :context]."
  ([e] (error->map e nil))
  ([e context]
   (let [exception-type (:type (.-data e))
         http-status    (:status (.-data e))]
     (cond-> {:status (cond
                        (= exception-type :reitit.coercion/request-coercion) 404
                        http-status http-status
                        :else 500)
              :body   {:error {:name           (.-name e)
                               :exception-type exception-type
                               :message        (.-message e)
                               :data           (make-circular-safe (.-data e))
                               :stacktrace     (string-to-vector (.-stack e))}}}
       context (assoc-in [:body :error :context] context)))))

(defn default-error-response
  "Builds a default error response. Accepts js/Error or an error map.
   Marks the response with :error? true."
  ([e request] (default-error-response e request nil))
  ([e request context]
   (if (error? e)
     (default-error-response (error->map e context) request context)
     (-> {:status 500
          :body   {:error   {:message "Something went wrong (500)"}
                   :request (-> request
                                ;; :config is the secrets-bearing map attached
                                ;; by wrap-config (see psite-middleware
                                ;; docstring); never embed it in a response.
                                (dissoc :reitit.core/match :reitit.core/router :config)
                                make-circular-safe)}}
         (deep-merge e)
         (assoc :error? true)))))

(defn error->response
  "Converts an error to a ring response map. If the request carries a custom
   :error-response handler, uses that; otherwise falls back to default-error-response."
  ([error request] (error->response error request nil))
  ([error request context]
   ((or (:error-response request)
        default-error-response)
    error request context)))

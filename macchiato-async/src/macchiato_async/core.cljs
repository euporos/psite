(ns macchiato-async.core
  "Handler boundary for Macchiato: wraps simple [req] -> response handlers
   into Macchiato's [req res raise] 3-arity, dispatching on promises,
   channels, errors, and plain maps."
  (:require
   [cljs.core.async :refer [go <!]]
   [cljs.core.async.impl.protocols :as async-protos]
   [macchiato.middleware.anti-forgery :as af]
   [macchiato.util.response :as r]
   [macchiato-async.errors :as errors]
   [taoensso.timbre :refer-macros [warnf errorf]])
  (:require-macros
   [macchiato-async.core]))

;; ---------------------------------------------------------------------------
;; Type predicates
;; ---------------------------------------------------------------------------

(defn thenable?
  "Returns true if x has a .then method (Promises/A+ duck-typing)."
  [x]
  (and (some? x) (fn? (.-then x))))

(defn channel?
  "Returns true if c implements the core.async ReadPort protocol."
  [c]
  (satisfies? async-protos/ReadPort c))

;; ---------------------------------------------------------------------------
;; Migration helper
;; ---------------------------------------------------------------------------

(defn chan->promise
  "Converts a core.async channel to a native js/Promise.
   Error values on the channel become rejections.
   Useful during migration when calling channel-returning functions
   from promise-based handlers."
  [ch]
  (js/Promise.
   (fn [resolve reject]
     (go (let [v (<! ch)]
           (if (errors/error? v)
             (reject v)
             (resolve v)))))))

;; ---------------------------------------------------------------------------
;; Response handling
;; ---------------------------------------------------------------------------

(defn- handler-context
  "Builds error context from the request and handler name."
  [req handler-name]
  (let [match (:reitit.core/match req)]
    (cond-> {}
      handler-name (assoc :handler-name handler-name)
      match        (assoc :route-template (:template match))
      (:url req)   (assoc :url (:url req)))))

(defn handle-response
  "Dispatches on the return value of a handler:
   - nil       → 204 No Content (with warning)
   - thenable  → .then/.catch with error boundary
   - channel   → go block with error check
   - js/Error  → error->response
   - otherwise → pass to res callback"
  [req res handler-name response]
  (cond
    (nil? response)
    (do (warnf "Handler %s returned nil response" (or handler-name "unknown"))
        (res (r/no-content)))

    (thenable? response)
    (-> response
        (.then (fn [result]
                 (handle-response req res handler-name result)))
        (.catch (fn [e]
                  (errorf "Promise rejected in handler %s: %s"
                          (or handler-name "unknown") (.-stack e))
                  (res (errors/error->response
                        e req (handler-context req handler-name))))))

    (channel? response)
    (go
      (try
        (let [result (<! response)]
          (if (errors/error? result)
            (do (errorf "Channel error in handler %s: %s"
                        (or handler-name "unknown") (.-stack result))
                (res (errors/error->response
                      result req (handler-context req handler-name))))
            (handle-response req res handler-name result)))
        (catch js/Error e
          (errorf "Channel exception in handler %s: %s"
                  (or handler-name "unknown") (.-stack e))
          (res (errors/error->response
                e req (handler-context req handler-name))))))

    (errors/error? response)
    (do (errorf "Handler %s returned error: %s"
                (or handler-name "unknown") (.-stack response))
        (res (errors/error->response
              response req (handler-context req handler-name))))

    :else
    (res response)))

;; ---------------------------------------------------------------------------
;; Handler wrapper
;; ---------------------------------------------------------------------------

(defn wrap-async
  "Wraps a simple [req] -> response handler into Macchiato's [req res raise]
   3-arity. Captures the anti-forgery token and dispatches the return value
   through handle-response."
  ([handler-fn] (wrap-async nil handler-fn))
  ([handler-name handler-fn]
   (fn [req res raise]
     (try
       (let [req (assoc req :af-token af/*anti-forgery-token*)]
         (handle-response req res handler-name (handler-fn req)))
       (catch js/Error e
         (errorf "Sync exception in handler %s: %s"
                 (or handler-name "unknown") (.-stack e))
         (res (errors/error->response
               e req (handler-context req handler-name))))))))

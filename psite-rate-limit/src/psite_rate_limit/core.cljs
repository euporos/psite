(ns psite-rate-limit.core
  "Per-IP token-bucket rate limiter for reitit routes.

   State: one atom keyed by [bucket-key ip] -> {:tokens :last-ms}.
   Single-instance, in-memory; lost on restart. Opportunistic GC from consume!."
  (:require [clojure.string :as str]
            [taoensso.timbre :refer [info warn]]))

(def config-schema
  [:map
   [:rate-limit/trust-proxy? {:optional true} :boolean]
   [:rate-limit/buckets {:optional true}
    [:map-of :keyword
     [:map
      [:capacity pos-int?]
      [:refill-per-sec number?]]]]])

(defonce state (atom {}))
(defonce ^:private gc-counter (atom 0))
(defonce ^:private logged-missing-ip? (atom false))

(defn- strip-v4-mapped [ip]
  (when ip (str/replace ip #"^::ffff:" "")))

(defn identify
  "Derives the client IP. Falls back through :remote-addr → node socket.
   X-Forwarded-For is only consulted when trust-proxy? is true."
  [req {:keys [trust-proxy?]}]
  (let [xff    (when trust-proxy?
                 (some-> (get-in req [:headers "x-forwarded-for"])
                         (str/split #",") first str/trim not-empty))
        direct (or (:remote-addr req)
                   (some-> req :node/request .-socket .-remoteAddress))
        ip     (strip-v4-mapped (or xff direct))]
    (when (and (nil? ip) (compare-and-set! logged-missing-ip? false true))
      (warn "rate-limit: could not derive client IP; limiter will be skipped for such requests"))
    ip))

(defn- idle-cap-seconds [{:keys [capacity refill-per-sec]}]
  (/ (double capacity) refill-per-sec))

(defn gc!
  "Drop entries whose bucket has fully refilled and been idle beyond cap/refill."
  [now-ms buckets]
  (swap! state
         (fn [s]
           (persistent!
            (reduce-kv
             (fn [acc [bkey _ip :as k] {:keys [tokens last-ms]}]
               (let [{:keys [capacity] :as bcfg} (get buckets bkey)]
                 (if (and bcfg
                          (>= tokens (double capacity))
                          (> (/ (- now-ms last-ms) 1000.0)
                             (idle-cap-seconds bcfg)))
                   acc
                   (assoc! acc k {:tokens tokens :last-ms last-ms}))))
             (transient {})
             s)))))

(defn- maybe-gc! [now-ms buckets]
  (when (zero? (mod (swap! gc-counter inc) 1024))
    (gc! now-ms buckets)))

(defn consume!
  "Attempt to consume one token from [bucket-key ip].
   Returns {:allowed? :remaining :retry-after-seconds}.
   Single-threaded JS: swap! never retries, so the volatile capture is safe."
  [bucket-key ip now-ms {:keys [capacity refill-per-sec]}]
  (let [k        [bucket-key ip]
        cap-d    (double capacity)
        decision (volatile! nil)]
    (swap! state
           (fn [s]
             (let [{:keys [tokens last-ms]
                    :or   {tokens cap-d last-ms now-ms}} (get s k)
                   elapsed-sec (/ (- now-ms last-ms) 1000.0)
                   refilled    (min cap-d (+ tokens (* refill-per-sec elapsed-sec)))]
               (if (>= refilled 1.0)
                 (do (vreset! decision
                              {:allowed?            true
                               :remaining           (int (js/Math.floor (- refilled 1.0)))
                               :retry-after-seconds 0})
                     (assoc s k {:tokens (- refilled 1.0) :last-ms now-ms}))
                 (do (vreset! decision
                              {:allowed?            false
                               :remaining           0
                               :retry-after-seconds (int (js/Math.ceil
                                                          (/ (- 1.0 refilled)
                                                             refill-per-sec)))})
                     (assoc s k {:tokens refilled :last-ms now-ms}))))))
    @decision))

(defn- rate-limit-response [bucket-key capacity retry-after-seconds]
  {:status  429
   :headers {"Content-Type"          "application/edn"
             "Retry-After"           (str retry-after-seconds)
             "X-RateLimit-Limit"     (str capacity)
             "X-RateLimit-Remaining" "0"}
   :body    {:error               :rate-limited
             :bucket              bucket-key
             :retry-after-seconds retry-after-seconds}})

(defn wrap-rate-limit
  "Per-route middleware. Skips when :devmode? is set, when no bucket config is
   found for bucket-key, or when no client IP can be derived."
  [bucket-key]
  (fn [handler]
    (fn [req res raise]
      (let [cfg        (:config req)
            devmode?   (:devmode? cfg)
            buckets    (:rate-limit/buckets cfg)
            bucket-cfg (get buckets bucket-key)]
        (if (or devmode? (nil? bucket-cfg))
          (handler req res raise)
          (let [now-ms (.now js/Date)
                ip     (identify req {:trust-proxy? (:rate-limit/trust-proxy? cfg)})]
            (if (nil? ip)
              (handler req res raise)
              (let [{:keys [allowed? retry-after-seconds]}
                    (consume! bucket-key ip now-ms bucket-cfg)]
                (maybe-gc! now-ms buckets)
                (if allowed?
                  (handler req res raise)
                  (do
                    (info (str "rate-limit denied bucket=" bucket-key
                               " ip=" ip
                               " retry-after=" retry-after-seconds "s"))
                    (res (rate-limit-response bucket-key
                                              (:capacity bucket-cfg)
                                              retry-after-seconds))))))))))))

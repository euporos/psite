(ns psite-transit.cljs-time
  "Transit handler pair for `goog.date.UtcDateTime` round-tripped through
  cljs-time accessors. Compose with `psite-transit.core/make-writer` /
  `make-reader`:

      (require '[psite-transit.core :as pt]
               '[psite-transit.cljs-time :as pt.time])

      (def writer (pt/make-writer {:handlers pt.time/write-handlers}))
      (def reader (pt/make-reader {:handlers pt.time/read-handlers}))

      (pt/serialize writer v)
      (pt/deserialize reader s)

  Requires cljs-time on the consumer's classpath — psite-transit itself
  does not depend on it."
  (:require [cljs-time.core :as t]
            [goog.date]))

(def tag "cljsdt")

(deftype ^:no-doc UtcDateTimeHandler []
  Object
  (tag [_ _] tag)
  (rep [_ v]
    #js [(t/year v)  (t/month v)  (t/day v)
         (t/hour v)  (t/minute v) (t/second v) (t/milli v)]))

(def write-handlers
  "Transit write-handler map: `goog.date.UtcDateTime` → \"cljsdt\"."
  {goog.date.UtcDateTime (UtcDateTimeHandler.)})

(def read-handlers
  "Transit read-handler map: \"cljsdt\" → `cljs-time.core/date-time`."
  {tag (fn [parts] (apply t/date-time parts))})

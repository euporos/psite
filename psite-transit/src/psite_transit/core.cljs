(ns psite-transit.core
  (:require [cljs-time.core :as t]
            [cognitect.transit :as transit]
            [goog.date]
            [goog.object]))

(deftype TimeHandler []
  Object
  (tag [_this _value] "cljsdt")
  (rep [_this value] #js [(t/year value)
                          (t/month value)
                          (t/day value)
                          (t/hour value)
                          (t/minute value)
                          (t/second value)
                          (t/milli value)]))

(def writer (transit/writer :json
                            {:handlers {goog.date.UtcDateTime (TimeHandler.)}}))

(def reader (transit/reader :json
                            {:handlers
                             {"cljsdt" (fn [year-to-milli]
                                         (apply t/date-time year-to-milli))}}))

(defn serialize [data]
  (transit/write writer data))

(defn deserialize [s]
  (transit/read reader s))

(ns psite-transit.core
  "Transit-JSON primitives. This namespace is handler-free — bring your own.
  For `goog.date.UtcDateTime` ↔ cljs-time round-tripping see
  `psite-transit.cljs-time`."
  (:require [cognitect.transit :as transit]))

(defn make-writer
  "Build a transit JSON writer. `opts` is passed to `cognitect.transit/writer`
  (e.g. `{:handlers {SomeType (SomeWriteHandler.)}}`)."
  ([]     (transit/writer :json))
  ([opts] (transit/writer :json opts)))

(defn make-reader
  "Build a transit JSON reader. `opts` is passed to `cognitect.transit/reader`
  (e.g. `{:handlers {\"tag\" (fn [rep] ...)}}`)."
  ([]     (transit/reader :json))
  ([opts] (transit/reader :json opts)))

(defn serialize
  "Write `data` as a transit JSON string using `writer`."
  [writer data]
  (transit/write writer data))

(defn deserialize
  "Read a transit JSON string using `reader`."
  [reader s]
  (transit/read reader s))

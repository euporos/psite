(ns directus-schema.runtime
  "Runtime helpers for Directus translation views: pool-side view creation,
   locale-aware COALESCE selection, and a `timestamp without time zone` parser
   that preserves the editor's wall-clock regardless of host TZ."
  (:require
   [clojure.string :as str]
   [cljs-time.coerce :as t.coerce]
   [honey.sql :as sql]
   [psite-pg.core :as pg]
   [taoensso.timbre :refer [infof errorf]]))

(defn ts-no-tz->cljs-time
  "Convert a `timestamp without time zone` value into a cljs-time DateTime
   whose UTC components match the wall-clock the user entered. Directus stores
   `dateTime` fields as wall-clock without zone; treating the value as UTC keeps
   cljs-time's UTC-default formatters rendering the right hour regardless of the
   Node process TZ. Accepts either the raw text node-pg might pass or the JS
   Date node-pg's default decoder produces."
  [v]
  (when v
    (let [d (cond
              (instance? js/Date v) v
              (string? v)           (js/Date. (str (str/replace v " " "T") "Z"))
              :else                 nil)]
      (when d
        (t.coerce/from-date
         (if (string? v)
           d
           (js/Date. (.UTC js/Date
                           (.getFullYear d) (.getMonth d) (.getDate d)
                           (.getHours d)    (.getMinutes d) (.getSeconds d)
                           (.getMilliseconds d)))))))))

(defn ensure-views!
  "Sequentially executes a vector of CREATE OR REPLACE VIEW HoneySQL maps
   (typically `directus-schema.core/view-defs` from the consumer) against the
   given pg pool. Returns a js/Promise that resolves once all views are tried."
  [pool view-defs]
  (infof "Creating %d translation views..." (count view-defs))
  (reduce
   (fn [chain view]
     (.then chain
            (fn [_]
              (let [[stmt] (sql/format view)]
                (infof "  SQL: %s" (subs stmt 0 (min 120 (count stmt))))
                (-> (pg/query pool view)
                    (.then  (fn [_] (infof "  View created OK")))
                    (.catch (fn [e] (errorf "View creation FAILED: %s" (.-message e)))))))))
   (js/Promise.resolve nil)
   view-defs))

(defn localized
  "Selects a translated field with locale fallback via COALESCE.
   `fallback-order` is a vector of locale keywords; the first attempt is
   `locale`, then the remaining entries (locale itself is filtered out).
   (localized :concerts.title :uk [:de :en :uk])
     => [[:coalesce :title_uk :title_de :title_en] :title]"
  [field-kw locale fallback-order]
  (let [n     (name field-kw)
        i     (.indexOf n ".")
        field (if (>= i 0) (subs n (inc i)) n)
        chain (into [locale] (remove #{locale} fallback-order))]
    [(into [:coalesce]
           (map #(keyword (str field "_" (name %))) chain))
     (keyword field)]))

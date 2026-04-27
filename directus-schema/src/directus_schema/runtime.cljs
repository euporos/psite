(ns directus-schema.runtime
  "Runtime helpers for Directus translation views: pool-side view creation,
   locale-aware COALESCE selection, and a `timestamp without time zone` parser
   that preserves the editor's wall-clock regardless of host TZ."
  (:require
   [clojure.string :as str]
   [cljs-time.coerce :as t.coerce]
   [cljs-time.core :as t]
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

(defn pg-date->cljs-time
  "Convert a Postgres DATE value (`type 1082`) into a cljs-time DateTime at
   UTC midnight on the same calendar day. node-postgres decodes DATE as a
   JS Date at the host's *local* midnight; reading its UTC instant (as
   cljs-time.coerce/from-date does) shifts the calendar day in non-UTC
   zones — e.g. CET `2026-01-01` becomes `2025-12-31T23:00Z`. We rebuild
   from the local components so the day is preserved regardless of TZ."
  [v]
  (when v
    (let [d (cond
              (instance? js/Date v) v
              (string? v)           (js/Date. v)
              :else                 nil)]
      (when d
        (t/date-time (.getFullYear d) (inc (.getMonth d)) (.getDate d))))))

(defn- view-name-of
  "Extracts the view name keyword from a :create-view HoneySQL clause that may
   be `[:or-replace name]`, `[name]`, or a bare name keyword."
  [view]
  (let [cv (:create-view view)]
    (cond
      (keyword? cv) cv
      (and (vector? cv) (= :or-replace (first cv))) (second cv)
      (vector? cv) (first cv)
      :else nil)))

(defn ensure-views!
  "Drops and recreates each translation view. We DROP ... CASCADE before CREATE
   instead of relying on CREATE OR REPLACE because Postgres' OR REPLACE rejects
   any change that reorders or removes columns — adding a new locale shifts the
   per-locale column interleave (name_de, name_en, name_uk → name_de, name_en,
   name_uk, name_it) and would otherwise silently fail. Returns a js/Promise
   that resolves once all views are processed."
  [pool view-defs]
  (infof "Recreating %d translation views..." (count view-defs))
  (reduce
   (fn [chain view]
     (.then chain
            (fn [_]
              (let [vname (view-name-of view)
                    drop-sql (str "DROP VIEW IF EXISTS \""
                                  (str/replace (name vname) "\"" "\"\"")
                                  "\" CASCADE")
                    [stmt] (sql/format view)]
                (infof "  SQL: %s" (subs stmt 0 (min 120 (count stmt))))
                (-> (.query pool drop-sql)
                    (.then (fn [_] (pg/query pool view)))
                    (.then  (fn [_] (infof "  View %s recreated OK" vname)))
                    (.catch (fn [e] (errorf "View %s recreation FAILED: %s"
                                            vname (.-message e)))))))))
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

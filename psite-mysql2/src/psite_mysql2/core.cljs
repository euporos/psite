(ns psite-mysql2.core
  "Minimal ClojureScript wrapper around mysql2/promise for Node.js.
   Returns native js/Promise instances."
  (:require
   ["mysql2/promise" :as mysql2]
   [honey.sql :as sql]))

;; ---------------------------------------------------------------------------
;; Type casting
;; ---------------------------------------------------------------------------

(defn build-type-parser
  "Converts a map of {type-keyword parser-fn} to a mysql2 typeCast callback.
   Example: {:DATETIME cljs-time.coerce/from-string, :TINY #(= \"1\" %)}"
  [definitions]
  (let [defs (reduce-kv (fn [m k v] (assoc m (name k) v)) {} definitions)]
    (fn [field next]
      (if (contains? defs (.-type field))
        (when-let [s (.string field)]
          ((get defs (.-type field)) s))
        (next)))))

;; ---------------------------------------------------------------------------
;; Pool lifecycle
;; ---------------------------------------------------------------------------

(defn create-pool
  "Creates a mysql2 connection pool from a Clojure config map.
   If :typeCast is a map, it is automatically wrapped via build-type-parser."
  [config]
  (let [config* (if (map? (:typeCast config))
                  (update config :typeCast build-type-parser)
                  config)]
    (.createPool mysql2 (clj->js config*))))

(defn end-pool
  "Gracefully closes the pool. Returns a native js/Promise."
  [pool]
  (.end pool))

;; ---------------------------------------------------------------------------
;; Result conversion
;; ---------------------------------------------------------------------------

(defn- convert-row [options main-table fields row]
  (reduce
   (fn [sofar [field [k v]]]
     (assoc sofar
            (keyword
             (when (:qualify-keys? options)
               (if (seq (.-table field))
                 (.-table field)
                 (name main-table)))
             k)
            v))
   {}
   (zipmap fields (js/Object.entries row))))

(defn- convert-result [options main-table fields results]
  (mapv (partial convert-row options main-table fields) results))

(defn- process-result [options honeysql result-array]
  (let [rows   (aget result-array 0)
        fields (aget result-array 1)]
    (if (js/Array.isArray rows)
      (convert-result options (:from honeysql) fields rows)
      rows)))

;; ---------------------------------------------------------------------------
;; Query execution
;; ---------------------------------------------------------------------------

(defn- do-log [options honeysql]
  (when-let [log-fn (:log-fn options)]
    (let [[pretty-sql & pretty-params] (sql/format honeysql {:pretty true})]
      (log-fn pretty-sql pretty-params))))

(defn- hsql-options
  "Strip library-specific keys before passing to sql/format."
  [options]
  (dissoc options :qualify-keys? :log-fn))

(defn query
  "Executes a HoneySQL map using pool.query() (no prepared statements).
   Returns a native js/Promise.
   SELECT → vector of Clojure maps. Non-SELECT → raw JS ResultSetHeader."
  ([pool honeysql]
   (query pool {} honeysql))
  ([pool options honeysql]
   (let [[sql-string & params] (sql/format honeysql (hsql-options options))]
     (do-log options honeysql)
     (.then (.query pool sql-string (clj->js (vec params)))
            (partial process-result options honeysql)))))

(defn execute
  "Like query but uses pool.execute() (prepared statements).
   Suitable for parameterized DML but not DDL."
  ([pool honeysql]
   (execute pool {} honeysql))
  ([pool options honeysql]
   (let [[sql-string & params] (sql/format honeysql (hsql-options options))]
     (do-log options honeysql)
     (.then (.execute pool sql-string (clj->js (vec params)))
            (partial process-result options honeysql)))))

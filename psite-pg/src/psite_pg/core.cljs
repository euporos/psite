(ns psite-pg.core
  "Minimal ClojureScript wrapper around node-pg for Node.js.
   Mirrors the psite-mysql2.core API. Returns native js/Promise instances."
  (:require
   ["pg" :as pg]
   [honey.sql :as sql]))

;; ---------------------------------------------------------------------------
;; Pool lifecycle
;; ---------------------------------------------------------------------------

(defn create-pool
  "Creates a node-pg connection pool from a Clojure config map.
   Recognised keys: :host :port :user :password :database (passed to pg.Pool).
   Parser keys (both optional, both pool-local — never mutate pg.types):
     :type-parsers  — {oid (fn [js-value] v)}. Runs in this lib *after*
                      node-pg's built-in decoder. Keyed by
                      Result.fields[i].dataTypeID.
     :text-parsers  — {oid (fn [raw-string] v)}. Installed via node-pg's
                      `types.getTypeParser` hook so it replaces node-pg's
                      built-in text decoder for that OID on this pool only."
  [config]
  (let [parsers      (:type-parsers config)
        text-parsers (:text-parsers config)
        types-opt    (when (seq text-parsers)
                       #js {:getTypeParser
                            (fn [oid fmt]
                              (or (when (= fmt "text")
                                    (get text-parsers oid))
                                  (.getTypeParser pg/types oid fmt)))})
        pg-cfg       (cond-> (dissoc config :type-parsers :text-parsers)
                       types-opt (assoc :types types-opt))
        pool         (new pg/Pool (clj->js pg-cfg))]
    (set! (.-__typeParsers ^js pool) (or parsers {}))
    pool))

(defn end-pool
  "Gracefully closes the pool. Returns a native js/Promise."
  [pool]
  (.end pool))

;; ---------------------------------------------------------------------------
;; Formatting & logging
;; ---------------------------------------------------------------------------

(defn- hsql-options
  "Strip library-specific keys and force the postgres dialect with numbered params."
  [options]
  (-> options
      (dissoc :log-fn)
      (assoc :numbered true :quoted true)))

(defn- do-log [options honeysql]
  (when-let [log-fn (:log-fn options)]
    (let [[pretty-sql & pretty-params]
          (sql/format honeysql (assoc (hsql-options options) :pretty true))]
      (log-fn pretty-sql pretty-params))))

;; ---------------------------------------------------------------------------
;; Result conversion
;; ---------------------------------------------------------------------------

(defn- field-parsers
  "Returns a map of field-name → parser-fn, derived from the pg Result.fields array
   and the caller-supplied type-parsers OID map. Fields without a parser are absent."
  [fields parsers]
  (persistent!
   (reduce (fn [m field]
             (if-let [parser (get parsers (.-dataTypeID field))]
               (assoc! m (.-name field) parser)
               m))
           (transient {})
           fields)))

(defn- convert-row [parser-by-name row]
  (persistent!
   (reduce (fn [m k]
             (let [v (aget row k)
                   v (if-let [p (get parser-by-name k)] (p v) v)]
               (assoc! m (keyword k) v)))
           (transient {})
           (js-keys row))))

(defn- process-result [pool result]
  (let [rows (.-rows result)]
    (if (js/Array.isArray rows)
      (let [parsers      (.-__typeParsers ^js pool)
            parser-by-nm (field-parsers (.-fields result) parsers)]
        (mapv #(convert-row parser-by-nm %) rows))
      result)))

;; ---------------------------------------------------------------------------
;; Query execution
;; ---------------------------------------------------------------------------

(defn query
  "Executes a HoneySQL map against the pool. Returns a native js/Promise.
   SELECT → vector of Clojure maps with keyword keys.
   Non-SELECT → raw pg Result object."
  ([pool honeysql]
   (query pool {} honeysql))
  ([pool options honeysql]
   (let [[sql-string & params] (sql/format honeysql (hsql-options options))]
     (do-log options honeysql)
     (.then (.query pool sql-string (clj->js (vec params)))
            (partial process-result pool)))))

(defn execute
  "Alias for `query`. node-pg has no separate prepared-statement entrypoint at
   the pool level; kept for API parity with psite-mysql2."
  ([pool honeysql]
   (query pool honeysql))
  ([pool options honeysql]
   (query pool options honeysql)))

(ns directus-schema.core
  "Compile-time schema validation for Directus-backed ClojureScript projects.

   Reads a Directus JSON schema snapshot at shadow-cljs compile time and emits:
   - A def for each collection (value: collection name keyword)
   - A def for each field, prefixed by collection (value: qualified keyword :collection.field)
   - Translation fields appear as fields of their parent collection
   - An `as` helper for HoneySQL SELECT aliasing
   - A `schema-meta` map for downstream tooling (views, write helpers)

   Usage in a .cljs file:
     (ns my.schema
       (:require-macros [directus-schema.core :refer [defschema]]))
     (defschema \"schema/snapshot.json\" {:locales [\"de\" \"en\" \"uk\"]})"
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [directus-schema.views :as views]))

;; ---------------------------------------------------------------------------
;; Snapshot loading
;; ---------------------------------------------------------------------------

(defn load-snapshot
  "Reads a Directus schema snapshot JSON file. Path is relative to project root
   (the JVM working directory during shadow-cljs compilation)."
  [path]
  (json/read-str (slurp path) :key-fn keyword))

;; ---------------------------------------------------------------------------
;; Snapshot parsing
;; ---------------------------------------------------------------------------

(defn- real-field?
  "A field represents a real database column when it has a schema entry
   and is not a Directus alias (M2M, translations, etc.)."
  [field]
  (some? (:schema field)))

(defn- system-collection?
  [collection-name]
  (str/starts-with? (name collection-name) "directus_"))

(defn- translation-table?
  [collection-name suffix]
  (str/ends-with? (name collection-name) suffix))

(defn- parent-collection-name
  "Derives the parent collection name from a translation table name.
   E.g. \"concerts_translations\" with suffix \"_translations\" => \"concerts\""
  [translation-table-name suffix]
  (let [s (name translation-table-name)]
    (subs s 0 (- (count s) (count suffix)))))

(defn- translation-system-field?
  "Translation tables have system fields (id, foreign key, language key)
   that should not appear as user-facing fields on the parent collection."
  [field-name parent-name language-key]
  (let [fk (str parent-name "_id")]
    (contains? #{"id" fk language-key} (name field-name))))

(defn parse-snapshot
  "Transforms a raw Directus schema snapshot into a schema model.

   Returns a map of collection-keyword to:
     {:fields       #{:col1 :col2 ...}          ;; base table columns
      :translations #{:title :description ...}}  ;; translation columns (if any)

   Config keys:
     :locales              - e.g. [\"de\" \"en\" \"uk\"]
     :translations-suffix  - default \"_translations\"
     :language-key         - default \"languages_code\""
  [snapshot config]
  (let [suffix   (get config :translations-suffix "_translations")
        lang-key (get config :language-key "languages_code")

        ;; Real fields grouped by collection name (as keyword)
        fields-by-coll
        (->> (:fields snapshot)
             (filter real-field?)
             (group-by (comp keyword :collection))
             (reduce-kv (fn [m k v]
                          (assoc m k (set (map (comp keyword :field) v))))
                        {}))

        all-colls  (set (map (comp keyword :collection) (:collections snapshot)))
        user-colls (remove system-collection? all-colls)

        ;; Build translation-field map: parent-keyword -> #{translated field keywords}
        translation-map
        (->> user-colls
             (filter #(translation-table? % suffix))
             (reduce
              (fn [m tt]
                (let [parent   (parent-collection-name tt suffix)
                      fields   (get fields-by-coll tt #{})
                      user-fld (into #{} (remove #(translation-system-field? % parent lang-key))
                                     fields)]
                  (assoc m (keyword parent) user-fld)))
              {}))

        ;; Base collections = user collections minus system and translation tables
        base-colls (remove #(or (system-collection? %)
                                (translation-table? % suffix))
                           user-colls)]

    (reduce
     (fn [schema coll]
       (let [fields       (get fields-by-coll coll #{})
             translations (get translation-map coll #{})
             ;; When a field name exists in both base and translations,
             ;; treat it as a translation field (translated version wins).
             base-only    (into #{} (remove translations) fields)]
         (assoc schema coll
                (cond-> {:fields base-only}
                  (seq translations) (assoc :translations translations)))))
     (sorted-map)
     (sort base-colls))))

;; ---------------------------------------------------------------------------
;; defschema macro
;; ---------------------------------------------------------------------------

(defn- field-symbol
  "Creates a symbol like `concerts-id` from collection keyword :concerts and field keyword :id."
  [coll field]
  (symbol (str (name coll) "-" (name field))))

(defn- field-keyword
  "Creates a dot-qualified keyword like :concerts.id for use in HoneySQL.
   Note: this is NOT a namespaced keyword — (namespace :concerts.id) is nil.
   HoneySQL renders it as `concerts`.`id`."
  [coll field]
  (keyword (str (name coll) "." (name field))))

(defmacro defschema
  "Reads a Directus schema snapshot and populates the current namespace with
   collection and field defs for compile-time validated query building.

   Arguments:
     snapshot-path - path to Directus JSON snapshot (relative to project root)
     config        - map with keys:
                       :locales              - vector of locale strings
                       :translations-suffix  - (optional, default \"_translations\")
                       :language-key         - (optional, default \"languages_code\")

   Emits into the current namespace:
     - Per collection: (def concerts :concerts)
     - Per translated collection: (def concerts_v :concerts_v) — view name
     - Per field:      (def concerts-id :concerts.id)
     - Translation fields under parent: (def concerts-title :concerts.title)
     - (defn as [kw] ...) — HoneySQL SELECT alias helper
     - (def schema-meta {...}) — full schema metadata for tooling
     - (def view-defs [...]) — HoneySQL maps for CREATE OR REPLACE VIEW"
  [snapshot-path config]
  (let [snapshot (load-snapshot snapshot-path)
        schema   (parse-snapshot snapshot config)

        coll-defs
        (for [coll (keys schema)]
          `(def ~(symbol (name coll)) ~coll))

        field-defs
        (for [[coll {:keys [fields]}] schema
              field (sort fields)]
          `(def ~(field-symbol coll field) ~(field-keyword coll field)))

        trans-defs
        (for [[coll {:keys [translations]}] schema
              :when translations
              field (sort translations)]
          `(def ~(field-symbol coll field) ~(field-keyword coll field)))

        ;; View name symbols for collections with translations
        view-defs
        (for [[coll {:keys [translations]}] schema
              :when translations]
          `(def ~(symbol (str (name coll) "_v")) ~(keyword (str (name coll) "_v"))))

        ;; HoneySQL maps for CREATE OR REPLACE VIEW (executed at server startup)
        view-hsql (views/generate-all-views schema config)

        ;; Print schema summary at compile time
        _ (do (println "\n[defschema] Loaded" (count schema) "collections from" snapshot-path)
              (doseq [[coll {:keys [fields translations]}] schema]
                (println (str "  " (name coll)
                              (when translations (str " → " (name coll) "_v"))))
                (println (str "    fields:  " (str/join ", " (sort (map name fields)))))
                (when translations
                  (println (str "    transl:  " (str/join ", " (sort (map name translations))))))))

        ;; Attach locales to schema-meta for downstream use (views, writes)
        meta-with-locales
        (reduce-kv (fn [m k v]
                     (assoc m k (assoc v :locales (:locales config))))
                   (sorted-map) schema)]

    `(do
       ;; as — wraps a dot-qualified keyword into a [kw alias] pair for HoneySQL SELECT.
       ;; (:concerts.id) => [:concerts.id :concerts_id]
       (defn ~'as [qualified-kw#]
         (let [n# (~'name qualified-kw#)
               i# (.indexOf n# ".")]
           [qualified-kw# (~'keyword (if (>= i# 0)
                                       (str (~'subs n# 0 i#) "_" (~'subs n# (~'inc i#)))
                                       n#))]))

       ~@coll-defs
       ~@view-defs
       ~@field-defs
       ~@trans-defs

       (def ~'schema-meta ~meta-with-locales)
       (def ~'view-defs ~view-hsql))))

(ns directus-schema.views
  "Generates HoneySQL CREATE OR REPLACE VIEW maps for Directus collections
   that have associated translation tables.

   Each translated collection gets a view named {collection}_v that LEFT JOINs
   the translation table once per locale, exposing raw per-locale columns
   (e.g. title_de, title_en, title_uk) alongside the base table columns.

   Called by directus-schema.core/defschema at compile time; view maps are
   emitted as view-defs and executed at server startup."
  (:require [honey.sql :as sql]))

(defn- dot-kw
  "Joins keywords with a dot, e.g. (dot-kw :c :id) -> :c.id"
  [& parts]
  (keyword (apply str (interpose "." (map name parts)))))

(defn- alias-kw
  "Creates a field_locale keyword, e.g. (alias-kw :title \"de\") -> :title_de"
  [field locale]
  (keyword (str (name field) "_" locale)))

(defn view-name
  "Returns the view name keyword for a collection, e.g. :concerts -> :concerts_v"
  [collection]
  (keyword (str (name collection) "_v")))

(defn generate-view
  "Generates a HoneySQL map for CREATE OR REPLACE VIEW for a single translated collection.

   Arguments:
     collection   - collection keyword, e.g. :concerts
     fields       - set of base field keywords
     translations - set of translation field keywords
     locales      - vector of locale strings, e.g. [\"de\" \"en\" \"uk\"]
     config       - map with :translations-suffix, :language-key"
  [collection fields translations locales config]
  (let [suffix    (get config :translations-suffix "_translations")
        lang-key  (get config :language-key "languages_code")
        trans-tbl (keyword (str (name collection) suffix))
        fk        (keyword (str (name collection) "_id"))

        ;; Base columns: c.id, c.datetime, ...
        base-cols (mapv #(dot-kw :c %) (sort fields))

        ;; Translation columns: [t_de.title :title_de], ...
        trans-cols (vec (for [field (sort translations)
                             locale locales
                             :let [alias (keyword (str "t_" locale))]]
                         [(dot-kw alias field) (alias-kw field locale)]))

        ;; LEFT JOINs: one per locale
        ;; Locale values are inlined since DDL doesn't support parameters
        joins (vec (mapcat
                    (fn [locale]
                      (let [alias (keyword (str "t_" locale))]
                        [[trans-tbl alias]
                         [:and
                          [:= (dot-kw alias fk) (dot-kw :c :id)]
                          [:= (dot-kw alias (keyword lang-key)) [:inline locale]]]]))
                    locales))]

    (-> {:select    (into base-cols trans-cols)
         :from      [[collection :c]]
         :left-join joins}
        (assoc :create-view [:or-replace (view-name collection)]))))

(defn generate-all-views
  "Generates HoneySQL view maps for all translated collections in a parsed schema.

   Arguments:
     schema  - output of directus-schema.core/parse-snapshot
     config  - map with :locales, :translations-suffix, :language-key

   Returns a vector of HoneySQL maps."
  [schema config]
  (let [locales (:locales config)]
    (into []
          (for [[coll {:keys [fields translations]}] schema
                :when translations]
            (generate-view coll fields translations locales config)))))

(defn views->sql
  "Formats a vector of HoneySQL view maps into SQL strings.
   Useful for debugging: (doseq [s (views->sql views)] (println s))"
  [views]
  (mapv #(first (sql/format % {:quoted false})) views))

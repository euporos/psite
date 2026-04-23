(ns psite-seo.json-ld
  "Typed schema.org JSON-LD constructors.

  Typed builders return plain maps, not <script> tags. Pass one or many
  maps to `ld` to emit a single <script>. Vector input becomes an @graph,
  so multiple entities on one page share a script and can cross-reference
  each other via @id.

  Keys `\"@context\"` and `\"@type\"` are strings (the only JSON-LD keys
  with non-identifier characters). Other keys are normal keywords
  (:name, :startDate, …) and `clj->js` converts them at emit time."
  (:require [psite-seo.util :as util]))

(defn entity
  "(entity :MusicEvent {...}) → assoc \"@type\". Escape hatch for any
  schema.org type not covered by a named constructor."
  [type m]
  (assoc m "@type" (if (keyword? type) (name type) type)))

(defn music-event  [m] (entity :MusicEvent m))
(defn event        [m] (entity :Event m))
(defn organization [m] (entity :Organization m))
(defn website      [m] (entity :WebSite m))
(defn person       [m] (entity :Person m))
(defn article      [m] (entity :Article m))

(defn breadcrumb-list
  "items: [{:name :url}] in display order. Returns a BreadcrumbList map."
  [items]
  (entity :BreadcrumbList
          {:itemListElement
           (vec (map-indexed
                 (fn [i {:keys [name url]}]
                   (entity :ListItem
                           {:position (inc i) :name name :item url}))
                 items))}))

(defn item-list
  "items: seq of entity maps. Wraps each in a ListItem at its 1-based
  position."
  [items]
  (entity :ItemList
          {:itemListElement
           (vec (map-indexed
                 (fn [i item]
                   (entity :ListItem (assoc item :position (inc i))))
                 items))}))

(defn ld
  "Wrap a schema.org map (or vector of maps) in a
  <script type=application/ld+json>. Vector input is emitted as a single
  @graph so multiple entities share one tag. Nils and empty collections
  are stripped recursively from anywhere in the tree. Returns nil if the
  input strips down to nothing."
  [data]
  (let [stripped (util/strip-nil data)]
    (cond
      (nil? stripped) nil

      (sequential? stripped)
      (util/script-tag {"@context" "https://schema.org"
                        "@graph"   (vec stripped)})

      (map? stripped)
      (util/script-tag
       (cond-> stripped
         (not (contains? stripped "@context"))
         (assoc "@context" "https://schema.org"))))))

(ns psite-seo.util
  "Internal primitives: nil-stripping and JSON-LD <script> emission."
  (:require [clojure.string :as str]))

(defn- non-empty? [x]
  (not (or (nil? x)
           (and (coll? x) (empty? x)))))

(defn strip-nil
  "Recursively remove nil values and empty collections from nested maps
  and sequentials. Lets JSON-LD call sites omit `when` guards around
  conditionally-present fields."
  [x]
  (cond
    (map? x)
    (reduce-kv (fn [m k v]
                 (let [v' (strip-nil v)]
                   (cond-> m (non-empty? v') (assoc k v'))))
               {} x)

    (sequential? x)
    (into [] (comp (map strip-nil) (filter non-empty?)) x)

    :else x))

(defn json-ld-string
  "JSON-stringify `data`, escaping any `</` sequence so a string value
  containing `</script>` cannot break out of the surrounding <script> tag."
  [data]
  (-> data clj->js js/JSON.stringify
      (str/replace "</" "<\\/")))

(defn script-tag
  "Hiccup [:script {:type application/ld+json} <json>]. Uses a plain string
  child, not :dangerouslySetInnerHTML — correct for server hiccup."
  [data]
  [:script {:type "application/ld+json"}
   (json-ld-string data)])

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
  "Hiccup [:script {:type application/ld+json} ...]. Emits the JSON via
  :dangerouslySetInnerHTML so the string content is not HTML-escaped —
  browsers do not decode entities inside <script>, so escaped quotes
  (&quot;) would otherwise leave the JSON-LD unparseable. Both hiccups
  (server) and React (client) special-case this attribute."
  [data]
  [:script {:type "application/ld+json"
            :dangerouslySetInnerHTML {:__html (json-ld-string data)}}])

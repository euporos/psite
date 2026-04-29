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
  "Emit <script type=application/ld+json>JSON</script> as raw HTML.

  The naive form [:script {:type \"application/ld+json\"} json-string]
  fails on both sides of the divide:
    - macchiato/hiccups (server) escapes string children unconditionally,
      turning \" into &quot;. Browsers do NOT decode entities inside a
      <script> raw-text element, so the JSON-LD parser sees &quot; and
      rejects it.
    - React's :dangerouslySetInnerHTML wraps correctly, but hiccups'
      implementation of that attribute discards the wrapping tag entirely
      and emits only the __html — stripping the <script> wrapper.

  Workaround: pre-render the full <script>…</script> string and pass it
  through :dangerouslySetInnerHTML on a dummy element. Hiccups discards
  the dummy wrapper and emits our raw script tag verbatim; React renders
  the dummy as a real element containing the raw script tag — also valid
  HTML for ld+json since it ignores nested layout."
  [data]
  (let [html (str "<script type=\"application/ld+json\">"
                  (json-ld-string data)
                  "</script>")]
    [:span {:dangerouslySetInnerHTML {:__html html}}]))

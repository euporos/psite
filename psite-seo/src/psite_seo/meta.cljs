(ns psite-seo.meta
  "Hiccup emitters for <meta> tags (and <title>, for call-site uniformity).
  Every emitter returns nil when its content is nil/blank so callers can
  splice them into a head vector with no surrounding `when` guards."
  (:require [clojure.string :as str]))

(defn- blank? [x]
  (or (nil? x)
      (and (string? x) (str/blank? x))))

(defn title [s]
  (when-not (blank? s) [:title s]))

(defn meta-tag [name content]
  (when-not (blank? content)
    [:meta {:name name :content content}]))

(defn property-tag [property content]
  (when-not (blank? content)
    [:meta {:property property :content content}]))

(defn http-equiv [equiv content]
  (when-not (blank? content)
    [:meta {:http-equiv equiv :content content}]))

(defn description [s] (meta-tag "description" s))

(defn keywords [xs-or-s]
  (meta-tag "keywords"
            (cond
              (string? xs-or-s)     xs-or-s
              (sequential? xs-or-s) (str/join ", " xs-or-s))))

(defn robots [x]
  (meta-tag "robots"
            (cond
              (nil? x)     nil
              (keyword? x) (name x)
              (set? x)     (str/join "," (map name x))
              :else        (str x))))

(defn viewport
  ([]  (viewport "width=device-width, initial-scale=1"))
  ([s] (meta-tag "viewport" s)))

(defn author      [s] (meta-tag "author" s))
(defn theme-color [s] (meta-tag "theme-color" s))

(defn charset
  ([]  (charset "utf-8"))
  ([s] (when-not (blank? s) [:meta {:charset s}])))

(defn google-site-verification [s]
  (meta-tag "google-site-verification" s))

;; Open Graph

(defn og-title       [s]   (property-tag "og:title" s))
(defn og-description [s]   (property-tag "og:description" s))
(defn og-url         [url] (property-tag "og:url" url))
(defn og-site-name   [s]   (property-tag "og:site_name" s))

(defn og-type
  ([]  (og-type "website"))
  ([s] (property-tag "og:type" s)))

(defn og-locale
  "Emits og:locale. Accepts either hyphen (hreflang-style \"de-DE\") or
  underscore (\"de_DE\") form; always normalises to underscore for OG."
  [s]
  (when-not (blank? s)
    (property-tag "og:locale" (str/replace s "-" "_"))))

(defn og-image
  "Accepts a plain URL string or a map {:url :width :height :alt}.
  The map form expands to og:image + og:image:width/height/alt secondaries."
  [url-or-map]
  (cond
    (blank? url-or-map)  nil
    (string? url-or-map) (property-tag "og:image" url-or-map)
    (map? url-or-map)
    (let [{:keys [url width height alt]} url-or-map]
      (list (property-tag "og:image" url)
            (when width  (property-tag "og:image:width"  (str width)))
            (when height (property-tag "og:image:height" (str height)))
            (when alt    (property-tag "og:image:alt"    alt))))))

(defn open-graph
  "Emit a sequence of og:* meta tags from `m`.
   Keys: :title :description :image :url :type :site-name :locale.
   :image is a URL string or a map with :url/:width/:height/:alt.
   Nil-valued keys are dropped."
  [m]
  (let [{:keys [title description image url type site-name locale]} m]
    (list (og-title       title)
          (og-description description)
          (og-image       image)
          (og-url         url)
          (og-type        (or type "website"))
          (og-site-name   site-name)
          (og-locale      locale))))

;; Twitter card

(defn- twitter-meta [k v]
  (meta-tag (str "twitter:" (name k)) v))

(defn twitter-card
  "Emit a sequence of twitter:* meta tags.
   m keys: :card (default \"summary_large_image\"), :title, :description,
   :image, :image-alt, :site, :creator."
  [m]
  (let [{:keys [card title description image image-alt site creator]
         :or   {card "summary_large_image"}} m]
    (list (twitter-meta :card        card)
          (twitter-meta :title       title)
          (twitter-meta :description description)
          (twitter-meta :image       image)
          (twitter-meta :image:alt   image-alt)
          (twitter-meta :site        site)
          (twitter-meta :creator     creator))))

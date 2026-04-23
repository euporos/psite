(ns psite-seo.link
  "Hiccup emitters for <link> tags: canonical, alternate (hreflang),
  icons, manifest."
  (:require [clojure.string :as str]))

(defn- blank? [x]
  (or (nil? x) (and (string? x) (str/blank? x))))

(defn canonical [url]
  (when-not (blank? url)
    [:link {:rel "canonical" :href url}]))

(defn alternate
  "One <link rel=alternate hreflang=...>."
  [hreflang url]
  (when-not (or (blank? hreflang) (blank? url))
    [:link {:rel "alternate" :hreflang hreflang :href url}]))

(defn alternates
  "Emit a sequence of alternate links from a coll of {:hreflang :url} maps."
  [pairs]
  (for [{:keys [hreflang url]} pairs]
    (alternate hreflang url)))

(defn icon [{:keys [href sizes type rel]}]
  (when-not (blank? href)
    [:link (cond-> {:rel (or rel "icon") :href href}
             sizes (assoc :sizes sizes)
             type  (assoc :type  type))]))

(defn apple-touch-icon
  [{:keys [href sizes] :or {sizes "180x180"}}]
  (when-not (blank? href)
    [:link {:rel "apple-touch-icon" :sizes sizes :href href}]))

(defn manifest [href]
  (when-not (blank? href)
    [:link {:rel "manifest" :href href}]))

(defn favicon-set
  "Emit the common apple-touch + 32x32 + 16x16 png icon set
  (+ optional manifest) from a base path like \"/imgs/favicon\"."
  [{:keys [base-path manifest?]}]
  (list
   (apple-touch-icon {:href (str base-path "/apple-touch-icon.png")})
   (icon {:href (str base-path "/favicon-32x32.png")
          :type "image/png" :sizes "32x32"})
   (icon {:href (str base-path "/favicon-16x16.png")
          :type "image/png" :sizes "16x16"})
   (when manifest?
     (manifest (str base-path "/site.webmanifest")))))

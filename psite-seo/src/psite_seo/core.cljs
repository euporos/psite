(ns psite-seo.core
  (:require [psite-routing.core :as routing]))

(defn- breadcrumb-item [req position name path]
  {"@type"    "ListItem"
   "position" position
   "name"     name
   "item"     (routing/make-path-absolute
               req (if (vector? path)
                     (apply (partial routing/reverse-match req) path)
                     path))})

(defn breadcrumb-list
  "Returns a hiccup node emitting a JSON-LD BreadcrumbList script tag.
   items is a flat sequence of alternating name/path pairs."
  [req items]
  [:script {:type "application/ld+json"
            :dangerouslySetInnerHTML
            {:__html (.stringify js/JSON
                                 (clj->js
                                  {"@context"        "https://schema.org"
                                   "@type"           "BreadcrumbList"
                                   "itemListElement" (map-indexed
                                                      (fn [i [n p]]
                                                        (breadcrumb-item req (inc i) n p))
                                                      (partition 2 items))}))}}])

(ns psite-hiccup.core
  (:require [cljs.pprint]
            [hiccups.runtime]
            [macchiato.util.response :as r]))

(defn obfuscate-email [env-path]
  (list [:span.psite-oml {:env-path (js/encodeURIComponent (str env-path))}]))

(defn obfuscate-phone [env-path]
  (list [:span.psite-oph {:env-path (js/encodeURIComponent (str env-path))}]))

(defn html->response
  ([html]
   (html->response 200 html))
  ([code html]
   (-> {:body html :status code}
       (r/content-type "text/html"))))

(defn pprint-to-string [x]
  (with-out-str (cljs.pprint/pprint x)))

(defn dangerous-html
  "Hiccup wrapper that injects HTMLSTRING via React's dangerouslySetInnerHTML."
  [htmlstring]
  [:div {:dangerouslySetInnerHTML {:__html htmlstring}}])

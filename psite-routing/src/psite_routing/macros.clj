(ns psite-routing.macros
  (:require [clojure.walk]))

(defn- remove-keys [data]
  (clojure.walk/postwalk
   (fn [x] (if (map? x) (dissoc x :handler :middleware) x))
   data))

(defmacro routes-reduced-for-matching
  "Strip :handler and :middleware from route data at macroexpand time so the
   same .cljc route file can be required by both the Node server build and the
   browser build without the browser needing to resolve handler namespaces."
  [route-data]
  (remove-keys route-data))

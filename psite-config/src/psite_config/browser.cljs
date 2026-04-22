(ns psite-config.browser
  (:require [clojure.edn :as edn]
            [clojure.pprint :as pprint]))

(def env (atom nil))

(defn ^:export load [edn]
  (reset! env (edn/read-string (js/decodeURIComponent edn)))
  (println "Loaded settings:")
  (pprint/pprint @env))

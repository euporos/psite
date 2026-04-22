(ns psite-config.server
  (:require [psite-config.read :as read]))

(defn select-paths [m pathseq]
  (reduce
   (fn [sofar path]
     (assoc-in sofar path (get-in m path)))
   {}
   pathseq))

(def env (read/read-env))

(defn set-browser-env-string [env]
  (let [share (or (get env :share-with-frontend)
                  (do (js/console.warn "psite-config: :share-with-frontend missing from config; no keys will be exposed to the browser")
                      []))
        paths (map #(if (vector? %) % (vector %)) (conj share :frontend))
        frontend-settings (select-paths env paths)]
    [:script (str "frontend_env_string='" (js/encodeURIComponent
                                           (str frontend-settings)) "';")]))
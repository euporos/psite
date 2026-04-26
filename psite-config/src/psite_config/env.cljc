(ns psite-config.env
  (:require #?(:node [psite-config.server :as server]
               :cljs [psite-config.browser :as browser])))

#?(:node
   (defn setting
     ([pointer]
      (setting server/env pointer))
     ([env pointer]
      (let [result (cond (keyword? pointer) (get env pointer ::missing)
                         (vector? pointer)  (get-in env pointer ::missing)
                         :else (throw (ex-info "config pointer must be keyword or vector" {:pointer pointer})))]
        (if (= result ::missing)
          (do (js/console.warn "Server: missing config key " (str pointer) pointer) nil)
          result))))
   :cljs
   (defn setting [pointer]
     (let [env @browser/env
           result (cond (keyword? pointer) (get env pointer ::missing)
                        (vector? pointer)  (get-in env pointer ::missing)
                        :else (throw (ex-info "config pointer must be keyword or vector" {:pointer pointer})))]
       (if (= result ::missing)
         (do (js/console.warn "Browser: missing config key " (str pointer) pointer) nil)
         result))))

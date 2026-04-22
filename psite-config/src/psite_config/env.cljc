(ns psite-config.env
  (:require #?(:node [psite-config.server :as server]
               :cljs [psite-config.browser :as browser])))

#?(:node
   (defn setting
     ([pointer]
      (setting server/env pointer))
     ([env pointer]
      (let [result (cond (keyword? pointer) (get env pointer)
                         (vector? pointer) (get-in env pointer)
                         :else (throw (new js/Error)))]
        (when (nil? result) (js/console.warn "Server: Got a nil config value for " (str pointer) pointer))
        result)))
   :cljs
   (defn setting [pointer]
     (let [result (cond (keyword? pointer) (get @browser/env pointer)
                        (vector? pointer) (get-in @browser/env pointer)
                        :else (throw (new js/Error)))]
       (when (nil? result) (js/console.warn "Browser: Got a nil config value for " (str pointer)  pointer))
       result)))

(ns psite-config.read
  (:require [cljs.reader :as edn]
            [clojure.string :as s]
            ["fs" :as fs]))

(defn obfuscate [text]
  (js/btoa (reduce str (reverse text))))

(defn- secret-tag [tag-name]
  {:psite/secret tag-name})

(def readers {'psite-config.read/obfuscate obfuscate
              'psite/secret              secret-tag})

(defn find-unresolved-secrets
  "Returns a vec of [path tag-name] for every #psite/secret sentinel
  that was not replaced by a later merge layer."
  [cfg]
  (letfn [(walk [path x]
            (cond
              (and (map? x) (contains? x :psite/secret))
              [[path (:psite/secret x)]]
              (map? x)
              (into [] (mapcat (fn [[k v]] (walk (conj path k) v)) x))
              :else []))]
    (walk [] cfg)))

(defn- parse-boolean [s]
  (boolean (some-> s (.trim) (.toLowerCase) (= "true"))))

(defn str->value [v]
  "ENV vars and system properties are strings. str->value will convert:
  the numbers to longs, the alphanumeric values to strings, and will use Clojure reader for the rest
  in case reader can't read OR it reads a symbol, the value will be returned as is (a string)"
  (cond
    (re-matches #"[0-9]+" v) (js/parseFloat v)
    (re-matches #"(?i)^(true|false)$" v) (parse-boolean v)
    (re-matches #"\w+" v) v
    :else
    (try
      (let [parsed (edn/read-string v)]
        (if (symbol? parsed)
          v
          parsed))
      (catch js/Error _
        v))))

(defn- k->path [k dash level]
  (as-> k $
    (s/lower-case $)
    (s/split $ level)
    (map (comp keyword
               #(s/replace % dash "-"))
         $)))

(defn env->path [k]
  (k->path k "_" #"__"))

(defn file-props [filename]
  (some->> filename
           (fs/readFileSync)
           (.toString)
           (edn/read-string {:readers readers})))

(defn env-props []
  (let [env (.-env js/process)]
    (reduce
     (fn [props k]
       (assoc-in props (env->path k) (str->value (aget env k))))
     {}
     (js/Object.keys env))))

(defn- deep-merge [a b]
  (merge-with (fn [x y]
                (cond (map? y) (deep-merge x y)
                      (vector? y) (concat x y)
                      :else y))
              a b))

(defn read-env
  "Reads config by deep-merging settings files and env vars.

   Options (in initial sofar map):
     :merge-config  string  — path to settings EDN file (default \"settings.edn\")
     :log-fn        fn      — called with the resolved config map for logging;
                              defaults to nil (no output)"
  ([]
   (read-env {:merge-config "settings.edn"} (env-props)))
  ([sofar env-props]
   (if (:merge-config sofar)
     (read-env (deep-merge
                (dissoc sofar :merge-config)
                (file-props (:merge-config sofar)))
               env-props)
     (let [relevant-keys (remove #{:log-fn} (keys sofar))
           log-fn        (:log-fn sofar)
           result        (if-let [additional-merge (:merge-config env-props)]
                           (read-env (assoc sofar :merge-config additional-merge)
                                     (dissoc env-props :merge-config))
                           (deep-merge sofar env-props))]
       (when log-fn (log-fn (select-keys result relevant-keys)))
       result))))

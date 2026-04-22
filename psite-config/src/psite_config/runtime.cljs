(ns psite-config.runtime
  "Runtime helpers for the shared lib-config contract.

   Each participating library exports a `config-schema` var (a Malli
   :map schema) listing its required keys. The application composes
   those schemas with `compose`, builds a matching value map, calls
   `validate!` once at boot, then attaches the result to every request
   under `:config`. Libraries read their keys via `cfg` or with
   `(get-in req [:config :lib/key])`."
  (:require [malli.core :as m]
            [malli.error :as me]))

(defn compose
  "Merges Malli :map schemas into a single :map schema. Later entries
   with the same key override earlier ones."
  [& schemas]
  (into [:map] (mapcat #(rest (m/form %)) schemas)))

(defn validate!
  "Throws ex-info with a humanized error if config does not match schema."
  [schema config]
  (when-let [explanation (m/explain schema config)]
    (throw (ex-info (str "Invalid lib-config: "
                         (pr-str (me/humanize explanation)))
                    {:errors explanation}))))

(defn cfg
  "Reads a namespaced lib-config key from a request."
  ([req k]           (get-in req [:config k]))
  ([req k not-found] (get-in req [:config k] not-found)))

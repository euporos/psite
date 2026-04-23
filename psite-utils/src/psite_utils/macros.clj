(ns psite-utils.macros
  (:require [clojure.string :as str]))

(defmacro cache-bust
  "Append a `?v<hash>` query string to PATH-WITHOUT-HASH, where the hash is
  computed at compile time from the file at `public<path>`. Used to bust
  browser caches when static assets change."
  [path-without-hash]
  {:pre [(string? path-without-hash)
         (str/starts-with? path-without-hash "/")]}
  (str path-without-hash "?v"
       (hash (slurp (str "public" path-without-hash)))))

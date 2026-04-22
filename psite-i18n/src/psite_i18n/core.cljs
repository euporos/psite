(ns psite-i18n.core
  (:require-macros [psite-i18n.core]))

(defn by-locale
  "Looks up locale in source (a map of locale-keyword -> value).
   Falls back through fallback order. Returns \"\" if no locale matches."
  [fallback locale source]
  (or (get source locale)
      (some #(get source %) fallback)
      ""))

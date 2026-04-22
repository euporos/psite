(ns psite-i18n.core)

(defmacro defsnip
  "Defines a function [locale] -> string using fallback-expr for locale resolution.
   snip is a pair: [fn-name {:de \"...\" :en \"...\" :uk \"...\"}]"
  [fallback-expr snip]
  `(defn ~(first snip) [locale#]
     (psite-i18n.core/by-locale ~fallback-expr locale# ~(second snip))))

(defmacro defsnips
  "Defines multiple snippet functions from a vector of [fn-name translation-map] pairs."
  [fallback-expr snipcol]
  `(do
     ~@(map (fn [s] `(psite-i18n.core/defsnip ~fallback-expr ~s)) snipcol)))

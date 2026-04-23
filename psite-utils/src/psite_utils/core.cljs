(ns psite-utils.core
  (:require [clojure.string :as str]))

(defn map-vals [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn csv
  "Join VALS with SEPARATOR. Empty/nil values are skipped."
  [separator & vals]
  (reduce #(str %1 separator %2)
          (remove empty? vals)))

(defn capitalize
  "Uppercase the first letter of each word."
  [string]
  (reduce str
          (map #(str/replace
                 % (re-pattern (str "^" (first %)))
                 (.toUpperCase (first %)))
               (str/split string #"\b"))))

(defn edn-to-json-string [edn]
  (.stringify js/JSON (clj->js edn)))

(defn google-maps
  "Build a Google Maps search URL from one or more search strings."
  [& searchstrings]
  (apply str
         "https://www.google.com/maps/place/"
         (interpose "+"
                    (reduce into
                            (map #(str/split % #" +") searchstrings)))))

(def umlaute-ascii
  {"ä" "ae"
   "ö" "oe"
   "ü" "ue"
   "ß" "ss"
   "é" "e"
   "à" "a"})

(def ukr-ascii
  [["а" "a"]  ["б" "b"]  ["в" "v"]   ["г" "h"]    ["ґ" "g"]
   ["д" "d"]  ["е" "e"]  ["є" "ye"]  ["ж" "zh"]   ["з" "z"]
   ["и" "y"]  ["і" "i"]  ["ї" "yi"]  ["й" "j"]    ["к" "k"]
   ["л" "l"]  ["м" "m"]  ["н" "n"]   ["о" "o"]    ["п" "p"]
   ["р" "r"]  ["с" "s"]  ["т" "t"]   ["у" "u"]    ["ф" "f"]
   ["х" "h"]  ["ц" "c"]  ["ч" "tsh"] ["ш" "sh"]   ["щ" "shch"]
   ["ю" "yu"] ["я" "ya"] ["ь" ""]    ["’" ""]])

(defn replace-by-table [string table]
  (reduce #(str/replace %1 (first %2) (second %2)) string table))

(defn replace-by-tables [string & tables]
  (reduce replace-by-table string tables))

(defn- strip-non-ascii [instring]
  (str/replace instring #"[^a-z0-9-_ ]" ""))

(defn make-string-url-compatible
  "Normalize STRING into an ASCII URL-safe token. Lowercases, transliterates
  German umlauts and Ukrainian Cyrillic, strips remaining non-ASCII, and
  replaces spaces with WHITESPACEREP (default \"-\")."
  ([instring]
   (make-string-url-compatible instring "-"))
  ([instring whitespacerep]
   (if (empty? instring)
     ""
     (-> (str/lower-case instring)
         (replace-by-tables umlaute-ascii ukr-ascii)
         strip-non-ascii
         (str/replace #" " whitespacerep)
         js/encodeURI))))

(def slugify #(make-string-url-compatible % "_"))

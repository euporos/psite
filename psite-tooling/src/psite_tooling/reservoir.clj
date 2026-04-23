(ns psite-tooling.reservoir
  (:require [clojure.java.io]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as str])
  (:import
   [java.io File]
   [java.nio.file Paths Files FileSystems LinkOption]))

(defn read-files-into-map
  [dir extension parser keywordize?]
  (let [rework-keys     #(if keywordize?
                           (keyword (str/replace % #"\.[^\.]+$" "")) %)
        grammar-matcher (.getPathMatcher
                         (java.nio.file.FileSystems/getDefault)
                         (str "glob:*.{" extension "}"))
        texts           (->> dir
                             clojure.java.io/file
                             file-seq
                             (filter #(.isFile %))
                             (filter #(.matches grammar-matcher (.getFileName (.toPath %))))
                             (mapv #(vector
                                     (-> % .getName rework-keys)
                                     (parser
                                      (slurp
                                       (.getAbsolutePath %)))))
                             (into {}))]
    texts))

(defn ^:export svgs-with-replacements [{:keys [source target replacements]}]
  (let [source-files (read-files-into-map source "svg" identity false)]
    (doseq [[path source-file] source-files]
      (println "processing: " (str target "/" path))
      (spit (str target "/" path)
            (reduce
             (fn [processed-file [search replace]]
               (str/replace processed-file search replace))
             source-file
             replacements)))))

(defn get-files-by-regex  [dir regex]
  (let [files           (->> dir
                             clojure.java.io/file
                             file-seq
                             (filter #(.isFile %))
                             (filter #(re-matches regex (str (.getFileName (.toPath %))))))]
    files))

(defn target-basename [source-file res qual]
  (let [[extension basename] (reverse (str/split (.getName source-file) #"\."))]
    (str basename "_" res "_" qual "." extension)))

(defn file-attribute
  "Return the value of the specified `attribute` of the file at `file-path`
   in the current default file system. The argument `attribute` may be passed
   as a keyword or a string, but must be an attribute name understood be 
   `java.nio.file.Files`."
  [file-path attribute]
  (Files/getAttribute
   (.getPath
    (FileSystems/getDefault)
    (str file-path)
    (into-array java.lang.String []))
   (name attribute)
   (into-array LinkOption [])))

(defn ^:export thumbnail-with-imagemagick [{:keys [only-newer?
                                                   source-dir
                                                   target-dir
                                                   res-quals
                                                   files-pattern]
                                            :or {only-newer? false
                                                 files-pattern #".+\.(JPG|jpg|psd)$"
                                                 res-quals [[1920 70]
                                                            [1024 70]
                                                            [768 70]
                                                            [512 70]]}}]
  (doseq [source-file (get-files-by-regex source-dir files-pattern)
          [res _qual] res-quals
          :let [target-file (clojure.java.io/file target-dir
                                                  (str res)
                                                  (.getName source-file))

                res-param (str res "x" res)
                update?
                (or
                 (not only-newer?)
                 (not (.exists target-file))
                 (< 0 (compare (file-attribute source-file "lastModifiedTime")
                               (file-attribute target-file "lastModifiedTime"))))]]
    ;TODO: copy original excluding non-jpgs
    (when update?
      (println (.getName source-file) " → " (.getName target-file))
      (.mkdirs (clojure.java.io/file (.getParent target-file)))
      (when-let [error (:err (sh "convert"
                                 (str source-file)
                                 "-resize" res-param
                                 ;; "-quality" qual
                                 (str target-file)))]
        (println error)))))

#_(thumbnail-with-imagemagick {:source-dir "/media/lapdaten/ARBEITSSD/dev/festival/festival-macchiato/reservoir"
                               :target-dir "/media/lapdaten/ARBEITSSD/dev/festival/festival-macchiato/public/compiled/from_reservoir"
                               :include-original? true
                               :res-quals [[1920 70]
                                           [1024 70]
                                           [768 70]
                                           [512 70]]})

(ns psite-menu.core)

(defn- expand-insertpoint
  [insertpoint insertables]
  (map
   #(if (map? %)
      (merge (:defaults insertpoint) %)
      (update-in % [1] (fn [headitem]
                         (merge (:defaults insertpoint) headitem))))
   ((:id insertpoint) insertables)))

(defn compose-menus
  "Merge dynamic insertable items into a static base menu.
   Base menus are nested vectors of maps where each map has a :type
   (:headitem, :menuitem, :insertpoint). :insertpoint entries are
   replaced by entries from `insertables` keyed by their :id, with
   :defaults from the insertpoint merged onto each inserted item."
  [basemenu insertables]
  (reduce
   (fn [sofar-in-subvec menupart]
     (cond
       (vector? menupart)
       (conj sofar-in-subvec (compose-menus menupart insertables))
       (= (:type menupart) :insertpoint)
       (into sofar-in-subvec
             (compose-menus
              (expand-insertpoint menupart insertables)
              insertables))
       :else (conj sofar-in-subvec menupart)))
   []
   basemenu))

(ns anaphor.render
  (:require [clojure.string :as string]))

(defn- map-class [x]
  (cond (coll? x) :coll
        (string? x) :string
        :else :default))

(defmulti render (fn [x _] (if (vector? x) (first x) (map-class x))))

(defmethod render :default [x opts] (render (rest x) opts))

(defmethod render :string [s _] s)

(defmethod render :coll [coll opts]
  (map #(render % opts) coll))

(defmethod render :conjp [x opts]
  (let [[_ {word :conj} parts] x
        c (count parts)
        r #(render % opts)]
    (cond (= c 1) (r (first parts))
          (= c 2) (str (r (first parts)) " " word " " (r (second parts)))
          (> c 2) (str (string/join ", " (r (butlast parts)))
                       " " word " "
                       (r (last parts))))))

(comment
 (defn render [clause]
   (clojure.string/join " " (filter string? (flatten clause)))))

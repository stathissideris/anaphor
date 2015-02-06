(ns anaphor.utils
  (:require [clojure.zip :as zip]))

(defn edit-tree
  [f tree]
  (loop [z (zip/vector-zip tree)]
    (if (zip/end? z)
      (zip/root z)
      (recur (zip/next (f z (zip/node z)))))))

(defn walk-vec
  "Like prewalk but only for seqs and uses zippers."
  [f s]
  (loop [z (zip/vector-zip s)]
    (if (zip/end? z)
      (zip/root z)
      (recur (zip/next (zip/replace z (f (zip/node z))))))))

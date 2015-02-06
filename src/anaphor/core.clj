(ns anaphor.core
  (:require [clojure.walk :as walk]
            [clojure.zip :as zip]
            [clojure.core.match :refer [match]]
            [anaphor.utils :as utils]
            [anaphor.render :refer [render]]))

;;conjunction phrases
(defn and-conj [node]
  (match node
   [:and parts] [:conjp {:conj "and"} parts]
   :else node))

(defn or-conj [node]
  (match node
   [:or parts] [:conjp {:conj "or"} parts]
   :else node))

;;pp prepositional phrases
(defn pp [node]
  (match node
   [:pp p [:np & rest]] [:pp' [:p p] (vec (concat [:np] rest))] ;;it's an np
   [:pp p np]           [:pp' [:p p] (vec (concat [:np] np))] ;;it's a list
   :else node))

;;add support for attribute map
(defn the [node]
  (match
   node
   [:the n]            [:np [:det "the"] [:n n]]
   [:the n [:pp' pp]]  [:np [:det "the"] [:nom [:n n] [:pp' pp]]]
   [:the [:adj adj] n] [:np [:det "the"] [:nom [:adj adj] [:n n]]]
   :else node))

(def rules [the pp and-conj or-conj])

(defn produce [tree]
  (walk/postwalk (apply comp rules) tree))

(defn is-class? [node c]
  (and (vector? node) (= c (first node))))

(defn remove-class [tree c]
  (utils/edit-tree
   (fn [tree node]
     (if (is-class? node c)
       (zip/remove tree)
       tree))
   tree))

(comment
  (produce [:pp "of"
            [[:n "President"]
             [:pp "of" [:the [:adj "United"] "States"]]]])
  (produce [:advp "abruptly" [:vp [:v "waking"]]])
  (produce [:or "this" "that"]))

(comment
  ;;sketch for noun
  [:n "cake"]
  [:n "cake" "cakes"]
  [:n {:singular "cake"
       :plural "cakes"
       :gender :neutral}])

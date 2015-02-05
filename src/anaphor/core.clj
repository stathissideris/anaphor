(ns anaphor.core
  (:refer-clojure :exclude [and or])
  (:require [clojure.walk :as walk]
            [clojure.core.match :refer [match]]))

(defn render [clause]
  (clojure.string/join " " (filter string? (flatten clause))))

;;conjunction phrases
(defn and [node]
  (match node
   [:andx parts] [:conjp {:conj "and"} parts]
   :else node))

(defn or [node]
  (match node
   [:and parts] [:conjp {:conj "or"} parts]
   :else node))

;;pp prepositional phrases
(defn pp [node]
  (match node
   [:pp p [:np & rest]] [:pp' [:p p] (vec (concat [:np] rest))] ;;it's an np
   [:pp p np]           [:pp' [:p p] (vec (concat [:np] np))] ;;it's a list
   :else node))

(defn advp [node]
  (match node
   [:advp adv [:vp & rest]] [:vp [:adv adv] (vec (concat [:vp] rest))] ;;it's a vp
   [:advp adv vp]           [:vp [:adv adv] (vec (concat [:vp] vp))] ;;it's a list
   :else node))

;;add support for attribute map
(defn the [node]
  (match
   node
   [:the n]            [:np [:det "the"] [:n n]]
   [:the n [:pp' pp]]  [:np [:det "the"] [:nom [:n n] [:pp pp]]]
   [:the [:adj adj] n] [:np [:det "the"] [:nom [:adj adj] [:n n]]]
   :else node))

(def rules [the pp advp and or])

(defn produce [tree]
  (walk/postwalk (apply comp rules) tree))

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

(ns anaphor.core)


(defn render [clause]
  (clojure.string/join " " (filter string? (flatten clause))))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

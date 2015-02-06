(ns dev
  (:require [clojure.tools.namespace.repl :as ns-tools]
            [clojure.pprint :refer [pprint]]))

(defn refresh []
  (ns-tools/refresh))

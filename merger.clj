(ns merger
  (:require
   [clojure.core.match :refer [match]]
   [maa :refer [<|]]
   [sexpr :as sx]
   [util :refer :all]))

(comment)

(defn from-sxs [sxs]
  ())

(defn top-extract [top]
  (match [top]
    [["defsrc" & rest]] {}))



(defn load []
  (<|
   let [kbd (sx/parse (slurp "kanata.kbd"))
        f (fn [acc top]
            (deep-merge-with eq-orUB acc
                             (top-extract top)))]
   (reduce f {} kbd)))
(ns util
  (:require
   [clojure.core.match :refer [match]]
   [maa :refer [<|]]))

(import '[java.awt.datatransfer StringSelection DataFlavor] '[java.awt Toolkit])
(defn pbcopy [^String text]
  (let [selection (StringSelection. text)
        clipboard (.getSystemClipboard (Toolkit/getDefaultToolkit))]
    (.setContents clipboard selection nil)))
(defn pbpaste []
  (-> (Toolkit/getDefaultToolkit)
      .getSystemClipboard
      (.getData DataFlavor/stringFlavor)))

(defn UB [& of]
  (throw (ex-info "UB" {:of of})))
(defn eq-orUB "except nil" [& elems]
  (<|
   (if (apply = (filter identity elems))
     (first (filter identity elems)))
   (apply UB "not-eq" elems)))

(defn deep-merge-with
  "Like merge-with, but merges maps recursively, applying the given fn
  only when there's a non-map at a particular level.
  (deep-merge-with + {:a {:b {:c 1 :d {:x 1 :y 2}} :e 3} :f 4}
                     {:a {:b {:c 2 :d {:z 9} :z 3} :e 100}})
  -> {:a {:b {:z 3, :c 3, :d {:z 9, :x 1, :y 2}}, :e 103}, :f 4}"
  [f & maps]
  (apply
   (fn m [& maps]
     (if (every? map? maps)
       (apply merge-with m maps)
       (apply f maps)))
   maps))
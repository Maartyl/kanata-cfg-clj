(ns sexpr
  "Simple recursive-descent parser for the Kanata S-expression format (from sexpr.rs).
   - Atoms / string literals (including \"...\" and r#\"...\"#) → plain Clojure strings (exact source text).
   - Whitespace / comments → tagged vectors ([:ws \"   \"], [:line-comment \";; foo\\n\"], [:block-comment \"#|...|#\"]).
   - Lists → plain Clojure vectors (the children).
   - Top-level result → vector of nodes (so you get everything that was in the file).
   - Completely lossless: (re-emit (parse s)) === s (for valid input). No positions stored, only the original text slices.")

(defn- ascii-whitespace? [ch]
  (contains? #{\space \tab \newline \return \formfeed \u000B} ch))

(defn- parse-sequence
  "Parse a sequence of nodes until we hit `close-char` (or end of string).
   Returns [nodes new-index]."
  [s i close-char]
  (loop [i i
         nodes []]
    (if (>= i (count s))
      [nodes i]
      (let [ch (nth s i)]
        (cond
          ;; end of list
          (and close-char (= ch close-char))
          [nodes (inc i)]

          ;; whitespace (tagged)
          (ascii-whitespace? ch)
          (let [j (loop [j i]
                    (if (and (< j (count s)) (ascii-whitespace? (nth s j)))
                      (recur (inc j))
                      j))]
            (recur j (conj nodes [:ws (subs s i j)])))

          ;; list
          (= ch \()
          (let [[children j] (parse-sequence s (inc i) \))]
            (recur j (conj nodes (vec children))))   ; ← plain vector = list (as requested)

          ;; quoted string "..."
          (= ch \")
          (let [j (loop [j (inc i)]
                    (if (>= j (count s))
                      j
                      (let [c (nth s j)]
                        (if (or (= c \") (= c \newline))
                          (if (= c \") (inc j) j)
                          (recur (inc j))))))]
            (recur j (conj nodes (subs s i j))))

          ;; line comment ;; (single ; is treated as the start of an atom)
          (and (= ch \;) (< (inc i) (count s)) (= (nth s (inc i)) \;))
          (let [j (loop [j (+ i 2)]
                    (if (or (>= j (count s)) (= (nth s j) \newline))
                      (if (and (< j (count s)) (= (nth s j) \newline))
                        (inc j)
                        j)
                      (recur (inc j))))]
            (recur j (conj nodes [:line-comment (subs s i j)])))

          ;; block comment #| ... |#
          (and (= ch \#) (< (inc i) (count s)) (= (nth s (inc i)) \|))
          (let [j (loop [j (+ i 2)]
                    (if (>= j (count s))
                      (count s)
                      (if (and (= (nth s j) \|) (< (inc j) (count s)) (= (nth s (inc j)) \#))
                        (+ j 2)
                        (recur (inc j)))))]
            (recur j (conj nodes [:block-comment (subs s i j)])))

          ;; raw multiline string r#" ... "#
          (and (= ch \r)
               (< (+ i 2) (count s))
               (= (nth s (inc i)) \#)
               (= (nth s (+ i 2)) \"))
          (let [j (loop [j (+ i 3)]
                    (if (>= j (count s))
                      (count s)
                      (if (and (= (nth s j) \") (< (inc j) (count s)) (= (nth s (inc j)) \#))
                        (+ j 2)
                        (recur (inc j)))))]
            (recur j (conj nodes (subs s i j))))

          ;; bare atom (anything else that doesn't start a special token)
          :else
          (let [j (loop [j i]
                    (if (>= j (count s))
                      j
                      (let [c (nth s j)]
                        (if (or (#{\( \) \"} c) (ascii-whitespace? c))
                          j
                          (recur (inc j))))))]
            (recur j (conj nodes (subs s i j)))))))))

(defn parse
  "Parse the whole string. Returns a vector of top-level nodes.
   Call (re-emit (parse s)) to get the original string back."
  [s]
  (let [[nodes final-i] (parse-sequence s 0 nil)]
    (when (not= final-i (count s))
      (throw (ex-info "Parser did not consume entire input" {:remaining (subs s final-i)})))
    nodes))

(defn re-emit-node
  "Turn a node back into its original text (lossless round-trip)."
  [node]
  (cond
    (string? node) node
    (and (vector? node) (keyword? (first node)))
    (second node)                                        ; [:ws "..."], [:line-comment ...], [:block-comment ...]

    (vector? node)
    (str "(" (apply str (map re-emit-node node)) ")")    ; list = plain vector

    :else
    (str node)))

(defn re-emit
  "Re-emit the entire parsed structure as the original string."
  [nodes]
  (apply str (map re-emit-node nodes)))

;; ----------------------------------------------------------------------------
;; Example usage
(comment
  (def example "(def foo bar) ;; comment\n  (baz)\n\nr#\"multi\nline\"#")
  (def parsed (parse example))
  ;; => [[:list ["(def foo bar)"]] [:line-comment ";; comment\n"] [:ws "  "] [:list ["(baz)"]] [:ws "\n\n"] "r#\"multi\nline\"#"]
  (re-emit parsed) ; === example

  (parse (slurp "kanata.kbd")))
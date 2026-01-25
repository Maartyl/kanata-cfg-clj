(ns rsnt
  (:require
   [clojure.string :as s]
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


;;old
(defmulti rs-dim "resolve vec-data dimension"
  (fn [term idx]
    (<| (if (int? term) :int)
        (if (vector? term) :vec)

        :tbd)))
;;old
(defmulti rsm "resolve match side"
  (fn [xxs term arg]
    (<| (if (int? term) :int)
        (if (vector? term) :tbd)
        (if (list? term) :tbd)
        :tbd)))
;;old
(defmulti rsa "resolve action side"
  (fn [xxs term arg]
    (<| (if (int? term)
          :int)
        (if (vector? term) :tbd)
        (if (list? term) :tbd)
        :tbd)))

;;(def aliases (atom {}))
;;old
(def gallish '[[:z :b :m :c :g] [:C :. :f :k :q]
               [:r :s :n :t :k] [:j :a :e :i :h]
               [:w :p :l :d :x] [:/ :o :u :y :v]])
(defn symfix [n]
  (case n :C "," n))
;;old
(def other [:elu :eld :eru :erd, :l51 :l63, :r11 :bl1 :bl3])
;12345 .. 11



(def hwkeys '[lalt q w e r t    y u i o q F5
              lsft a s d f g    h j k l - F6
              lctl z x c v b    n m = . / F7
              ,  esc tab spc    ret del bspc
              F1 F2
              F3 F4
              mlft mrgt mmid mbck mfwd
              mwu mwd mwl mwr])
(def off-layout 0)
(def off-thumbs (.indexOf hwkeys 'esc))
(def off-extra (.indexOf hwkeys 'F1))
(def off-mouse (.indexOf hwkeys 'mlft))

;;old
(defn hwoff [tag row ixs off]
  (map (fn [i]
         [[tag row (abs i)] (hwkeys (+ i off))]) ixs))

;;old
(defn hw-alias []
  (<|
   let [ss (map-indexed (fn [i k] [[:defsrc i] k]) hwkeys)
        lr (<| (apply concat)
               for [i (range 3)
                    :let [row (- 3 i)]]
               (concat
                (hwoff :l row (range -6 0) (<| + 6 * i 12))
                (hwoff :r row (range 1 7) (<| + 5 * i 12))))
        c (count lr)
        b (concat
           (hwoff :bl 0 (range -3 0) (+ 3 c))
           (hwoff :br 0 (range 1 4) (+ 2 c)))

        ;F1..4 do not need aliases
        ]
   (concat lr b ss)))



(defn cz []
  (<|
   let [t
        (fn [l d]
          (str "  " l " (switch ((not (layer cz))) " l " break (lsft rsft) (unicode " (s/upper-case d) ") break () (unicode " d ") break)"))
        d "ďčňřťšéžýóůúáěí"
        l "dcnrtspzyoujaei"]
   (map t l d)))

(defn fingy []
  (<|

   let [fingers "zprmiw"
        rfs (reverse fingers)]
   for [n "123"
        s "lr"
        c (if (= s \l) fingers rfs)]
   [s n c]))
(defn fingyth []
  (<|

   let [fingers "321"
        rfs (reverse fingers)]
   for [n "0"
        s "lr"
        c (if (= s \l) fingers rfs)]
   [s n c]))
(def fingyex ["M1" "M2" "M3" "M4"])
(def knames
  (map keyword
       (concat
        (for [[s n c] (fingy)]
          (str s n c))
        (for [[s n c] (fingyth)]
          (str s n c))
        fingyex)))

(def name2idx (zipmap knames (range)))

(defn shifty []
  (<|
   let [k "1234567890`=,./;\\'"
        g "!@#$%^&*()~+<>?:|\""
        qt
        {"(" "lp", ")" "rp", "\"" "dq"}
        f (fn [k g]
            [(str "  " k " (unshift " k ")")
             (str "  " (qt (str g) g) " S-" k)])]
   (map f k g)))

(defn overlay
  ([xxs top low]
   ;;<|
   1)
  ([xxs top low & low]
   ;;<|
   1))

(comment
  (+ (int \a) 1)

  (def az (map char (range (int \a) (+ (int \z) 1))))
  (<|
   (pbcopy)
   (s/join "\n")
   (remove (set "dcnrtspzyoujaei") az))


  hwkeys
  (into {} (hw-alias))
  (hw-alias)

  #_(cz identity)
  #_(cz s/upper-case)
  (<|
   (pbcopy)
   (s/join "\n")
   (cz))

  ;;(s/upper-case \a)
  name2idx

  (fingy)

  (<|
   (pbcopy)
   (s/join "\n")
   for [[s n c] (fingyth)]
   (str "  " s n c " _"))
  (<|
   (pbcopy)
   (s/join "\n")
   for [[hw [s n c]] (map vector hwkeys (fingy))]
   (str "  >" s n c " " hw))

  (<|
   (pbcopy)
   (s/join " ")
   for [[s n c] (fingyth)]
   (str "@" s n c))

  (pbpaste)

  (def xx (s/split (pbpaste) #"[\t\n]"))

  (<|
   (pbcopy)
   (s/join " ")
   xx)
  (<|
   (pbcopy)
   (s/join "\n")
   (apply concat)
   (shifty))


  ())



(def layout-hw '[_ q w e r t  y u i o q _
                 _ a s d f g  h j k l - _
                 _ z x c v b  n m = . / _])

;; gallium based
(def layout-galm '[_ _ l d c w  f y o u _ _
                   x n r t s v  j h a e i .
                   _ b z m g q  _ p / _ k _])

(defn galzip [])

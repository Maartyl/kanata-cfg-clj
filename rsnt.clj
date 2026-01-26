(ns rsnt
  (:require
   [clojure.string :as s]
   [clojure.walk :as w]
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

(def hw-lpass (into [] (repeat (count hwkeys) nil)))
(def l-floor (into [] (repeat (count hwkeys) 'XX)))

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


(def layout-hw   '[_ q w e r t  y u i o p _
                   _ a s d f g  h j k l _ _
                   _ z x c v b  n m _ . _ _])

;; gallium based
(def layout-galm '[_ _ l d c w  f y o u _ _
                   x n r t s v  j h a e i .
                   _ b z m g q  _ p / _ k _])


(defn cz
  "fmt [l d u]"
  ([]
   (cz
    (fn [l d u]
      (str "  " l " (switch ((not (layer cz))) " l " break (lsft rsft) (unicode " u ") break () (unicode " d ") break)"))))
  ([fmt]
   (let [d "ďčňřťšéžýóůúáěí"
         l "dcnrtspzyoujaei"]
     (map fmt l d (s/upper-case d)))))

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
   let [k "[]1234567890-`=,./;\\'"
        g "{}!@#$%^&*()_~+<>?:|\""
        qt
        {"(" "lp", ")" "rp", "\"" "dq"}
        f (fn [k g]
            [(str "  " k " (unshift " k ")")
             (str "  " (qt (str g) g) " S-" k)])]
   (map f k g)))

(defn- kpass? [term]
  (case term (nil "" "_" _ :_) ::pass))

(defmulti rsa "resolve action side"
  #_{:clj-kondo/ignore [:unused-binding]}
  (fn [xxs term arg idx ts ls]
    (<|
     or (kpass? term)
     (if (int? term) ::int)
     (if (fn? term) ::fn)
     ;; (if (string? term) :str)
     ;; (if (vector? term) :v)
     ;; (if (list? term) :tbd)
     ::tbd)))

;;TOUP:  resolve t
#_{:clj-kondo/ignore [:unused-binding]}
(defmethod rsa ::tbd
  [xxs term arg idx ts ls] term)

(defmethod rsa ::fn
  [xxs term arg idx ts ls]
  (term arg {::idx idx
             ::around ts
             ::down ls}))
#_{:clj-kondo/ignore [:unused-binding]}
(defmethod rsa ::pass
  [xxs term arg idx ts ls] arg)

(defn rsl [l]
  ;;TOUP: ensure in defsrc vec form

  (<|
   (if (vector? l)
     (into l (-> (- (count hw-lpass)
                    (count l))
                 (repeat nil)
                 seq)))

   (throw (ex-info "rsl not v" {::l l}))))

(defn overlay
  ([xxs top low]
   (mapv
    (fn [t l i] (rsa xxs t l i top low))
    (rsl top)
    (rsl low)
    (range)))
  ([xxs top low & lower]
   (overlay xxs top
            (apply overlay xxs low lower))))

(defn +at [x]
  (<|
   let []
   (case x
     "_" x
     ":" "@:")
   (if (not (next x)) (str "@" x))
   (if (s/starts-with? x ":") (s/replace-first x ":" ""))
   (str "@" x)))

(defn mmod [held & {::keys []  :as opts}]
  (<|
   let [th 'tap-hold-release-keys
        ;; figure out good ...
        z (list th 0 200 :atap
                (list th 0 200 :alater :ahold))]

   (fn [covered {::keys [idx around]  :as oxx}]
     ;; still might mean fall-thru layer
     (kpass? covered)
     (list 'tap-hold-release-keys 0 888 covered held
           (list
            (around (+ idx 1))
            (around (- idx 1)))))))

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
   (map
    (fn [l [s n c]]
      (str "  " s n c " @" l))
    layout-galm (fingy)))
  layout-hw
  (<|
   (pbcopy)
   (s/join "\n")
   (map
    (fn [g h]
      (str "  $g" g " " h))
    layout-galm layout-hw))


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
   let []
   (s/join " ")
   (map +at)
   (filter seq xx))

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


(defn modsft [a o]
  (list 'tap-hold-release-keys 0 200 "@r" 'sft '(n t s)))

(def layout-hrm '[_ _ _ _ _ _  _ _ _ _ _ _
                  _ _ _ _ _ _  _ _ _ _ _ _
                  _ _ _ _ _ _  _ _ _ _ _ _])

(defn dw [tr]
  (<|
   (if (string? tr) tr)
   (if (coll? tr)
     (str "(" (s/join " " (map dw tr)) ")"))
   (if (keyword? tr) (str "@" (name tr)))
   (str tr)))


(comment
  (dw '(cdl
        "cdl"
        [cdl :cdl]))


  ())

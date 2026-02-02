
(ns kan
  (:require
   [clojure.string :as s]
   [clojure.walk :as w]
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

(def ^:private _ '_)

(defn chrange [a z]
  (map char (range (int a) (+ (int z) 1))))

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

(defn lay  [off & actions]
  (into (subvec hw-lpass 0 off) actions))

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
                   x n r t s z  j h a e i .
                   _ b v m g q  _ p / _ k _])

(def layout-gaxt '[A B _ _ _ _  _ _ _ _ C D
                   _ _ _ _ _ _  _ _ _ _ _ _
                   E _ _ _ _ F  G _ _ S _ H])
;; S - semi


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

(defn shifty-tr []
  (<|
   let [k "[]1234567890-`=,./;\\'"
        g "{}!@#$%^&*()_~+<>?:|\""
        ;; qt
        ;; {"(" "lp", ")" "rp", "\"" "dq"}
        az (chrange \a \z)
        Az (chrange \A \Z)

        f (fn [k g]
            {::vars {(str "-" k) (str "(macro (unshift " k ") 5)")}
             k (str "$-" k)
             g (str "S-" k)})]
   (apply
    deep-merge-with UB
    (map f
         (concat k az)
         (concat g Az)))))

(def char2act (assoc (shifty-tr)
                     \space "spc"
                     'S ";"
                     'semi ";"))

(defn- kpass? [term]
  (case term (nil "" "_" _ #_:_ ::pass) ::pass nil))
(defn- knoop? [term]
  (case term ("XX" XX ::noop) ::noop nil))

(defmulti rsa "resolve action side"
  #_{:clj-kondo/ignore [:unused-binding]}
  (fn [xxs term arg idx ts ls]
    (<|
     or (kpass? term)
     ;; (if (int? term) ::int)
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
   (if (seq? l)
     (rsl (into [] (take (count hw-lpass)) l)))

   (throw (ex-info "rsl not vq" {::l l}))))

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

   (fn [covered {::keys [idx down]  :as oxx}]
     ;; still might mean fall-thru layer
     (kpass? covered)
     (list 'tap-hold-release-keys 0 888 covered held
           ;; nonsense - not just letters
           (list
            (down (+ idx 1))
            (down (- idx 1)))))))
(defn hr [held]
  (<|
   let [th 'tap-hold-release-keys
        ;; figure out good ...
        z (list th 0 200 :atap
                (list th 0 200 :alater :ahold))]

   (fn [covered {:as _xc}]
     ;; still might mean fall-thru layer
     ;; (kpass? covered)

     (list 't! "hr" covered held))))


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

(def layout-hrm
  (<|
   let [s (hr "sft")
        c (hr "ctl")
        a (hr "alt")
        l (hr "(layer-while-held f00)")
        r (hr "(layer-while-held rpi)")
        n (hr "(layer-while-held nums)")]


   [_ _ _ _ _ _  _ _ _ _ _ _
    l _ s c a _  _ a c s _ r
    _ _ _ _ _ _  _ _ _ _ n _]))

(defn map-render [tr]
  "-tbd-")
(defn render [tr]
  (<|
   (if (string? tr) tr)
   (if (map? tr) ((::render tr map-render) tr))
   (if (coll? tr)
     (str "(" (s/join " " (map render tr)) ")"))
   (if (keyword? tr) (str "@" (name tr)))
   (str tr)))

(defn laymap-prefix [pre l]
  (mapv #(if (kpass? %1)
           '_
           (concat pre [%1]))
        l))

(comment
  (render '(cdl
            "cdl"
            [cdl :cdl]))
  (keyword 'rts)

  (<|
   (pbcopy)
   (render)
   (list* "deflayer gr1")
   ;;(s/join "\n")
   (overlay
    {}

    (laymap-prefix (list 'chord "gr1") (lay off-thumbs 'l3 'l2 'l1))
    (laymap-prefix (list 'chord "gr1") layout-galm)

    l-floor))


  (<|
   (pbcopy)
   (render)
   (filter (comp not kpass?) layout-galm))

  ;;  #_(map #(fn [n _] (if (= %1 '_)
  ;;                     n
  ;;                     (list 'chord "gr1" %1)))
  ;;        layout-galm)

  ())


(defn char-out [a]
  (match [a]
    [(x :guard string?)]
    (match [(map char-out x)]
      [([solo] :seq)] solo
      [xs] (list* 'macro xs))
    [(_ :guard (comp not char?))]
    (UB a)

    [c]
    (or (char2act c) (list 'unicode (str c)))

    [x] (UB x)))

;; acc = top-ish kbd terms
;;; {::layer {name v}} etc.
(defn tokbd [action]
  (match [action]
    [nil] '_
    [(x :guard string?)] x
    ;; todo: translate specials
    [(x :guard symbol?)] (name x)
    [(x :guard keyword?)] (str "@" (name x))

    [{::txt txt}] (char-out txt)

    [(x :guard vector?)] (mapv tokbd x)

    ;; tbd vec
    ;; 
    [([(f :guard symbol?) & args] :seq)] (list* (tokbd f) (map tokbd args))
    [([(f :guard string?) & args] :seq)] (list* (tokbd f) (map tokbd args))

    ;; (::foo ...)



    #_[]))

(defn template [{::keys [vars]}]

  (apply
   str
   (concat
    ["(defvars"]
    (map (fn [[n a]] (str "\n  " n " " a)) vars)

    ["\n)"])))

(def lsqs-rsym [_ _ _ _ _ _  _ _ _ _ _ _
                _ _ _ _ _ _  _ _ _ _ _ _
                _ _ _ _ _ _  _ _ _ _ _ _])


(def g2hw (into
           {} (map vector
                   (overlay
                    {}
                    (lay off-thumbs 'l3 'l2 'l1 'r1 'r2 'r3)
                    layout-gaxt
                    layout-galm)
                   hwkeys)))



(defn spread-prefix [pv cs]
  (<|
   let [ps (take-while #(not (vector? %1)) cs)
        cc (drop-while #(not (vector? %1)) cs)
        p (into pv ps)]
   (if (empty? cc) [p])
   (apply concat
          (map #(spread-prefix p %1) cc))))

(defn chord2 [v]
  (<|
   let [chord (seq (map g2hw (drop-last 1 v)))
        action (last v)]
   (if (not chord) [::no-chord v])


   [::chord2 chord (tokbd action) 'first-release ()]))





;; (but-last) last 36 first-release ()
(def gchords
  ;; shared prefixes
  '[#_[l1
       [r1 {::seq (multi "," spc)}]]
    [r1
     [l1 (multi "," spc)]
     #_[h "bks"]]

    [l1
     [r "@lp"]
     [r t "@rp"]]
    [l2
     [t f]
     [n v]]

    [r1]
    [r2
     [u w]]

    [S  ;; ; semi
     [/ ","]
     [p "f"]]

    [amps [x x]]
    [slap [x x]]

    [t r e {::txt "true"}]
    [l2 t s e {::txt "false"}]

    []])

(comment
  (render (tokbd {::txt "true"}))

  (template (shifty-tr))


  (->> gchords
       (spread-prefix [])
       (map chord2))
  ;; (apply concat)
  ;; (apply str)
  ;; (pbcopy)

  g2hw

  (<|
   (pbcopy)
   ;;(render)
   ;;(list* "deflayer gr1")
   ;;(s/join "\n")
   (render)
   ;;(tokbd)
   (overlay
    {}

    layout-hrm
    (map #(or (kpass? %1)  (keyword %1)) layout-galm)
    (map keyword layout-gaxt)
    (lay off-thumbs  :l03 :l02 :l01 :r01 :r02 :r03)
    (lay off-extra :M1 :M2 :M3 :M4)
    (lay off-mouse "(tap-hold 12 12 mlft mlft)")


    hwkeys))


  [])

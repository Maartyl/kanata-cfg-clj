
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

(def ^:private _ '_)
(def ^:private __ '_)
(def ^:private XX 'XX)
(def ^:private xx 'XX)

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
(def lff-thumbs (into [] (repeat off-thumbs nil)))
(def lff-extra (into [] (repeat off-extra nil)))
(def lff-mouse (into [] (repeat off-mouse nil)))

(def l-pass (into [] (repeat (count hwkeys) nil)))
(def l-floor (into [] (repeat (count hwkeys) 'XX)))

(defn lay  [off & actions]
  (into (subvec l-pass 0 off) actions))

;;old
(defn hwoff [tag row ixs off]
  (map (fn [i]
         [[tag row (abs i)] (hwkeys (+ i off))]) ixs))

;; left hand side 
(defn llhs  [& actions]
  (<|
   let [st (fn [s t] (->> actions
                          (drop s)
                          (take t)))
        a (st 0 6)
        b (st 6 6)
        c (st 12 6)
        d (st 18 3)
        n6 (repeat 6 nil)]
   (concat
    a n6 b n6 c n6 d)))
(defn lrhs  [& actions]
  (<|
   let [st (fn [s t] (->> actions
                          (drop s)
                          (take t)))
        a (st 0 6)
        b (st 6 6)
        c (st 12 6)
        d (st 18 3)
        n6 (repeat 6 nil)]
   (concat
    n6 a n6 b n6 c nil nil nil d)))
(defn lay19 [k1 k2 k3 k4 k5 k6 k7 k8 k9]
  (<|
   [_ _ _ _ _ _  _ k7 k8 k9 _ _
    _ _ _ _ _ _  _ k4 k5 k6 _ _
    _ _ _ _ _ _  _ k1 k2 k3 _ _]))

(defn layraw [s] (filter not-empty (s/split s #"\s+")))

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
           (hwoff :br 0 (range 1 4) (+ 2 c)))]

        ;F1..4 do not need aliases

   (concat lr b ss)))


(def layout-hw   '[_ q w e r t  y u i o p _
                   _ a s d f g  h j k l _ _
                   _ z x c v b  n m _ . _ _])

;; gallium v2 raw
(def layout-gallium
  '[_ b l d c v  j y o u _ _
    _ n r t s g  p h a e i _
    _ x q m w z  k f / _ . _])

;; gallium based
(def layout-galm '[_ _ l d c w  f y o u _ _
                   x n r t s z  j h a e i .
                   _ b v m g q  _ p / _ k _])

(def layout-gaxt '[A B _ _ _ M  N _ _ _ C D
                   X _ _ _ _ U  V _ _ _ _ Y
                   E _ _ _ _ F  G _ R S _ H])
;; S - semi


;; caster based
#_(def layout-cstrm
    '[_ _ w d f j   b l o u _ _
      x c s t r z   _ n a i h _
      _ y g v m q   _ p _ _ k _
      _ _ _         e _ _])
(def layout-cstrm
  '[_ _ w d l j   b f o u _ _
    x c s t r z   _ n a i h _
    _ y g v m q   _ p _ _ k _
    _ _ _         e _ _])
;; caster based
;; (def layout-cstrm
;;   '[_ q w d l j   b f o u _ _
;;     x c s t r z   _ n a i h _
;;     _ y g v p _   _ m _ _ k _
;;     _ _ _         e _ _])

(def leffort [9,9,2,1,2,4,4,2,1,2,9,9,
              3,1,0,0,0,5,5,0,0,0,1,3,
              7,3,4,2,1,8,8,1,2,4,3,7])

;; v2
(def key-names
  "l/r{} + thumbs: 123"
  ["4 a b c d 7"
   "5 e f g h 8"
   "6 i j k l 9"])
(def lay-knames
  (<|
   let [[a e i] (map #(s/split %1 #" ") key-names)
        t (s/split "1 2 3" #" ")
        l #(for [k (reverse %1)] (str "l" k))
        r #(for [k %1] (str "r" k))]
   (into [] cat)
   (for [s [a e i t]
         f [l r]]
     (f s))))


(defn cz
  "fmt [l d u]"
  ([]
   (cz
    (fn [l d u]
      (str "  " l " (switch ((not (layer cz))) " l " break (lsft rsft) (unicode " u ") break () (unicode " d ") break)"))))
  ([fmt]
   (let [d "ďčňřťšéžýóůúáěíě"
         l "dcnrtspzyoujaeih"]
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

(defn shifty
  ([]
   (shifty (fn [k g]
             [(str "  " k " (unshift " k ")")
              (str "  " g " S-" k)])))
  ([f]
   (<|
    let [k (map str "[]1234567890-`=,./;\\'")
         g (map str "{}!@#$%^&*()_~+<>?:|\"")
         qt
         {"(" "lp", ")" "rp", "\"" "dq"}
         g (map #(qt %1 %1) g)]
    (map f k g))))

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
                     'S "@;"
                     'semi "@;"))

(defn- kpass? [term]
  (case term (nil "" "_" _ #_:_ ::pass) ::pass nil))
(defn- knoop? [term]
  (case term ("XX" XX ::noop) ::noop nil))

(defn lmap [f lxs]
  (map #(if (kpass? %1) %1 (f %1)) lxs))

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
     (into l (-> (- (count l-pass)
                    (count l))
                 (repeat nil)
                 seq)))

   (if (nil? l) l-pass)
   (if (seq? l)
     (rsl (into [] (take (count l-pass)) l)))

   (if (and (map? l) (::def-layer l))
     (rsl @(::def-layer l)))

   (throw (ex-info "rsl not vq" {::l l}))))

(defn overlay
  ([xxs top] (rsl top))
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
(defn templ [name & args]
  (fn [covered _]
    (list* 't! name covered args)))

(defn hr [held] (templ "hr" held))
(defn hrl [held] (templ "hrl" held))

(def lthc (into lff-thumbs  '[esc tab spc , spc - ret]))
(def lthm
  (into lff-thumbs
        [(templ "l3")
         (templ "l2")
         (templ "l1")
         (templ "r1")
         (templ "r2")
         (templ "r3")]))


(comment
  (+ (int \a) 1)

  (def az (map char (range (int \a) (+ (int \z) 1))))
  (<|
   (pbcopy)
   (s/join "\n")
   (remove (set "dcnrtspzyoujaeih") az))


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


(def layout-hrm
  (<|
   let [g (hr "lmet")
        s (hr "sft")
        c (hr "ctl")
        š (hr "rsft")
        č (hr "rctl")
        a (hr "alt")
        l (hrl "f00")
        r (hrl "rh")
        x (hrl "rpi")
        h (hrl "lh")
        y (hrl "lnav2")
        p (hrl "paredit-move")
        q (hrl "paredit-act")
        n (hrl "nums")]

   [_ _ _ p q _  _ _ _ _ _ _
    a x s _ _ _  _ _ _ š n r
    _ a s c g _  _ g č š _ _]))

(defn map-render [tr]
  "-tbd-map-render-")
(defn render "to str" [tr]
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



;; seq abandonned; unpleasant; => one shot layers


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

  (->>
   (::vars char2act)
   #_(apply concat)
   (map (fn [[n v]] (str "  " n " " v)))
   (s/join "\n")
   pbcopy)

  (->> gchords
       (spread-prefix [])
       (map chord2))
  ;; (apply concat)
  ;; (apply str)
  ;; (pbcopy)

  g2hw
  (-> lay-knames
      render
      pbcopy)
  (-> (for [k lay-knames]
        (list 'chord "schorts" k))
      render
      pbcopy)




  [])

(defn old-main []
  (<|
   (pbcopy)
   ;;(render)
   ;;(list* "deflayer gr1")
   ;;(s/join "\n")
   (#(s/replace %1 #"^[(]|[)]$" ""))
   (render)
   ;;(tokbd)
   (overlay
    {}


    layout-hrm
    lthm
    #_(map #(or (kpass? %1)  (keyword %1)) layout-galm)
    (map #(or (kpass? %1)  (keyword %1)) layout-cstrm)

    (map keyword layout-gaxt)
    lthc
    #_(lay off-thumbs  :l03 :l02 :l01 :r01 :r02 :r03)
    (lay off-extra :M1 :M2 :M3 :M4)
    #_(lay off-mouse "(tap-hold 22 22 mlft mlft)")


    hwkeys)))


;;; -----------------------

;; key-def as update (state) and action
(defmulti as-ua
  (fn [d]
    (<|
     (if (vector? d) (UB "as-ua tbd" d))
     (if (string? d) ::idty)
     (if (symbol? d) ::idty)
     (if (keyword? d) ::idty)
     (if (int? d) ::idty)
     (if (nil? d) ::idty)
     (if (not (map? d)) (UB "as-ua" d (type d)))
     (::op d))))
(defmethod as-ua ::idty [d] {::action d})

(defn uavec "[u+a] -> u+[a]"
  [uas]
  (reduce
   (fn [{acc ::action ucc ::update}
        {a ::action u ::update}]
     {::action (conj acc a)
      ::update (deep-merge-with eq-orUB ucc u)})
   {::action []
    ::update {}}
   uas))

;; (def alias-registry (atom {::inserts []
;;                            ::lookup {}}))

;; - cyclic might inf-recur , simple delay needed too precise placement, unsafe
;; -- this is invoked once (per top), ensured by me.
(defn deflayer-reducer [name layer-ref]
  ;; ensure skipping recompute
  (fn [defs]
    (let [{::keys [action update]}
          (uavec (map as-ua @layer-ref))]
      (deep-merge-with
       eq-orUB
       defs
       update
       {::layers {name action}}))))

(defmacro deflayer [name & overs]
  `(def ~name
     {::op ::layer
      ::name '~name
      ::def-layer (delay (overlay {} ~@overs))}))
(defmethod as-ua ::layer [{::keys [name def-layer]}]
  ;; later. : upd from actions here, or later?
  ;; - cyclic might inf-recur despite defer

  ;; kept as non-raw-kan action on purpose - usage reinterprets
  {::action {::op ::layer
             ::name name}
   ::update {::deferred-updates {[name def-layer] deflayer-reducer}}})

(defn process-deferred-updates [defs]
  (if (not (::deferred-updates defs))
    defs
    (recur (reduce-kv
            (fn [acc k v] ((apply v k) acc))
            (dissoc defs ::deferred-updates)
            (::deferred-updates defs)))))


(defn thr
  ([tap hold]
   {::op ::tap-hold ::ctor "tap-hold-release"
    ::tap tap ::hold hold})
  ;; TODO: better way to have 2 args
  ([hold] #(thr (and %2 %1) hold)))
(defn thp
  ([tap hold]
   {::op ::tap-hold ::ctor "tap-hold-press"
    ::tap tap ::hold hold})
  ([hold] #(thp %1 hold)))

(defn osh "one-shot hold"
  ([hold]
   {::op ::one-shot ::ctor "one-shot"
    ;; ms always 500 for now
    ::hold hold})
  ([] #(osh %1)))
(def osA (osh "lalt"))
(def osS (osh "lsft"))
(def osC (osh "lctl"))
(def osM (osh "lmet"))

(defn kan [ctor & args]
  ;; generic kanata call = list (ctor ...)
  {::op ::kan
   ::ctor ctor ::args args})
(defmethod as-ua ::kan [{ctor ::ctor args ::args}]
  (update (uavec (map as-ua (cons ctor args)))
          ::action list*))
(defmethod as-ua ::tap-hold [{::keys [ctor tap hold]}]
  (update (uavec (map as-ua [ctor 0 588 tap hold]))
          ::action list*))
(defmethod as-ua ::one-shot [{::keys [ctor hold]}]
  (update (uavec (map as-ua ['one-shot 900 hold]))
          ::action list*))

(deflayer lbase-old

  layout-hrm
  lthm
  #_(map #(or (kpass? %1)  (keyword %1)) layout-galm)
  (map #(or (kpass? %1)  (keyword %1)) layout-cstrm)

  (map keyword layout-gaxt)
  lthc
  #_(lay off-thumbs  :l03 :l02 :l01 :r01 :r02 :r03)
  (lay off-extra
       "mute"
       "prtsc"
       "(layer-while-held raws)"
       "(t! thr lrld (layer-while-held hw-info))")
  #_(lay off-mouse "(tap-hold 22 22 mlft mlft)")


  hwkeys)

(deflayer l-paredit-move
  (lmap
   #(kan 'macro "C-A-p" 2 %1)
   (lrhs
    'w 'd 't 'b  _  _
    'h 'l 'o 'r 'x 'e
    'x 'q 'v 'p 'x  _)))
(deflayer l-paredit-act
  (lmap
   #(kan 'macro "C-A-p" 2 %1)
   (lrhs
    'x 'a 't 'z  _  _
    'x 'm 'n 'f 'g 'x
    'x 'j 'c 'i 'y  _)))

(deflayer l-cuts
  (llhs
   xx   xx   osC   XX osM  xx
   'C-z 'C-a 'C-c  xx 'C-v xx
   xx   osA  'C-x  xx xx   xx))

(deflayer l-cuts+paredit-move
  l-cuts
  l-paredit-move)

(comment
  (-> l-cuts+paredit-move
      ::def-layer
      force)
  (-> lbase-old
      ::def-layer
      force)

  (lrhs
   _ _ _ _ _ _
   _ _ _ _ _ _
   _ _ _ _ _ _)

  [])

(deflayer l-arrows-core
  (->>
   "
   _ _ | _ _ _
   _ < - > _ _
   _ _ = _ _ _
    "
   layraw
   (apply lrhs)))

(deflayer l-symbols1
  (->>
   "
  _    _   \\  @  /  _      :   {  |  }   %   _
  _    `    ^  *  +  $      !   <  -  >   ?   ;
  _    %    ~  #  &  _      _   '  =  :   !   _
       _     `     _          _     @_    _
      "
   layraw))

(deflayer l-f10
  (apply lay19 (map #(str 'f %1) (range 11 20)))
  (lrhs
   "f24" _ _ _ "f23" _
   "f22" _ _ _ "f20" "f21"
   _     _ _ _ _ _))
(deflayer l-f00
  (apply lay19 (map #(str 'f %1) (range 1 10)))
  (lrhs
   _     _ _ _ _ _
   "f12" _ _ _ (thr "f10" l-f10) "f11"
   _     _ _ _ _ _))

(deflayer l-fast
  l-f00
  (llhs
   _    _ 'M-1 'M-2 _ _
   'M-s _ 'C-s _    _ _
   _    _ _    _    _ _))

(defn lb-thumbs []
  (letfn [(l3 [atap _i] (thr atap l-symbols1))
          (l2 [atap _i] (thr atap l-fast))
          (l1 [atap _i] (thr atap 'tbd))
          (r1 [atap _i] (thr atap 'tbd))
          (r2 [atap _i] (thr atap 'tbd))
          (r3 [atap _i] (thr atap 'tbd))]
    [l3 l2 l1 r1 r2 r3]))


(def l-secondary
  (let [g (thr "lmet")
        s (thr "sft")
        c (thr "ctl")
        š (thr "rsft")
        č (thr "rctl")
        a (thr "alt")
        z (thr "ralt")
        ;; FIXME: direct layers, not t!
        ;;  - f00 is now just rhs
        l (identity "f00")
        r (identity "rh")
        x (identity "rpi")
        h (identity "lh")
        y (identity "lnav2")
        p (thr l-cuts+paredit-move)
        q (identity "paredit-act")
        n (identity "nums")]

    [_ _ _ p q _  _ _ _ _ _ _
     a x _ _ _ _  _ _ _ _ n r
     _ c s c g _  _ g č š a _]))
(deflayer l-primary
  (apply lay off-thumbs (lb-thumbs))
  l-secondary
  layout-cstrm)
;; TODO: rest


(defn defs-by-main-layer [l]
  (let [{a ::action u ::update} (as-ua l)
        init {::layer-default (::name a)}]

    (process-deferred-updates u)))
(comment

  (defs-by-main-layer l-primary)

  (old-main)

  *e
  [])
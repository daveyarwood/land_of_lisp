(ns land-of-lisp.attack-of-the-robots!
  (:require [clojure.pprint :refer (cl-format)]
            [lanterna.terminal :as t]))

(def ^:dynamic *terminal* (t/get-terminal :text))

(def directions ; i use dvorak. qwerty equivalent: QWE/A-D/ZXC
  {\' -65   \, -64   \. -63
   \a -1             \e   1
   \;  63   \q  64   \j  65})

(defn print-instructions! []
  (t/put-string *terminal* "qwe/asd/zxc to move, (t)eleport, (l)eave" 0 0))

(def quit? (atom false))

(def player (ref 544))
(def robots (ref (repeatedly 10 #(rand-int 1024))))

(defn teleport! []
  (dosync (ref-set player (rand-int 1024))))

(defn leave!
  ([] (leave! "bye!"))
  ([msg]
    (t/clear *terminal*)
    (t/put-string *terminal* msg)
    (reset! quit? true)))

(defn you-win!  [] (leave! "you win!"))
(defn you-lose! [] (leave! "you lose!"))

(defn player-command! []
  (let [cmd (t/get-key-blocking *terminal*)]
    (case cmd
      \y (teleport!) ; (t)eleport
      \n (leave!)    ; (l)eave
      (when-let [∆ (get directions cmd)]
        (dosync (alter player + ∆))))))

(defn- move-robot [pos]
  (if (> (count (filter #{pos} @robots)) 1)
    pos
    (let [new-locs (for [[_ ∆] directions
                         :let [new-pos (+ pos ∆)]]
                     [(+ (Math/abs (- (rem new-pos 64)
                                      (rem @player 64)))
                         (Math/abs (- (bit-shift-right new-pos 6)
                                      (bit-shift-right @player 6))))
                      new-pos])]
      (->> new-locs (sort-by first) first second))))

(defn robots-move! []
  (dosync (alter robots (partial map move-robot))))

(defn all-robots-scrapped? []
  (every? #(> (count (filter #{%} @robots)) 1) @robots))

(defn player-scrapped? []
  (some #{@player} @robots))

(defn draw-board! []
  (t/clear *terminal*)
  (t/put-string *terminal*
    (cl-format nil "~%|~{~<|~%|~,65:;~A~>~}|"
               (for [pos (range 1024)
                     :let [robots-here (count (filter #{pos} @robots))]]
                 (cond
                   (> robots-here 1) \#
                   (= robots-here 1) \A
                   (= pos @player)   \@
                   :else             \space)))))

(defn game-loop []
  (print-instructions!)
  (player-command!)
  (while (not @quit?)
    (cond
      (all-robots-scrapped?) (you-win!)
      (player-scrapped?)     (you-lose!)
      :else (do
              (robots-move!)
              (draw-board!)
              (game-loop)))))

(defn play-game []
  (t/in-terminal *terminal*
    (game-loop)))

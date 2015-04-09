(ns land-of-lisp.attack-of-the-robots!
  (:require [clojure.pprint :refer (cl-format)])
  (:import  (jline Terminal)))

(def directions ; i use dvorak. qwerty equivalent: QWE/A-D/ZXC
  {\' -65   \, -64   \. -63
   \a -1             \e   1
   \;  63   \q  64   \j  65})

(defn print-instructions! []
  (cl-format true "~&qwe/asd/zxc to move, (t)eleport, (l)eave:"))

(def quit? (atom false))

(def player (ref 544))
(def robots (ref (repeatedly 10 #(rand-int 1024))))

(defn teleport []
  (dosync (ref-set player (rand-int 1024))))

(defn leave []
  (cl-format true "~&bye!~&")
  (reset! quit? true))

(defn player-command! []
  (let [term (Terminal/getTerminal)
        cmd  (char (.readCharacter term System/in))]
    (case cmd
      \y (teleport) ; (t)eleport
      \n (leave)    ; (l)eave
      (when-let [∆ (get directions cmd)]
        (dosync (alter player + ∆))))))

(defn game-loop []
  (while (not @quit?)
    "do game stuff, TODO"))

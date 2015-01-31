(ns land-of-lisp.orc-battle)

; settings

(def ^:dynamic *monster-num* 12)

; protocols

(defprotocol Stats
  (dead? [this] "Returns whether this is dead or not.")
  (show [this] "Print stats about this."))

(defprotocol Attack
  (attack! [this] "Attacks."))

(defprotocol TakeDamage
  (hit [this damage] "Returns modified version of this with damage taken."))

; player

(declare monsters pick-monster random-monster all-dead?)

(defrecord Player [health agility strength]
  Stats
  (dead? [this]
    (<= health 0))
  (show [this]
    (printf (str "You are a valiant knight with a health of %d, an agility "
                 "of %d, and a strength of %d.\n")
            health agility strength)
    (flush))
  Attack
  (attack! [this]
    (print "Attack style: [s]tab [d]ouble swing [r]oundhouse: ")
    (flush)
    (case (read-line)
      "s" (dosync
            (alter (pick-monster)
                   hit (+ 3 (rand-int (bit-shift-right strength 1)))))
      "d" (let [x (inc (rand-int (quot strength 6)))]
            (println "Your double swing has a strength of" x)
            (dosync
               (alter (pick-monster) hit x)
               (when-not (all-dead? @monsters)
                 (alter (pick-monster) hit x))))
      (dotimes [_ (+ 2 (rand-int (quot strength 3)))]
        (when-not (all-dead? @monsters)
          (dosync (alter (random-monster) hit 1)))))))


(def player (ref (->Player 30 30 30)))

(defn init-player []
  (dosync
    (alter player assoc-in [:health] 30)
    (alter player assoc-in [:agility] 30)
    (alter player assoc-in [:strength] 30)))

; monsters

(def monsters (ref nil))
(def monster-builders (ref nil))

(defn random-monster []
  (let [monster (rand-nth @monsters)]
    (if (dead? @monster)
      (recur)
      monster)))

(defn pick-monster []
  (printf "Monster #(0-%d): " (dec *monster-num*))
  (flush)
  (let [x (try (Integer/parseInt (read-line))
            (catch NumberFormatException e
              (println "That's not a number.")
              (pick-monster)))]
    (if-let [monster (get @monsters x)]
      (if (dead? @monster)
        (do
          (println "That monster is already dead.")
          (pick-monster))
        monster)
      (do
        (println "That is not a valid monster number.")
        (pick-monster)))))

(defn show-all [monsters]
  "to do - maybe just (doseq [m monster] (show m))")

(defn init-monsters []
  "to do")

; game

(defn game-loop []
  (when-not (or (dead? @player) (all-dead? @monsters))
    (show @player)
    (dotimes [_ (inc (quot (max 0 (:agility @player)) 15))]
      (when-not (all-dead? @monsters)
        (show-all @monsters)
        (attack! @player)))
    (doseq [monster @monsters]
      (if-not (dead? @monster)
        (attack! @monster)))
    (recur)))

(defn orc-battle []
  (init-monsters)
  (init-player)
  (game-loop)
  (cond
    (dead? @player)
      (println "You have been killed. Game Over.")
    (all-dead? @monsters)
      (println "Congratulations! You have vanquished all of your foes.")))

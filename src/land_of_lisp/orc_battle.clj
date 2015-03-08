(ns land-of-lisp.orc-battle
  (:require [fenrir :refer :all]
            [clojure.string :as str]))

; settings

(def ^:dynamic *monster-num* 12)

; base classes

(defclass Mortal [] [health]
  (dead? [] (<= health 0))
  (hit [damage] "Returns modified version of this with damage taken."))

(defclass Attacker [] [strength]
  (attack! [] "Attacks."))

; player

(declare monsters pick-monster random-monster)

(defclass Player [Mortal Attacker] [agility]
  (show []
    (printf (str "You are a valiant knight with a health of %d, an agility "
                 "of %d, and a strength of %d.\n")
            health agility strength)
    (flush))
  (attack! []
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
               (when-not (every? dead? @monsters)
                 (alter (pick-monster) hit x))))
      (dotimes [_ (+ 2 (rand-int (quot strength 3)))]
        (when-not (every? dead? @monsters)
          (dosync (alter (random-monster) hit 1)))))))

(def player (ref (ctor Player :health 30 :agility 30 :strength 30)))

(defn init-player! []
  (dosync
    (alter player set-slot :health 30)
    (alter player set-slot :agility 30)
    (alter player set-slot :strength 30)))

; monsters

(defclass Monster [Mortal] []
  (ctor [] (base-ctor *fclass* :health (rand-int 10)))
  (monster-class [] (-> (str (type *self*)) (str/split #"/") last))
  (show [] (println "A fierce" (monster-class *self*)))
  (hit [damage]
    (let [after-damage (update-in *self* [:health] - damage)]
      (if (dead? after-damage)
        (println "You killed the" (str (monster-class *self*) \!))
        (println "You hit the" (str (monster-class *self*) \,)
                 "knocking off" damage "health points!"))
      after-damage)))

(def monsters (ref []))
(def monster-builders (ref []))

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
  (println "Your foes:")
  (dotimes [x *monster-num*]
    (let [monster (@monsters x)]
      (printf "%d. %s"
              x
              (if (dead? monster)
                "**dead**"
                (printf "(Health=%d)" (:health monster))
                (show monster))))))

(defn init-monsters! []
  (dosync
    (ref-set monsters
             (repeatedly *monster-num* ((rand-nth @monster-builders))))))

(defclass Orc [Monster Attacker] [club-level]
  (ctor [] (base-ctor *fclass* :club-level (rand-int 8)
                               :health (:health (ctor Monster))))
  (show [] (println "A wicked orc with a level" club-level "club"))
  (attack! []
    (let [damage (rand-int club-level)]
      (println "An orc swings his club at you and knocks off" damage
               "of your health points.")
      (dosync (alter player update-in [:health] - damage)))))

(dosync (alter monster-builders conj #(ctor Orc)))

; game

(defn game-loop []
  (when-not (or (dead? @player) (every? dead? @monsters))
    (show @player)
    (dotimes [_ (inc (quot (max 0 (:agility @player)) 15))]
      (when-not (every? dead? @monsters)
        (show-all @monsters)
        (attack! @player)))
    (doseq [monster @monsters]
      (if-not (dead? @monster)
        (attack! @monster)))
    (recur)))

(defn orc-battle []
  (init-monsters!)
  (init-player!)
  (game-loop)
  (cond
    (dead? @player)
      (println "You have been killed. Game Over.")
    (every? dead? @monsters)
      (println "Congratulations! You have vanquished all of your foes.")))

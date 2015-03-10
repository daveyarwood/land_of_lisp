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
  (attack! [] "Attacks. Also returns (potentially modified) self."))

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
               (when-not (every? #(dead? @%) @monsters)
                 (alter (pick-monster) hit x))))
      (dotimes [_ (+ 2 (rand-int (quot strength 3)))]
        (when-not (every? #(dead? @%) @monsters)
          (dosync (alter (random-monster) hit 1)))))
    *self*))

(def player (ref (ctor Player :health 30 :agility 30 :strength 30)))

(defn init-player! []
  (dosync
    (alter player set-slot :health 30)
    (alter player set-slot :agility 30)
    (alter player set-slot :strength 30)))

; monsters

(defclass Monster [Mortal] []
  (ctor [] (base-ctor *fclass* :health (inc (rand-int 10))))
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
    (let [monster (monsters x)]
      (print (str x \. \space))
      (if (dead? @monster)
        (println "**dead**")
        (do
          (printf "(Health=%d) " (:health @monster))
          (show @monster))))))

(defn init-monsters! []
  (dosync
    (ref-set monsters
             (vec (take *monster-num*
                        (repeatedly #(ref ((rand-nth @monster-builders)))))))))

(defclass Orc [Monster Attacker] [club-level]
  (ctor [] (base-ctor *fclass* :club-level (inc (rand-int 8))
                               :health (:health (ctor Monster))))
  (show [] (println "A wicked orc with a level" club-level "club"))
  (attack! []
    (let [damage (inc (rand-int club-level))]
      (println "An orc swings his club at you and knocks off" damage
               "of your health points.")
      (dosync (alter player update-in [:health] - damage)))
    *self*))

(defclass Hydra [Monster Attacker] []
  (show [] (println "A malicious hydra with" health "heads"))
  (hit [damage]
    (let [after-damage (update-in *self* [:health] - damage)]
      (if (dead? after-damage)
        (println "The corpse of the fully decapitated and decapacitated hydra"
                 "falls to the floor!")
        (println "You lop off" damage "of the hydra's heads!"))
      after-damage))
  (attack! []
    (let [damage (inc (rand-int (bit-shift-right health 1)))]
      (println "A hydra attacks you with" damage "of its heads!"
               "It also grows back one more head!")
      (dosync (alter player update-in [:health] - damage))
      (update-in *self* [:health] inc))))

(defclass SlimeMold [Monster Attacker] [sliminess]
  (ctor [] (base-ctor *fclass* :sliminess (inc (rand-int 5))
                               :health (:health (ctor Monster))))
  (show [] (println "A slime mold with a sliminess of" sliminess))
  (attack! []
    (let [slime-factor (inc (rand-int sliminess))]
      (print "A slime mold wraps around your legs and decreases your agility"
             "by" (str slime-factor \! \space))
      (dosync (alter player update-in [:agility] - slime-factor))
      (if (zero? (rand-int 2))
        (do
          (println "It also squirts in your face, taking away a health point!")
          (dosync (alter player update-in [:health] dec)))
        (println)))
    *self*))

(defclass Brigand [Monster Attacker] []
  (attack! []
    (case (first (apply max-key val @player))
      :health (do
                (println "A brigand hits you with his slingshot, taking off"
                         "2 health points!")
                (dosync (alter player update-in [:health] - 2)))
      :agility (do
                 (println "A brigand catches your leg with his whip, taking"
                          "off 2 agility points!")
                 (dosync (alter player update-in [:agility] - 2)))
      :strength (do
                  (println "A brigand cuts your arm with his whip, taking off"
                           "2 strength points!")
                  (dosync (alter player update-in [:strength] - 2))))
    *self*))

(dosync
  (doseq [monster [Orc Hydra SlimeMold Brigand]]
    (alter monster-builders conj #(ctor monster))))

; game

(defn game-loop []
  (println)
  (when-not (or (dead? @player) (every? #(dead? @%) @monsters))
    (show @player)
    (println)
    (dotimes [_ (inc (quot (max 0 (:agility @player)) 15))]
      (when-not (every? #(dead? @%) @monsters)
        (show-all @monsters)
        (println)
        (dosync (alter player attack!))
        (println)))
    (doseq [monster @monsters]
      (if-not (dead? @monster)
        (dosync (alter monster attack!))))
    (recur)))

(defn orc-battle []
  (init-monsters!)
  (init-player!)
  (game-loop)
  (cond
    (dead? @player)
      (println "You have been killed. Game Over.")
    (every? #(dead? @%) @monsters)
      (println "Congratulations! You have vanquished all of your foes.")))

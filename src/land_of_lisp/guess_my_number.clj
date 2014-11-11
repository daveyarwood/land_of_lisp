(ns land-of-lisp.guess-my-number)

(def lower (atom 1))
(def upper (atom 100))

(defn guess-my-number []
  (bit-shift-right (+ @lower @upper) 1))

(defn smaller []
  (reset! upper (dec guess-my-number))
  (guess-my-number))

(defn bigger []
  (reset! lower (inc guess-my-number))
  (guess-my-number))

(defn start-over []
  (reset! lower 1)
  (reset! upper 100)
  (guess-my-number))
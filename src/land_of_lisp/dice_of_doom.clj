(ns land-of-lisp.dice-of-doom
  (:require [clojure.string :as str]))

(def ^:dynamic *num-players* 2)
(def ^:dynamic *max-dice* 3)
(def ^:dynamic *board-size* 4)
(def ^:dynamic *board-hexnum* (* *board-size* *board-size*))

(defn gen-board []
  (into []
    (repeatedly *board-hexnum* #(hash-map :player (rand-int *num-players*)
                                          :dice   (inc (rand-int *max-dice*))))))

(declare add-passing-move attacking-moves)

(defn game-tree* [board player spare-dice first-move?]
  {:player player
   :board  board 
   :moves  (add-passing-move board
                             player
                             spare-dice
                             first-move?
                             (lazy-seq (attacking-moves board player spare-dice)))}) 

(def game-tree (memoize game-tree*))

(defn add-new-dice [board player spare-dice]
  (loop [acc []
         [{cur-player :player
           cur-dice   :dice :as x} & more] board
         n spare-dice]
    (if (or (nil? x) (zero? n)) 
      (vec (concat acc (drop (count acc) board)))
      (let [[updated new-n] (if (and (= cur-player player)
                                      (< cur-dice *max-dice*))
                               [(update-in x [:dice] inc) (dec n)]
                               [x n])]
        (recur (conj acc updated) more new-n)))))

(defn add-passing-move [board player spare-dice first-move? moves]
  (if first-move?
    moves
    (cons {:move   :pass
           :result (game-tree (add-new-dice board player (dec spare-dice))
                              (mod (inc player) *num-players*)
                              0
                              true)}
          moves)))

(defn neighbors* [pos]
  (let [up   (- pos *board-size*)
        down (+ pos *board-size*)]
    (filter #(and (not (neg? %)) 
                  (< % *board-hexnum*))
            (concat (list up down)
                    (when-not (zero? (mod pos *board-size*))
                      (list (dec up) (dec pos)))
                    (when-not (zero? (mod (inc pos) *board-size*))
                      (list (inc pos) (inc down)))))))

(def neighbors (memoize neighbors*))

(defn board-attack [board player src dest dice]
  (mapv #(condp = %
           src  {:player player :dice 1}
           dest {:player player :dice (dec dice)}
           (nth board %))
        (range (count board))))

(defn attacking-moves [board cur-player spare-dice]
  (letfn [(player [pos] (:player (nth board pos)))
          (dice   [pos] (:dice   (nth board pos)))]
    (mapcat (fn [src]
              (when (= (player src) cur-player)
                (mapcat (fn [dest]
                          (when (and (not= (player dest) cur-player)
                                     (> (dice src) (dice dest)))
                            (list
                              {:move   {:from src :to dest}
                               :result (game-tree (board-attack board 
                                                                cur-player 
                                                                src 
                                                                dest 
                                                                (dice src))
                                                  cur-player
                                                  (+ spare-dice (dice dest))
                                                  nil)})))
                        (neighbors src))))
            (range *board-hexnum*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn player-letter [n]
  (char (+ 97 n)))

(defn draw-board [board]
  (doseq [y (range *board-size*)]
    (print (apply str (repeat (- *board-size* y) \space)))
    (doseq [x (range *board-size*)]
      (let [{:keys [player dice]} (nth board (+ x (* *board-size* y)))]
        (printf "%s-%s " (player-letter player) dice)))
    (println))) 

(defn print-info [{:keys [board player] :as tree}]
  (println "current player =" (player-letter player) \newline)
  (draw-board board))

(defn handle-human [{:keys [moves] :as tree}]
  (println "choose your move:\n")
  (doseq [n (range (count moves))]
    (printf "%s. " (inc n))
    (let [{:keys [move]} (nth moves n)]
      (if (not= move :pass)
        (println (:from move) "->" (:to move))
        (println "end turn"))))
  (:result (nth moves (dec (Integer/parseInt (read-line))))))

(defn winners [board]
  (let [totals (frequencies (map :player board))
        best   (apply max (vals totals))]
    (for [[player score] totals :when (= score best)]
      player)))

(defn announce-winner [board] 
  (let [w (winners board)]
    (if (> (count w) 1)
      (println "The game is a tie between:" 
               (str/join ", " (map player-letter w)))
      (println "The winner is" (player-letter (first w))))))

(defn play-vs-human [{:keys [board moves] :as tree}]
  (print-info tree)
  (println)
  (if (empty? moves)
    (announce-winner board)
    (play-vs-human (handle-human tree))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare get-ratings)

(defn rate-position* [tree player]
  (if-not (empty? (:moves tree))
    (apply (if (= (:player tree) player) max min)
           (get-ratings tree player))
    (let [w (winners (:board tree))]
      (if (contains? (set w) player)
        (/ 1 (count w))
        0))))

(def rate-position (memoize rate-position*))

(defn get-ratings [{:keys [moves]} player]
  (map (fn [{:keys [result]}] 
         (rate-position result player)) 
       moves))

(defn handle-computer [{:keys [player moves] :as tree}]
  (let [ratings (get-ratings tree player)]  
    (->> (map-indexed vector ratings)
         (apply max-key second)
         first
         (nth moves)
         :result)))

(defn play-vs-computer [{:keys [player board moves] :as tree}]
  (print-info tree)
  (println)
  (cond 
    (empty? moves) (announce-winner board)
    (zero? player) (play-vs-computer (handle-human tree))
    :else          (play-vs-computer (handle-computer tree))))

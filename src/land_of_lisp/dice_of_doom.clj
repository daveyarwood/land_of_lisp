(ns land-of-lisp.dice-of-doom
  (:require [clojure.string :as str]))

(def ^:dynamic *num-players* 2)
(def ^:dynamic *max-dice* 3)
(def ^:dynamic *board-size* 2)
(def ^:dynamic *board-hexnum* (* *board-size* *board-size*))

(defn gen-board []
  (into []
    (repeatedly *board-hexnum* #(hash-map :player (rand-int *num-players*)
                                          :dice   (inc (rand-int *max-dice*))))))

(declare add-passing-move attacking-moves)

(defn game-tree [board player spare-dice first-move?]
  {:player player
   :board  board 
   :moves  (add-passing-move board
                             player
                             spare-dice
                             first-move?
                             (attacking-moves board player spare-dice))}) 

(defn add-new-dice [board player spare-dice]
  (letfn [(f [[{cur-player :player 
                cur-dice   :dice :as x} & more :as board]
              n]
            (cond
              (nil? x)  nil
              (zero? n) board
              :else     (if (and (= cur-player player)
                                 (< cur-dice *max-dice*))
                          (cons {:player cur-player
                                 :dice (inc cur-dice)}
                                (f more (dec n)))
                          (cons x (f more n)))))]
    (into [] (f board spare-dice))))

(defn add-passing-move [board player spare-dice first-move? moves]
  (if first-move?
    moves
    (cons {:move   :pass
           :result (game-tree (add-new-dice board player (dec spare-dice))
                              (mod (inc player) *num-players*)
                              0
                              true)}
          moves)))

(defn neighbors [pos]
  (let [up   (- pos *board-size*)
        down (+ pos *board-size*)]
    (filter #(and (not (neg? %)) 
                  (< % *board-hexnum*))
            (concat (list up down)
                    (when-not (zero? (mod pos *board-size*))
                      (list (dec up) (dec pos)))
                    (when-not (zero? (mod (inc pos) *board-size*))
                      (list (inc pos) (inc down)))))))

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

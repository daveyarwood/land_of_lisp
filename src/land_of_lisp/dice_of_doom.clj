(ns land-of-lisp.dice-of-doom)

(def ^:dynamic *num-players* 2)
(def ^:dynamic *max-dice* 3)
(def ^:dynamic *board-size* 2)
(def ^:dynamic *board-hexnum* (* *board-size* *board-size*))

(defn gen-board []
  (into []
    (repeatedly *board-hexnum* #(hash-map :player (rand-int *num-players*)
                                          :dice   (inc (rand-int *max-dice*))))))

(defn player-letter [n]
  (char (+ 97 n)))

(defn draw-board [board]
  (doseq [y (range *board-size*)]
    (print (apply str (repeat (- *board-size* y) \space)))
    (doseq [x (range *board-size*)]
      (let [{:keys [player dice]} (nth board (+ x (* *board-size* y)))]
        (printf "%s-%s " (player-letter player) dice)))
    (println))) 

(declare add-passing-move attacking-moves add-new-dice)

(defn game-tree [board player spare-dice first-move?]
  (list player
        board
        (add-passing-move board
                          player
                          spare-dice
                          first-move?
                          (attacking-moves board player spare-dice)))) 

(defn add-passing-move [board player spare-dice first-move? moves]
  (if first-move?
    moves
    (cons (list nil ;  description of move (pass = nil)
                (game-tree (add-new-dice board player (dec spare-dice))
                           (mod (inc player) *num-players*)
                           0
                           true))
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
                              (list (list src dest)
                                    (game-tree (board-attack board cur-player src dest (dice src))
                                               cur-player
                                               (+ spare-dice (dice dest))
                                               nil)))))
                        (neighbors src))))
            (range *board-hexnum*))))

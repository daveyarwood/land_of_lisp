(ns land-of-lisp.dice-of-doom-web
  (:require [land-of-lisp.dice-of-doom :refer (*board-size* gen-board
                                               game-tree winners
                                               player-letter)]
            [land-of-lisp.svg          :refer :all]
            [ring.adapter.jetty        :refer (run-jetty)]
            [ring.middleware.session.store  :refer :all]
            [compojure.core            :refer :all]
            [compojure.route           :as    route]
            [compojure.handler         :as    handler]
            [hiccup.core               :refer (html)]
            [hiccup.page               :refer (html5)]
            [clojure.string            :as    str]))  

(def ^:dynamic *board-width* 900)
(def ^:dynamic *board-height* 500)
(def ^:dynamic *board-scale* 64)
(def ^:dynamic *top-offset* 3)
(def ^:dynamic *dice-scale* 40)
(def ^:dynamic *dot-size* 0.05)
(def ^:dynamic *die-colors* [[255 63 63] [63 63 255]])
(def ^:dynamic *current-game-tree* nil)
(def ^:dynamic *from-tile* nil)

(defn draw-die-svg [x y color]
  (letfn [(calc-pt [[pt-x pt-y]]
            [(+ x (* *dice-scale* pt-x)) 
             (+ y (* *dice-scale* pt-y))])
          (place-and-draw [points color]
            (polygon (map calc-pt points) color))]
    ; draw faces
    (place-and-draw [[0 -1] [-0.6 -0.75] [0 -0.5] [0.6 -0.75]]
                    (brightness color 40))
    (place-and-draw [[0 -0.5] [-0.6 -0.75] [-0.6 0] [0 0.25]]
                    color)
    (place-and-draw [[0 -0.5] [0.6 -0.75] [0.6 0] [0 0.25]]
                    (brightness color -40))
    ; draw dots
    (doall (map (fn [x y]
                  (polygon (map (fn [xx yy]
                                  (calc-pt [(+ x (* xx *dot-size*))
                                            (+ y (* yy *dot-size*))]))
                                [-1 -1 1 1]
                                [-1 1 1 -1])
                           [255 255 255]))
                [-0.05 0.125 0.3 -0.3 -0.125 0.05 
                 0.2 0.2 0.45 0.45 -0.45 -0.2]
                [-0.875 -0.8 -0.725 -0.775 -0.7 -0.625 
                 -0.35 -0.05 -0.45 -0.15 -0.45 -0.05]))))

(defn draw-tile-svg [x y pos {:keys [dice]} xx yy color chosen-tile]
  (doseq [z (range 2)]
    (polygon (map (fn [[x y]]
                    [(+ xx (* *board-scale* x)) 
                     (+ yy (* *board-scale* 
                              (+ y (* (- 1 z) 0.1))))])
                  [[-1 -0.2] [0 -0.5] [1 -0.2]
                   [1 0.2] [0 0.5] [-1 0.2]])
             (if (= pos chosen-tile)
               (brightness color 100)
               color)))
  (doseq [z (range dice)]
    (draw-die-svg (+ xx
                     (* *dice-scale*
                        0.3
                        (if (odd? (+ x y z))
                          -0.3
                          0.3)))
                  (- yy (* *dice-scale* z 0.8)) 
                  color)))

(defn make-game-link [pos]
  (str "/game.html?chosen=" pos))

(defn draw-board-svg [board chosen-tile legal-tiles]
  (doseq [y (range *board-size*)
          x (range *board-size*)
          :let [pos (+ x (* *board-size* y))
                {:keys [player] :as hex} (nth board pos)
                xx (* *board-scale* (+ (* 2 x) (- *board-size* y)))
                yy (* *board-scale* (+ (* y 0.7) *top-offset*))
                color (brightness (nth *die-colors* player)
                                  (* -15 (- *board-size* y)))]]
    (if (contains? (set legal-tiles) pos)
      (tag g []
        (tag a ["xlink:href" (make-game-link pos)]
          (draw-tile-svg x y pos hex xx yy color chosen-tile)))
      (draw-tile-svg x y pos hex xx yy color chosen-tile))))

(defn initialize-game []
  (alter-var-root #'*from-tile* 
                  (constantly nil))
  (alter-var-root #'*current-game-tree* 
                  (constantly (game-tree (gen-board) 0 0 true))))

(defn announce-winner [board]
  (let [w (winners board)
        announcement (if (> (count w) 1)
                       (str "The game is a tie between " 
                            (str/join ", " (map player-letter w)))
                       (str "The winner is " 
                            (player-letter (first w))))]
    (html announcement [:br]
          [:br]
          [:a {:href "game.html"} "play again"])))

(defn handle-human [pos]
  (cond
    (not pos) 
    "Please choose a hex to move from:"

    (= pos "pass")
    (do
      (alter-var-root #'*current-game-tree* #(-> % :moves first :result))
      (html "Your reinforcements have been placed."
            [:br]
            [:a {:href (make-game-link nil)} "continue"]))

    (not *from-tile*)
    (do
      (alter-var-root #'*from-tile* (constantly pos))
      "Now choose a destination:")
    
    (= pos *from-tile*)
    (do
      (alter-var-root #'*from-tile* (constantly nil))
      "Move canceled.")
    
    :else
    (do 
      (alter-var-root #'*current-game-tree*
                      #(->> %
                            :moves
                            (filter (fn [{:keys [move]}]
                                      (= move {:from *from-tile*
                                               :to   pos})))
                            first
                            :result))
      (alter-var-root #'*from-tile* (constantly nil))
      (html "You may now"
            [:a {:href (make-game-link "pass")} "pass"]
            "or make another move:"))))

(defn handle-computer []
  (alter-var-root #'*current-game-tree* 
                  land-of-lisp.dice-of-doom/handle-computer)
  (html 
    "The computer has moved."
    [:script "window.setTimeout('window.location=\"game.html?chosen=NIL\"',5000)"]))

(defn draw-dod-page [{:keys [board moves]} selected-tile]
  (with-out-str
    (svg *board-width* *board-height*
         (draw-board-svg board
                         selected-tile
                         (map (if selected-tile
                                (fn [{:keys [move]}]
                                  (when (= (:from move) selected-tile)
                                    (:to move)))
                                (fn [{:keys [move]}]
                                  (:from move)))
                              moves)))))

(defroutes app
  (GET "/game.html" [chosen]
    (when-not (and *current-game-tree* chosen)
      (initialize-game))
    (html5 
      [:center 
       "Welcome to DICE OF DOOM!"
       [:br]
       (cond 
         (empty? (:moves *current-game-tree*))
         (announce-winner (:board *current-game-tree*))
           
         (zero? (:player *current-game-tree*))
         (handle-human chosen)

         :else
         (handle-computer))
       [:br]
       (draw-dod-page *current-game-tree* *from-tile*)]))
  (route/not-found "Sorry... I don't know that page."))

(defn serve []
  (run-jetty (handler/site app) {:port 3000}))

; (this doesn't really fully work... jetty is finicky.
; if I were to do this for reals, I would do it in Hoplon.)

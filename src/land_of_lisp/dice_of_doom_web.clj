(ns land-of-lisp.dice-of-doom-web
  (:require [land-of-lisp.dice-of-doom :refer (*board-size* gen-board)]
            [land-of-lisp.web-server]
            [land-of-lisp.svg          :refer :all]))

(def ^:dynamic *board-width* 900)
(def ^:dynamic *board-height* 500)
(def ^:dynamic *board-scale* 64)
(def ^:dynamic *top-offset* 3)
(def ^:dynamic *dice-scale* 40)
(def ^:dynamic *dot-size* 0.05)
(def ^:dynamic *die-colors* [[255 63 63] [63 63 255]])

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

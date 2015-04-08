(ns land-of-lisp.evolution)

;;; modeling data ;;;

(defrecord Point [x y])
(defrecord Rectangle [p1 p2])

(defn random-point
  "Returns a random coordinate within a rectangular area (field)."
  [field]
  (let [{:keys [p1 p2]} field
        {x1 :x, y1 :y}  p1
        {x2 :x, y2 :y}  p2
        x               (rand-nth (apply range (sort [x1 x2])))
        y               (rand-nth (apply range (sort [y1 y2])))]
    (Point. x y)))

(def ^:const UP-LEFT    0)
(def ^:const UP         1)
(def ^:const UP-RIGHT   2)
(def ^:const RIGHT      3)
(def ^:const DOWN-RIGHT 4)
(def ^:const DOWN       5)
(def ^:const DOWN-LEFT  6)
(def ^:const LEFT       7)

(def ∆
  {UP-LEFT   [-1 -1]  UP   [0 -1]  UP-RIGHT   [1 -1]
   LEFT      [-1  0]               RIGHT      [1  0]
   DOWN-LEFT [-1  1]  DOWN [0 1]   DOWN-RIGHT [1  1]})

;;; config ;;;

(def ^:dynamic *width* 79)
(def ^:dynamic *height* 23)
(def ^:dynamic *jungle* (Rectangle. (Point. 45 10) (Point. 55 20)))
(def ^:dynamic *plant-energy* 80)
(def ^:dynamic *reproduction-energy* 200)

;;; plants ;;;

(def plants (ref #{})) ; a set of coordinates where there are plants

(defn add-plants!
  "Adds one plant to the jungle region and one anywhere in the world."
  []
  (dosync
    (alter plants conj (random-point *jungle*))
    (alter plants conj (random-point (Rectangle. (Point. 0 0)
                                                 (Point. *width* *height*))))))

;;; animals ;;;

(defrecord Animal [location energy facing genes])

(def first-animal
  (ref
    (map->Animal {:location (Point. (bit-shift-right *width* 1)
                                    (bit-shift-right *height* 1))
                  :energy   350
                  :facing   UP-LEFT
                  :genes    (vec (repeatedly 8 #(inc (rand-int 10))))})))

(def animals (ref [first-animal]))

(defn move! [animal]
  (let [{{:keys [x y]} :location, facing :facing} @animal
        [∆x ∆y] (∆ facing)]
    (dosync
      (alter animal update-in [:location]
                    #(-> %
                       (update-in [:x] (fn [x] (rem (+ x ∆x *width*) *width*)))
                       (update-in [:y] (fn [y] (rem (+ y ∆y *height*) *height*)))))
      (alter animal update-in [:energy] dec))))

(defn turn! [animal]
  (let [{:keys [genes]} @animal
        x               (rand-int (apply + genes))
        angle           (fn angle [[gene & genes] x]
                          (let [xnu (- x gene)]
                            (if (neg? xnu)
                              0
                              (inc (angle genes xnu)))))]
    (dosync
      (alter animal update-in [:facing] #(rem (+ % (angle genes x)) 8)))))

(defn eat! [animal]
  (let [{:keys [location]} @animal]
    (when (contains? @plants location)
      (dosync
       (alter animal update-in [:energy] + *plant-energy*)
       (alter plants disj location)))))

(defn reproduce! [animal]
  (when (>= (:energy @animal) *reproduction-energy*)
    (let [child (update-in @animal [:genes (rand-int 8)]
                                   #(max 1 (+ % (rand-nth (range -1 2)))))]
      (dosync
        (alter animal update-in [:energy] #(bit-shift-right % 1))
        (alter animals conj (ref child))))))

;;; world ;;;

(defn update-world! []
  (dosync (alter animals (partial remove #(<= (:energy @%) 0))))
  (doseq [animal @animals]
    (turn! animal)
    (move! animal)
    (eat! animal)
    (reproduce! animal))
  (add-plants!))

(defn draw-world! []
  (letfn [(pos [{:keys [x y]}]
            (cond
              (some #(= (:location @%) (Point. x y)) @animals) \M
              (contains? @plants (Point. x y))                 \*
              :else                                            \space))
          (row [y]
            (concat [\|]
                    (for [x (range *width*)]
                      (pos (Point. x y)))
                    [\| \newline]))]
    (let [rows (for [y (range *height*)]
                 (apply str (row y)))]
      (println (apply str rows)))))

(defn evolution []
  (draw-world!)
  (let [input (read-line)]
    (when-not (= input "quit")
      (let [days (try (Integer/parseInt input)
                   (catch NumberFormatException e 1))]
        (dotimes [x days]
          (update-world!)
          (if (zero? (rem x 50)) (println \.)))
        (recur)))))

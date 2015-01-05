(ns land-of-lisp.grand-theft-wumpus
  (require [land-of-lisp.graph-util :refer :all]
           [clojure.set :refer (difference intersection)]))

;; game config

(def ^:dynamic *node-num* 30)
(def ^:dynamic *edge-num* 45)
(def ^:dynamic *worm-num* 3)
(def ^:dynamic *cop-odds* 1/15)

;; generating random edges

(defn random-node []
  (inc (rand-int *node-num*)))

(defn edge-pair [a b]
  (when-not (= a b)
    (list [a b] [b a])))

(defn make-edge-list []
  (apply concat
    (repeatedly *edge-num* #(edge-pair (random-node) (random-node)))))

;; preventing islands

(defn direct-edges [node edge-list]
  (filter #(= (first %) node) edge-list))

(defn get-connected
  "Returns the set of all nodes reachable from a given node."
  [node edge-list]
  (let [visited (atom #{})
        traverse (fn traverse [node]
                   (when-not (@visited node)
                     (swap! visited conj node)
                     (doseq [[from-node to-node] (direct-edges node edge-list)]
                       (traverse to-node))))]
    (traverse node)
    @visited))

(defn connected-nodes
  "Returns the cumulative set of all connected nodes."
  [edge-list]
  (set (apply concat edge-list)))

(defn find-islands [nodes edge-list]
  (difference (set nodes) (connected-nodes edge-list)))

(defn connect-island
  "Returns new edges going to and from an island and a randomly chosen node
   that is already connected."
  [island edge-list]
  (edge-pair island (rand-nth (seq (connected-nodes edge-list)))))

(defn connect-all-islands [nodes edge-list]
  (let [islands (find-islands nodes edge-list)
        new-connections (mapcat #(connect-island % edge-list) islands)]
    (concat edge-list new-connections)))

;; building the city edges (with cops)

(defn edge-map
  "Returns a map of each node to the nodes to which it is connected."
  [edge-list]
  (into {}
    (for [node (range 1 (inc *edge-num*))
          :let [direct-edges (direct-edges node edge-list)]
          :when (not (empty? direct-edges))]
      [node (set (map second direct-edges))])))

(defn add-cops [edge-map cops]
  (into {}
    (for [[node edges] edge-map]
      [node (for [edge edges]
              (if-not (empty? (intersection (set cops)
                                            (set (edge-pair node edge))))
                (list edge 'cops)
                (list edge)))])))

(defn make-city-edges []
  (let [nodes (range 1 (inc *node-num*))
        edge-list (connect-all-islands nodes (make-edge-list))
        cops (filter (fn [_] (<= (rand) *cop-odds*)) edge-list)]
    (add-cops (edge-map edge-list) cops)))

;; building the nodes

(defn neighbors [node edge-map]
  (map first (edge-map node)))

(defn within-one? [a b edge-map]
  (contains? (set (neighbors a edge-map)) b))

(defn within-two?  [a b edge-map]
  (boolean
    (or (within-one? a b edge-map)
        (some #(within-one? % b edge-map) (neighbors a edge-map)))))

(defn make-city-nodes [edge-map]
  (let [wumpus (random-node)
        glow-worms (repeatedly *worm-num* #(random-node))]
    (into {}
      (for [n (range 1 (inc *node-num*))]
        [n
        (remove nil?
          (list
           (cond
             (= n wumpus) 'wumpus
             (within-two? n wumpus edge-map) 'blood!)
           (cond
             ((set glow-worms) n) 'glow-worm
             (some #(within-one? n % edge-map) glow-worms) 'lights!)
           (let [[node & neighbors] (edge-map n)]
             (when (some #((set %) 'cops) neighbors) 'sirens!))))]))))

;; game state

(def congestion-city-nodes (ref nil))
(def congestion-city-edges (ref nil))
(def player-pos (ref nil))
(def visited-nodes (ref nil))

;; drawing only known city nodes

(defn known-city-nodes []
  (let [visited-info (zipmap @visited-nodes
                             (map @congestion-city-nodes @visited-nodes))
        neighbors (set (mapcat #(neighbors % @congestion-city-edges)
                               @visited-nodes))
        mystery-neighbors (into {} (map #(vector % '(?)) neighbors))]
    (merge mystery-neighbors
           visited-info
           {@player-pos (conj (visited-info @player-pos) '*)})))

(defn known-city-edges []
  (let [visited-info (zipmap @visited-nodes
                             (map @congestion-city-edges @visited-nodes))]
    (into {}
      (for [[node edges] visited-info]
        [node (for [edge edges]
                (if ((set @visited-nodes) (first edge))
                  edge
                  (list (first edge))))]))))

(defn draw-city! []
  (ugraph->png "city.dot" (for [[node stuff-in-it] @congestion-city-nodes]
                            (cons node stuff-in-it))
                          @congestion-city-edges))

(defn draw-known-city! []
  (ugraph->png "known-city.dot" (for [[node stuff-in-it] (known-city-nodes)]
                              (cons node stuff-in-it))
                            (known-city-edges)))

;; initializing a new game

(defn find-empty-node []
  (let [x (random-node)]
    (if (empty? (@congestion-city-nodes x))
      x
      (recur))))

(defn new-game! []
  (dosync
    (ref-set congestion-city-edges (make-city-edges))
    (ref-set congestion-city-nodes (make-city-nodes @congestion-city-edges))
    (ref-set player-pos (find-empty-node))
    (ref-set visited-nodes (list @player-pos)))
  (draw-city!)
  (draw-known-city!))

;; walking around town

(declare handle-direction handle-new-place)

(defn walk [pos]
  (handle-direction pos))

(defn charge [pos]
  (handle-direction pos :charging true))

(defn handle-direction [pos & {:keys [charging]}]
  (let [possible-edges (set (neighbors @player-pos @congestion-city-edges))]
    (if (contains? possible-edges pos)
      (handle-new-place pos :charging charging)
      (println "You can't get there from where you are!"))))

(defn handle-new-place [pos & {:keys [charging]}]
  (let [node (@congestion-city-nodes pos)
        glow-worm? (and ((set node) 'glow-worm)
                        (not ((set @visited-nodes) pos)))
        cops? (some #(and ((set %) pos) ((set %) 'cops))
                    (@congestion-city-edges @player-pos))
        wumpus? ((set node) 'wumpus)]
    (dosync
      (commute visited-nodes conj pos)
      (ref-set player-pos pos))
    (draw-known-city!)
    (cond
      cops? (println "You ran into the cops. Game over.")
      wumpus? (if charging
                (println "You found the wumpus!")
                (println "You ran into the wumpus. Game over."))
      charging (println "You wasted your last bullet. Game over.")
      glow-worm? (let [new-pos (random-node)]
                   (println "You ran into a Glow Worm Gang!"
                            "You are now at" new-pos)
                   (handle-new-place new-pos)))))

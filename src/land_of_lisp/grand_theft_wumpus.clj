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
  "Returns new edges going to and from an island and a randomly chosen node that is
   already connected."
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

(defn add-cops2 ;; alist version
  "Returns a 'nested alist' of each node to the nodes to which it is connected;
   if the edge contains cops, that edge is associated with the symbol 'cops.

   Using this format to follow suit with the book, which uses this format in
   order to be compatible with the graphing libraries."
  [edge-map cops]
  (map (fn [[node edges]]
         (cons node
               (for [edge edges]
                 (if-not (empty? (intersection (set cops)
                                               (set (edge-pair node edge))))
                   (list edge 'cops)
                   (list edge)))))
       edge-map))

(defn make-city-edges []
  (let [nodes (range 1 (inc *edge-num*))
        edge-list (connect-all-islands nodes (make-edge-list))
        cops (filter (fn [_] (<= (rand) *cop-odds*)) edge-list)]
    (add-cops (edge-map edge-list) cops)))

;; building the nodes

(defn look-up-node [node edge-alist]
  "Utility function for retrieving a node's entry from an alist.
   (In CL, this is just 'assoc'.)"
  (first (filter #(= (first %) node) edge-alist)))

(defn neighbors [node edge-map]
  (let [[nexus & neighbors] (edge-map node)]
    (map first neighbors)))

(defn neighbors2 [node edge-alist] ;; alist version
  (let [[nexus & neighbors] (look-up-node node edge-alist)]
    (map first neighbors)))

(defn within-one? [a b edge-alist]
  (contains? (set (neighbors a edge-alist)) b))

(defn within-two?  [a b edge-alist]
  (boolean
    (or (within-one? a b edge-alist)
        (some #(within-one? % b edge-alist) (neighbors a edge-alist)))))

(defn make-city-nodes [edge-map]
  (let [wumpus (random-node)
        glow-worms (repeatedly *worm-num* #(random-node))]
    (for [n (range 1 (inc *node-num*))]
      (remove nil?
        [n
         (cond
           (= n wumpus) 'wumpus
           (within-two? n wumpus edge-map) 'blood!)
         (cond
           ((set glow-worms) n) 'glow-worm
           (some #(within-one? n % edge-map) glow-worms) 'lights!)
         (let [[node & neighbors] (edge-map n)]
           (when (some #(= (second %) 'cops) neighbors) 'sirens!))]))))

(defn make-city-nodes2 [edge-alist] ;; alist version
  (let [wumpus (random-node)
        glow-worms (repeatedly *worm-num* #(random-node))]
    (for [n (range 1 (inc *node-num*))]
      (remove nil?
        [n
         (cond
           (= n wumpus) 'wumpus
           (within-two? n wumpus edge-alist) 'blood!)
         (cond
           ((set glow-worms) n) 'glow-worm
           (some #(within-one? n % edge-alist) glow-worms) 'lights!)
         (let [[node & neighbors] (look-up-node n edge-alist)]
           (when (some #(= (second %) 'cops) neighbors) 'sirens!))]))))

;; game state

(def congestion-city-nodes (ref nil))
(def congestion-city-edges (ref nil))
(def player-pos (ref nil))
(def visited-nodes (ref nil))

;; initializing a new game

(defn find-empty-node []
  (let [x (random-node)
        [node & stuff-in-it] (look-up-node x @congestion-city-nodes)]
    (if stuff-in-it (recur) x)))

(defn draw-city []
  (ugraph->png "city.dot" @congestion-city-nodes @congestion-city-edges))

(defn new-game []
  (dosync
    (ref-set congestion-city-edges (make-city-edges))
    (ref-set congestion-city-nodes (make-city-nodes @congestion-city-edges))
    (ref-set player-pos (find-empty-node))
    (ref-set visited-nodes [@player-pos]))
  (draw-city))

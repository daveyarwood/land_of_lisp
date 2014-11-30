(ns land-of-lisp.grand-theft-wumpus
  (require [land-of-lisp.graph-util :refer :all]
           [clojure.set :refer (difference)]))

(def congestion-city-nodes (ref nil))
(def congestion-city-edges (ref nil))
(def visited-nodes (ref nil))
(def node-num (ref 30))
(def edge-num (ref 45))
(def worm-num (ref 3))
(def cop-odds (ref 15))

;; generating random edges

(defn random-node []
  (inc (rand-int @node-num)))

(defn edge-pair [a b]
  (when-not (= a b)
    (list [a b] [b a])))

(defn make-edge-list []
  (apply concat
    (repeatedly @edge-num #(edge-pair (random-node) (random-node)))))

;; preventing islands

(defn direct-edges [node edge-list]
  (filter #(= (first %) node) edge-list))

(defn get-connected
  "Returns the set of all nodes connected to a given node."
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

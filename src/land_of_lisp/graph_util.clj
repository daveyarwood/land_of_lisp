(ns land-of-lisp.graph-util
  (:require [clojure.string :as str]
            [clojure.java.shell :refer (sh)]))

(def max-label-length 30)

(defn- dot-name [exp]
  (str/replace (str exp) #"[^A-Za-z0-9]" "_"))

(defn- dot-label [exp]
  (cond
    (nil? exp) ""
    (> (count (str exp)) max-label-length) (str
                                             (subs (str exp) 0 (- max-label-length 3))
                                             "...")
    :else (str exp)))

;;;

(defn- nodes->dot [nodes]
  (doseq [node nodes]
    (println (str (dot-name (first node))
                  "[label=\"" (dot-label node) "\"];"))))

(defn- edges->dot [edges]
  (doseq [[from-place edges] edges
         [to-place & edge] edges]
    (println (str (dot-name from-place) "->" (dot-name to-place)
                  "[label=\"" (dot-label edge) "\"];"))))

(defn- graph->dot [nodes edges]
  (println "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (println "}"))

(defn- dot->png [fname thunk]
  (binding [*out* (java.io.FileWriter. fname)]
    (thunk))
  (sh "dot" "-Tpng" "-O" fname)
  (shutdown-agents))

(defn graph->png [fname nodes edges]
  "Creates a directed graph (dot and png files) given filename, nodes and edges."
  (dot->png fname #(graph->dot nodes edges)))

(seq twa/edges)

;;;

(defn- uedges->dot [edges]
  (let [edges (seq edges)]
    (dotimes [n (count edges)]
      (let [[[this-place this-edges] & remaining-edges] (drop n edges)]
        (doseq [[that-place & edge] this-edges]
          (when-not (some #(= (first %) that-place) remaining-edges)
            (println (str (dot-name this-place) "--" (dot-name that-place)
                          "[label=\"" (dot-label edge) "\"];"))))))))

(defn- ugraph->dot [nodes edges]
  (println "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (println "}"))

(defn ugraph->png [fname nodes edges]
  "Creates an undirected graph (dot and png files) given filename, nodes and edges."
  (dot->png fname #(ugraph->dot nodes edges)))

(ns comp-runner.graph
  (:use clojure.core.incubator)
  (:require [clojure.set :as set])
  (:require [clojure.string :as string]))

(declare reduce-bfs input-nodes)

(defrecord OrGraph [inputs outputs]
  Object
  (toString [graph] (string/join
                  "\n"
                  (reduce-bfs
                    (fn [acc node]
                      (let [inputs (input-nodes graph node)]
                        (if (not (empty? inputs))
                          (conj acc (str (string/join ", " inputs) " -> " node))
                          acc)))
                    []
                    graph))))

(defn make-graph []
  "Creates empty oriented graph."
  (OrGraph. {} {}))

(defn add-node [graph node]
  "Returns graph which is the copy of specified graph plus the node.
  Returns the same graph if it already contains the node."
  (if (contains? (:outputs graph) node)
    graph
    (assoc-in (assoc-in graph [:outputs node] #{}) [:inputs node] #{})))

(defn add-edge [graph from to]
  "Returns new graph which is the specified graph with new edge"
  (let [graph-with-nodes (add-node (add-node graph from) to)]
    (update-in (update-in graph-with-nodes [:outputs from] conj to) [:inputs to] conj from)))

(defn input-nodes 
  "Returns set of nodes which have output edge into specified node"
  [graph node]
  ((:inputs graph) node))

(defn output-nodes 
  "Returns set of nodes which the specified node has output edges into"
  [graph node]
  ((:outputs graph) node))

(defn contains-edge? [graph from to]
  "Does the graph contain the edge?"
  (contains? (output-nodes graph from) to))

(defn contains-node? [graph node]
  "Does the graph contain the node?"
  (contains? (:outputs graph) node))

(defn contains-path? [graph from to]
  (if (or (not (contains-node? graph from)) (not (contains-node? graph to)))
    false
    (loop [visited #{} to-visit #{from}]
      (if (empty? to-visit)
        false
        (if (contains? to-visit to)
          true
          (let [check-from (first to-visit)
                new-visited (conj visited check-from)
                unvisited-outputs (set (filter #(not (contains? new-visited %)) (output-nodes graph check-from)))
                new-to-visit (disj (set/union to-visit unvisited-outputs) check-from)]
            (recur new-visited new-to-visit)))))))

(defn graph-empty? [graph]
  "Does the graph have no nodes?"
  (and (empty? (:outputs graph)) (empty? (:inputs graph))))

(defn nodes
  "Returns a set of all graph nodes for which the function returns true (all nodes by default)"
  ([graph]
   (set (-> graph :outputs keys)))
  ([func graph]
   (reduce (fn [ret node] (if (func node) (conj ret node) ret)) #{} (nodes graph))))

(defn remove-node
  "Returns new graph which is the same as provided but without specified node and all adjacent edges"
  [graph node]
  (let [new-graph (dissoc-in (dissoc-in graph [:outputs node]) [:inputs node])]
    (reduce 
      (fn [cur-graph cur-node]
        (update-in (update-in cur-graph [:outputs cur-node] disj node) [:inputs cur-node] disj node))
      new-graph
      (nodes new-graph))))

(defn remove-edge
  "Returns new graph which is the same as provided but without specified edge"
  [graph from to]
  (if (not (contains-edge? graph from to))
    graph
    (update-in (update-in graph [:outputs from] disj to) [:inputs to] disj from)))
  

(defn reduce-bfs
  "Performs traversal over the graph,
  calling specified function and passing accumulator and current node to it. Returns resulting accumulator."
  [function acc graph]
  (reduce function acc (-> graph :outputs keys)))

(defn filter-bfs
  "Performs traversal over the graph, calling specified function, passing current node to it.
  Returns new graph only with nodes for which the function returned true."
  [function graph]
  (reduce-bfs 
    (fn [cur-graph node]
      (if (function node)
        cur-graph
        (remove-node cur-graph node))) graph graph))

(defn some-bfs
  [function graph]
  (some function (-> graph :outputs keys)))

(defn remove-nodes
  "Removes specified nodes from the graph"
  [graph nodes]
  (filter-bfs (fn [node] (not (some #(= % node) nodes)))))

(defn remove-unreachable-nodes
  "Returns new graph with all unreachable nodes removed considering specified nodes as roots."
  [graph roots]
  (filter-bfs (fn [node] (some #(contains-path? graph % node) roots)) graph))
   
  
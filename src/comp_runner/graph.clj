(ns comp-runner.graph
  (:use clojure.core.incubator)
  (:require [clojure.set :as set])
  (:require [clojure.string :as string]))

(declare reduce-bfs input-nodes)

(defrecord OrGraph [inputs outputs attrs]
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
                    graph)))
  )

(defn make-graph []
  "Creates empty oriented graph."
  (OrGraph. {} {} {}))

(defn add-node [graph node]
  "Returns graph which is the copy of specified graph plus the node.
  Returns the same graph if it already contains the node."
  (if (contains? (:outputs graph) node)
    graph
    (-> graph
        (assoc-in [:outputs node] #{})
        (assoc-in [:inputs node] #{}))))

(defn add-node-attr [graph node k v]
  (assoc-in graph [:attrs node k] v))

(defn node-attr 
  ([graph node k]
   (node-attr graph node k nil))
  ([graph node k default]
   (get-in graph [:attrs node k] default)))

(defn add-edge [graph from to]
  "Returns new graph which is the specified graph with new edge"
  (-> graph
      (add-node from)
      (add-node to)
      (update-in [:inputs to] conj from)
      (update-in [:outputs from] conj to)))

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
  (let [new-graph (-> graph
                    (dissoc-in [:inputs node])
                    (dissoc-in [:outputs node])
                    (dissoc-in [:attrs node]))]
    (reduce 
      (fn [cur-graph cur-node]
        (-> cur-graph
            (update-in [:outputs cur-node] disj node)
            (update-in [:inputs cur-node] disj node)))
      new-graph
      (nodes new-graph))))

(defn remove-edge
  "Returns new graph which is the same as provided but without specified edge"
  [graph from to]
  (-> graph 
      (update-in [:outputs from] disj to)
      (update-in [:inputs to] disj from)))

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
   
  
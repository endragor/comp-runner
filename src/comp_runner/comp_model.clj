(ns comp-runner.comp-model
  (:require [comp-runner.graph :as graph]))

(defrecord CompModel [graph key-to-node]
  Object
  (toString [_] (.toString graph)))

(defn make-model []
  (CompModel. (graph/make-graph) {}))

(defrecord FuncNode [type func weight]
  Object
  (toString [_] (.toString func)))

(defrecord VarNode [type value weight]
  Object
  (toString [_] (.toString value)))

(defn make-func-node [{:keys [type func weight] :or {type :simple weight 1}}]
  (assert (integer? weight))
  (assert (contains? #{:simple :complex} type))
  (assert (fn? func))
  (FuncNode. type func weight))

(defn make-var-node [{:keys [type value weight] :or {type :simple weight 0}}]
  (assert (contains? #{:simple :massive} type))
  (assert (integer? weight))
  (VarNode. type value weight))

(defn func-node? [node]
  (= (type node) FuncNode))

(defn node-weight [node]
  (:weight node))

(defn node-func [node]
  (:func node))

(defn node-value [node]
  (:value node))

(defn make-model-from-graph [graph]
  (let [var-nodes (graph/nodes #(not (func-node? %)) graph)
        var-node-map (reduce #(assoc %1 (:value %2) %2) {} var-nodes)]
    (CompModel. graph var-node-map)))

(defn add-dependency [model node1 node2]
  (assert (= 1 (count (filter func-node? [node1 node2]))) (str "Dependencies must only be between functions and variables " node1 node2))
  (let [model-with-edge (update-in model [:graph] graph/add-edge node1 node2)]
    (if (not (func-node? node1))
      (update-in model-with-edge [:key-to-node] assoc (node-value node1) node1)
      (update-in model-with-edge [:key-to-node] assoc (node-value node2) node2))))

(defn inputs [model func-node]
  (assert (func-node? func-node))
  (assert (graph/contains-node? (:graph model) func-node))
  (graph/input-nodes (:graph model) func-node))

(defn output [model func-node]
  (assert (func-node? func-node))
  (assert (graph/contains-node? (:graph model) func-node))
  (first (graph/output-nodes (:graph model) func-node)))

(defn func-nodes [model]
  (graph/nodes func-node? (:graph model)))

(defn var-node-by-key [model kw]
  (let [ret ((:key-to-node model) kw)]
    (assert (not (nil? ret)))
    ret))

(defn requiring-funcs [model variable-keyword]
  (graph/output-nodes (:graph model) (var-node-by-key model variable-keyword)))

(defn- calculate-ready-inputs [model input-keys]
  (loop [to-visit input-keys visited #{} fn-inputs-count (zipmap (func-nodes model) (repeat 0))]
    (if (empty? to-visit)
      fn-inputs-count      
      (let [var-key (first to-visit)
            out-fns (requiring-funcs model var-key)
            new-visited (conj visited var-key)
            new-fn-inputs-count (reduce 
                                  (fn [counts out-fn] 
                                      (update-in counts [out-fn] inc))
                                  fn-inputs-count
                                  out-fns)
            full-fn-inputs (filter #(= (new-fn-inputs-count %) (count (inputs model %)))
                                   (keys new-fn-inputs-count))
            new-to-visit (reduce 
                           (fn [to-visit func-node]
                             (let [output (output model func-node)
                                   output-key (node-value output)]
                               (if (contains? new-visited output-key)
                                 to-visit
                                 (conj to-visit output-key))))
                           (disj to-visit var-key)
                           full-fn-inputs)]
        (recur new-to-visit new-visited new-fn-inputs-count)))))

(defn- remove-bad-paths [graph required-output]
  (graph/filter-bfs
    #(graph/contains-path? graph % required-output)
    graph))

(defn clean
  "Constructs new model which contains only variables and functions which can lead to calculation of
  required output from specified inputs"
  [model input-map required-output]
  (let [fns (func-nodes model)
        input-keys (set (keys input-map))
        fn-inputs-count (calculate-ready-inputs model input-keys)
        graph (:graph model)
        required-output-node (var-node-by-key model required-output)
        non-full-fn-inputs (filter #(< (fn-inputs-count %) (count (inputs model %))) fns)
        filtered-graph (graph/filter-bfs #(or (and (not (func-node? %))
                                                   (graph/contains-path? graph % required-output-node)) 
                                              (and (func-node? %)
                                                   (not (contains? non-full-fn-inputs %))
                                                   (not (contains? (inputs model %) required-output-node))
                                                   (not (contains? input-keys (output model %)))
                                                   (graph/contains-path? graph % required-output-node)))
                                         graph)
        graph-reachable (graph/remove-unreachable-nodes 
                          (remove-bad-paths filtered-graph required-output-node)
                          (map #(var-node-by-key model %) input-keys))
        result-graph (graph/filter-bfs 
                       #(or (not (func-node? %))
                            (= (count (graph/input-nodes graph-reachable %)) (count (graph/input-nodes graph %))))
                       graph-reachable)]    
    (make-model-from-graph result-graph)))




  
  
  
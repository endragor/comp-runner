(ns comp-runner.test.graph
  (:use comp-runner.graph
        clojure.test))

(deftest test-create
  (let [empty-graph (make-graph)]
    (is (graph-empty? empty-graph))))

(deftest test-add-node
  (let [graph (add-node (make-graph) "foo")]
    (is (contains-node? graph "foo"))))

(deftest test-remove-node
  (let [graph (remove-node (add-node (make-graph) "foo") "foo")
        graph (remove-node (add-edge (make-graph) "foo" "bar") "foo")]
    (is (not (contains-node? graph "foo")))
    (is (not (contains-edge? graph "foo" "bar")))
    (is (contains-node? graph "bar"))))

(deftest test-add-edge
  (let [graph (add-edge (make-graph) "foo" "bar")]
    (is (contains-node? graph "foo"))
    (is (contains-node? graph "bar"))
    (is (contains-edge? graph "foo" "bar"))
    (is (not (contains-edge? graph "bar" "foo")))
    (is (not (contains-edge? graph "baz" "baz")))))

(deftest test-remove-edge
  (let [graph (remove-edge (add-edge (make-graph) "foo" "bar") "foo" "bar")]
    (is (not (contains-edge? graph "foo" "bar")))
    (is (contains-node? graph "foo"))
    (is (contains-node? graph "bar"))))

(deftest test-contains-path
  (let [graph (reduce #(apply add-edge %1 %2) (make-graph) #{[1 2] [2 3] [3 4] [8 9]})]    
    (is (contains-path? graph 1 1))
    (is (contains-path? graph 1 2))
    (is (contains-path? graph 1 4))
    (is (not (contains-path? graph 1 8)))
    (is (not (contains-path? graph 10 10)))))

(deftest test-filter
  (let [graph (reduce #(add-node %1 %2) (make-graph) #{1 2 3 4 5 6})
        filtered-graph (filter-bfs #(> % 3) graph)]
    (is (every? #(contains-node? filtered-graph %) #{4 5 6}))))

(deftest test-reduce
  (let [graph (reduce #(add-node %1 %2) (make-graph) #{1 2 3 4 5 6})
        node-set (reduce-bfs #(conj %1 %2) #{} graph)]
    (is (= node-set #{1 2 3 4 5 6}))))

(deftest test-nodes
  (let [graph (reduce #(add-node %1 %2) (make-graph) #{1 2 3 4 5 6})]
    (is (= (nodes graph) #{1 2 3 4 5 6}))
    (is (= (count (nodes integer? graph)) 6))
    (is (empty? (nodes fn? graph)))))
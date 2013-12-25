(ns comp-runner.test.core
  (:use comp-runner.core
        clojure.test))

(defn tri-alpha {:computes :alpha} [{:keys [beta gamma]}]
  (- 180 beta gamma))

(defn tri-beta {:computes :beta} [{:keys [alpha gamma]}]
  (- 180 alpha gamma))

(defn tri-gamma {:computes :gamma} [{:keys [alpha beta]}]
  (- 180 alpha beta))

(defn tri-d1 {:computes :hx} [{:keys [y gamma]}]
  (* y (-> gamma Math/toRadians Math/sin)))

(defn tri-d2 {:computes :hz} [{:keys [x beta]}]
  (* x (-> beta Math/toRadians Math/sin)))

(defn tri-d3 {:computes :hy} [{:keys [z alpha]}]
  (* z (-> alpha Math/toRadians Math/sin)))

(defn tri-f1 {:computes :s} [{:keys [hx x]}]
  (* x (/ hx 2)))

(defn tri-f2 {:computes :s} [{:keys [hz z]}]
  (* z (/ hz 2)))

(defn tri-f3 {:computes :s} [{:keys [hy y]}]
  (* y (/ hy 2)))

(defn tri-g {:computes :s} [{:keys [x y z]}]
  (let [p (+ x y z)]
    (Math/sqrt (* p (- p x) (- p y) (- p z)))))

(defn tri-b {:computes :p :weight 1} [{:keys [x y z]}]
  (+ x y z))

(defn is-almost? [a b]
  (< (Math/abs (- a b)) 0.000001))

(deftest triangle-test
  (let [triangle-model (make-model tri-beta tri-d1 tri-d2 tri-d3 tri-f1 tri-f2 tri-f3 tri-g tri-b)
        test1 (compute triangle-model :s {:x 1 :z 2 :alpha 90 :gamma 60})
        test2 (compute triangle-model :s {:x 6 :hx 7})]
    (is (is-almost? test1 0.5))
    (is (= test2 (tri-f1 {:x 6 :hx 7})))
    (is (thrown-with-msg? IllegalArgumentException #"Not enough data to calculate the output" (compute triangle-model :s {})))
    (is (thrown-with-msg? IllegalArgumentException #"Not enough data to calculate the output" (compute triangle-model :s {:x 6})))))

; (defn arr-map {:computes :arr-out, :type :massive-map}
;   [{:keys [arr-in]}]
;   (+ arr-in 1))

; (deftest array-test
;   (let [arr-model (make-model arr-map)
;         test1 (compute arr-model [:arr-out 5] {:arr-in {5 5}})]
;     (is (= test1))))

(ns comp-runner.core
  (:require [comp-runner.comp-model :as model])
  (:require [clojure.set :as set]))

(defmacro make-model 
  "Create computational model out of a number of functions accepting maps and
  having :returns key in metadata with a keyword value representing a name of 
  model object returned by the function"
  [& func-syms]
  (let [triplets (map 
                   (fn [func]
                     (let [metadata `(meta (var ~func))
                           returns `(:computes ~metadata)
                           takes `(map keyword (-> ~metadata :arglists first first :keys))]
                       [func returns takes])) func-syms)]
    `(reduce 
       (fn [model# [func# returns# takes#]]
         (let [func-node# (model/make-func-node {:func func#})]
           (reduce (fn [model# input#] (model/add-dependency model# (model/make-var-node {:value input#}) func-node#))
                   (model/add-dependency model# func-node# (model/make-var-node {:value returns#}))
                   takes#)))
       ~(model/make-model)
       (list ~@triplets))))

(defn- ready-fns-from-statuses [fn-statuses]
  (reduce #(if (= (:computed (val %2)) (:total (val %2)))
             (conj %1 (key %2))
             %1) 
          #{}
          fn-statuses))

(declare compute-fn compute-fns)

(defn- compute-fn [ready-fn-node model ready-input-map-ref fn-statuses-ref]
  (if (contains? @ready-input-map-ref (model/node-value (model/output model ready-fn-node)))
    @ready-input-map-ref
    (let [fn-result ((model/node-func ready-fn-node) @ready-input-map-ref)
          output-variable-name (model/node-value (model/output model ready-fn-node))
          related-fns (model/requiring-funcs model output-variable-name)]      
      (dosync
        (commute ready-input-map-ref assoc output-variable-name fn-result)
        (alter fn-statuses-ref 
                 (fn [fn-statuses]
                   (reduce 
                     (fn [acc cur-fn]
                       (if (contains? acc cur-fn)
                         (update-in acc [cur-fn :computed] inc)
                         acc)) fn-statuses related-fns))))
      (let [ready-fn-nodes (ready-fns-from-statuses @fn-statuses-ref)]
        (dosync
          (alter fn-statuses-ref (fn [fn-statuses] (apply dissoc fn-statuses ready-fn-nodes))))
          (compute-fns ready-fn-nodes model ready-input-map-ref fn-statuses-ref)))))

(defn- compute-fns [ready-fn-nodes model ready-input-map-ref fn-statuses-ref]
  (let [futures (doall 
                  (for
                    [ready-fn-node ready-fn-nodes]
                    (future (compute-fn ready-fn-node model ready-input-map-ref fn-statuses-ref))))]
    (doseq [future futures] (deref future))))

(defn- do-compute [model output input-map]
  (let [fn-nodes (model/func-nodes model)
        inputs (set (keys input-map))
        fn-statuses (reduce 
                      (fn [acc cur-fn]
                        (let [fn-inputs (model/inputs model cur-fn)]
                          (assoc acc cur-fn {:computed (count (set/intersection 
                                                                (set (map #(model/var-node-by-key model %) inputs))
                                                                fn-inputs))
                                             :total (count fn-inputs)}))) {} fn-nodes)
        ready-fns (ready-fns-from-statuses fn-statuses)
        fn-statuses-ref (ref (apply dissoc fn-statuses ready-fns))
        ready-input-map-ref (ref input-map)]
    (compute-fns ready-fns model ready-input-map-ref fn-statuses-ref)
    (output @ready-input-map-ref)))

(defn compute
  "Computes value of output (keyword) out of the map (keyword->value) of known model objects.
  Throws IllegalArgumentException when there is not enough data to calculate the output."
  [model output input-map]
  (assert (keyword? output))
  (assert (every? keyword? (keys input-map)))
  (let [clean-model (model/clean model input-map output)]
    (when (empty? (model/func-nodes clean-model))       
      (throw (IllegalArgumentException. "Not enough data to calculate the output")))
    (do-compute (model/make-calculation-model clean-model (set (keys input-map)) output) output input-map)))
(ns hayek.strategies
  (:use [hayek.utils]
        [hayek.ta-lib]))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Helper Functions  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defn BS->BSH [BS]
;;   (map #(apply conj (vector (abs (apply min %)))
;;                (map (partial - (apply min %)) %)) BS))


;; (defn row-stochastic [coll]
;;   (map (fn [row] (map (fn [idx] (/ idx (apply + row))) row)) coll))


;; (defn equate-atom-lengths [& atoms]
;;   (let [m (apply min (map (comp count deref) atoms))]
;;     (doseq [a atoms]
;;       (swap! a (partial take-last m)))))


;; (defn one-hot
;;   [n coll]
;;   (let [emp (apply vector (repeat n 0))]
;;     (map #(assoc emp % 1) coll)))

(do 
(defn crosses
  "finds the crosses of two vectors"
  [alpha beta]
  (loop [return [0]
         rel (if (> (first alpha) (first beta)) 1 2)
         alpha (rest alpha)
         beta (rest beta)]
    (if (= 0 (count alpha))
      return
      (if (> (first alpha) (first beta))
        (if (= rel 1)
          (recur (conj return 0) 1 (rest alpha) (rest beta))
          (recur (conj return 1) 1 (rest alpha) (rest beta)))
        (if (= rel 2)
          (recur (conj return 0) 2 (rest alpha) (rest beta))
          (recur (conj return 2) 2 (rest alpha) (rest beta)))))))


(defn diff [vec]
  (loop [rem vec
         dif [0]]
    (if (= 1 (count rem))
      dif
      (recur (rest rem) (conj dif (- (second rem) (first rem)))))))


(defn sign [vec]
  (map #(cond (> 0 %) -1 (< 0 %) 1 :else 0) vec))


(defn sign-change [vec]
  (loop [res [0]
         vec vec]
    (if (= 1 (count vec))
      res
      (recur (conj res (cond (and (> (second vec) 0)
                                  (< (first vec) 0)) 1
                             (and (< (second vec) 0)
                                  (> (first vec) 0)) 2
                             :else 0))
             (rest vec)))))


(defn which-greater
  "returns which one is greater"
  [alpha beta]
  (map #(cond (> (nth alpha %) (nth beta %)) 1
              (< (nth alpha %) (nth beta %)) 2
              :else 0) (range (count alpha))))

(defn map_>=
  "returns true where > c"
  [coll c]
  (map #(>= % c) coll))

(defn map_<=
  "returns true where > c"
  [coll c]
  (map #(<= % c) coll))

(defn map_=
  "returns true where > c"
  [coll c]
  (map #(= % c) coll))

(defn map_NOT
  "returns (not %) onto coll"
  [coll]
  (map #(not %) coll))
 
(defn nth_>=
  "returns true where alpha > beta"
  [alpha beta]
  (map #(>= (nth alpha %) (nth beta %)) (range (count alpha))))

(defn nth_<=
  "returns true where alpha > beta"
  [alpha beta]
  (map #(<= (nth alpha %) (nth beta %)) (range (count alpha))))

(defn nth_=
  "returns true where alpha > beta"
  [alpha beta]
  (map #(= (nth alpha %) (nth beta %)) (range (count alpha))))

(defn nth_AND
  "returns true where (and alpha beta). 
   NOTE: responsible for returning with length of shorter indicator"
  [alpha beta]
  (let [len (min (count alpha) (count beta))
        alpha (take-last len alpha)
        beta (take-last len beta)]
    (map #(and (nth alpha %) (nth beta %)) (range (count alpha)))))

(defn nth_OR
  "returns true where (or alpha beta)
   NOTE: responsible for returning with length of shorter indicator"
  [alpha beta]
  (let [len (min (count alpha) (count beta))
        alpha (take-last len alpha)
        beta (take-last len beta)]
    (map #(or (nth alpha %) (nth beta %)) (range (count alpha)))))

(defn binned
  "returns integers corresponding to bins"
  [coll knots]
  (map (fn [obs] (or (first (filter #(> (nth knots %) obs)
                                    (range (count knots))))
                     (count knots)))
       coll))

(defn get-random-gate
  [gates]
  (rand-nth gates))

(defn get-AND-gate [gates] (first gates))
(defn get-OR-gate [gates] (second gates))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Strategy fragments  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def indicators
  [{:name "DI"
    :params [['timeperiod 14]]
    :data ['DI+ 'DI-]
    :methods [{:desc "which-greater"
               :fn '(fn [pos neg] (which-greater pos neg))
               :type :flag
               :range [1 2]}
              {:desc "which-crossed"
               :fn '(fn [pos neg] (crosses pos neg))
               :type :flag
               :range [1 2]}
              ]}
   {:name "ADX"
    :params [['timeperiod 14]]
    :data ['ADX]
    :methods [{:desc "which-bin"
               :fn '(fn [d] (binned d [15 20 25 30]))
               :type :scale
               :range [0 1 2 3 4]}
              ]}
   {:name "MACD"
    :params [['fastperiod 12] ['slowperiod 26] ['signalperiod 9]]
    :data ['MACD]
    :methods [{:desc "which-crossed"
               :fn '(fn [macd signal hist] (crosses signal macd))
               :type :flag
               :range [1 2]
               }
              {:desc "sign of angle"
               :fn '(fn [macd signal hist] (sign (diff hist)))
               :type :flag
               :range [-1 1]
               }
              ]}
   {:name "SAR"
    :params [['acceleration 0] ['maximum 0]] 
    :data ['SAR]
    :methods [{:desc "above/below price"
               :fn '(fn [period sar]
                      (let [open (take-last (count sar) (map (fn [r] (nth r 1)) period))]
                        (sign (map (fn [n] (- (nth open n) (nth sar n)))
                                              (range (count sar))))))
               :period true
               :type :flag
               :range [-1 1]
               }
              {:desc "crossed-price"
               :fn '(fn [period sar]
                      (let [open (take-last (count sar) (map (fn [r] (nth r 1)) period))]
                        (crosses open sar)))
               :period true
               :type :flag
               :range [1 2]
               }
              ]}
   {:name "CMO"
    :params [['timeperiod 14]]
    :data ['CMO]
    :methods [{:desc "below/within/above -50/50"
               :fn '(fn [d] (binned d [-50 50]))
               :type :flag
               :range [0 1 2]
               }
              ]}
   {:name "ADL"
    :params [['fastperiod 9] ['slowperiod 16]]
    :data ['ADL]
    :methods [{:desc "sign"
               :fn 'sign
               :type :flag
               :range [-1 1]
               }
              {:desc "sign-change"
               :fn 'sign-change
               :type :flag
               :range [1 2]
               }
              ]}
   {:name "Aroon"
    :params [['timeperiod 20]]
    :data ['Aroon]
    :methods [{:desc "one above and other below 50"
               :fn '(fn [pos neg upper lower]
                      (map (fn [n]
                             (cond (and (> (nth pos n) upper) (< (nth neg n) lower)) 1
                                   (and (> (nth neg n) upper) (> (nth pos n) lower)) 2
                                   :else 0)) (range (count pos))))
               :params [['upper 50] ['lower 50]]
               :type :flag
               :range [1 2]
               }
              ]}
   {:name "CCI"
    :params [['timeperiod 20]]
    :data ['CCI]
    :methods [{:desc "reversal at extreme"
               :fn '(fn [cci thresh]
                      (loop [res [0]
                             cci cci]
                        (if (= 1 (count cci)) res
                          (recur (conj res (cond (and (> (first cci) thresh)
                                                      (< (second cci) thresh)) 2
                                                 (and (< (first cci) (* -1 thresh))
                                                      (> (second cci) (* -1 thresh))) 1
                                                 :else 0)) (rest cci)))))
               :params [['thresh 100]]
               :type :flag
               :range [1 2]
               }
              ]}
   ;; {:name "RSI"
    ;; :params [['timeperiod 14]]
    ;; :data ['RSI]
    ;; }
   ])

;; ;; Just a reminder
;; (def indicators
;;   X {:fn 'SAR
;;   ?  :role :trailing_stop}
;;    {:fn 'Keltner
;;     :role :band} ; error
;;    {:fn 'ATR
;;     :role :volatility}
;;    {:fn 'ROC
;;     :role :trend_confirmation}
;;    {:fn 'StDev
;;     :role :volatility}
;;    {:fn 'RSI
;;     :role :buying_pressure}

(def gates
  [{:name :AND
    :num-inp 2
    :fn 'nth_AND}
   {:name :OR
    :num-inp 2
    :fn 'nth_OR}
   ;; {:name :NOT
    ;; :num-inp 1
    ;; :fn 'map_NOT}
   ;; {:name :BUT-NOT
    ;; :num-inp 2
    ;; :fn '(and %1 (not %2))}
   ])


(def protocols
  {:flag [{:desc "flag_="
           :fn 'map_=
           :param-fns ['(fn [range] (rand-nth range))]}]
   :bool [{:desc "is-true"
           :fn '%
           :param-fns []}]
   :scale [{:desc "scale_="
            :fn 'map_=
            :param-fns ['(fn [range] (rand-nth range))]}
           {:desc "scale_>="
            :fn 'map_>=
            :param-fns ['(fn [range] (rand-nth (drop-last (drop 1 range))))]}
           {:desc "scale_<="
            :fn 'map_<=
            :param-fns ['(fn [range] (rand-nth (drop-last (drop 1 range))))]}]
   })



;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Strategy generator  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO:
;;   X When a new strategy is initiated, only combine with AND
;;   * Determine whether NOT is important, or if it can be acheived naturally
;;     + probably can be worked to produce same likelihood with less adverse effects.
;;   X New strategies should be combined in several ways at once
;;     + AND and OR gates on all or fragments of the trees
;;       - This should be done at the top-level. merge fn takes specified gate.
;;   * New strategies (and potentially old ones) should be optimized
;;     + should be done to all active strategies on current window
;;     + need to write a method that moves parameters towards optimal
;;       - Try n manipulations and choose greedily
;;   * Mutually exclusive strategies fragments should be weeded out
;;   * Parameters right now are pulled from normal distribution
;;     + when the default is 0 there needs to be a new procedure
;;   * Handle cases where fast MUST BE < slow
;;   * make random parameter option for METHODS, i.e. CCI and Aroon

;; Considerations:
;;   * To NOT or not to NOT?
;;     + to NOT:
;;       - running :scale on bands or equivalent might utilize NOT-within or NOT-outside
;;       - merging agnts of opposite directions
;;     + not to NOT:
;;       -
;;     + maybe use NOT only when combining opposing strategies



(defn random-fn-tree
  [params]
  
  ;;;  helper functions  ;;;
  (defn use-random-protocol
    [protocols method]
    (let [protocol (rand-nth (protocols (method :type)))]
      (assoc protocol :params (map #(% (method :range))
                                   (eval (protocol :param-fns))))))
  
  (defn get-random-indicator
    [indicators protocols]
    (let [indicator (rand-nth indicators)
          ;; pull integer value for each parameter from normal distribution
          data_args (map #(round-up (max 6 (rand-norm (second %)
                                                      (* (second %) 0.25))))
                         (indicator :params))
          method (rand-nth (indicator :methods))
          method_args (if (contains? method :params)
                        (into [] (map #(round-up (max 6 (rand-norm (second %)
                                                                   (* (second %) 0.25))))
                                      (method :params))))]
      {:name (indicator :name)
       :args data_args
       :data (indicator :data)
       :method (assoc method
                      :protocol (use-random-protocol protocols method)
                      :args method_args)
       }))

  ;;;  Body  ;;;
  (if (> (params :gate_likelihood) (rand))
    (let [gate (get-AND-gate gates)]
      (loop [fn-branch {:gate gate}
             i 0]
        (if (> (inc i) (gate :num-inp))
          fn-branch
          (recur (assoc-in fn-branch [:gate (keyword (str i))]
                        (random-fn-tree params))
                 (inc i)))))
    {:indicator (get-random-indicator indicators protocols)})
  )


(defn compile-fn-tree
  [tree]

  (defn recursive-process
    [tree]
    (if (contains? tree :indicator)
      (let [indicator (tree :indicator)
            method (indicator :method)
            protocol (method :protocol)
            data-fns (map #(conj (indicator :args) 'period %)
                          (indicator :data))
            data-fns (if (and (contains? method :period) (method :period))
                       (concat [['period]] data-fns)
                       data-fns) ;; some methods may utilize raw price
            data-fns (list 'apply 'concat (conj (into [] data-fns) (method :args)))
            ]
        (concat (list (protocol :fn) (list 'apply (method :fn) data-fns))
                (protocol :params)))
      (let [gate (tree :gate)
            args (into (list) (map #(recursive-process (gate (keyword (str %))))
                                   (range (gate :num-inp))))]
        (conj args (gate :fn))
        )))
  
  (into (list (recursive-process tree)) (list '[period] 'fn)))


(defn merge-fn-trees
  "Merge random branches of two trees under gate. Random gate if not specified."
  ([tree1 tree2 params] (merge-fn-trees tree1 tree2 (get-random-gate gates) params))
  ([tree1 tree2 gate params]
   (defn choose-node [tree params]
     (loop [res 0]
       (if (not= 0 res)
         res
         (if (> (params :node_likelihood) (rand))
           tree
           (recur (loop [tree tree]
                    (if (contains? tree :indicator)
                      (if (> (params :node_likelihood) (rand))
                        tree
                        0)
                      (let [nodes (shuffle [((tree :gate) :0)
                                            ((tree :gate) :1)])]
                        (cond (> (params :node_likelihood) (rand))
                              (first nodes)
                              (> (params :node_likelihood) (rand))
                              (second nodes)
                              :else (cond (contains? (first nodes) :gate)
                                          (recur (first nodes))
                                          (contains? (second nodes) :gate)
                                          (recur (second nodes))
                                          :else 0))))))))))
   {:gate (assoc gate :0 (choose-node tree1 params) :1 (choose-node tree2 params))}))

)

;;;;;;;;;;;;;;
;;;  test  ;;;
;;;;;;;;;;;;;;

;; (do
;;    (def periods (get-periods))
;;    (def period (first periods)))


;; (do
;;   (def params {:gate_likelihood 0
;;                :node_likelihood 0.7})
;;   (def tree (random-fn-tree params))
;;   (def tree1 (random-fn-tree params))
;;   (def tree2 (random-fn-tree params))
;;   (compile-fn-tree (merge-fn-trees tree1 tree2 params)))

;; ((eval (compile-fn-tree tree)) period)

;; (repeatedly 1000
;;             #(do
;;                (def tree (random-fn-tree params))
;;                ((eval (compile-fn-tree tree))period)))



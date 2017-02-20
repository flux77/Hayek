(ns hayek.core
  (:require [com.hypirion.clj-xchart :as c])
  (:use [hayek.ta-lib]
        [hayek.utils]
        [hayek.simulator]
        [hayek.strategies])
  (:gen-class))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Helper Functions  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(do
  
  (defn reset-agntlog [agntlog]
    (dosync
     (ref-set agntlog {:max_id -1
                       :agnts []}))
    nil)

  (defn save-agntlog
    [agntlog]
    (spit "resources/agnts/agnt.log" @agntlog))


  (defn load-agntlog []
    (ref (read-string (slurp "resources/agnts/agnt.log"))))


  (defn generate-agnt
    ([params] (generate-agnt params (random-fn-tree params)))
    ([params fn_tree]
     {:fn_tree fn_tree
      :fn (compile-fn-tree fn_tree)
      :wallet 1000
      :atr_candles 14
      :atr_dist (+ 1 (* 3 (rand)))
      :closing_fn 'trailing-stop-ATR
      :bid_fn 'only-long-bids
      :parents nil
      :archive []
      }))


  (defn log-agnt-performace
    [agnt tradelog agntlog]
    (if (not (some #{0} (map count tradelog)))
      (let [agnt_id (ref -1)
            num_trades (map count tradelog)
            risk (map #(abs (mean (filter (partial > 0) (map :gain %)))) tradelog)
            reward (map #(mean (filter (partial < 0) (map :gain %))) tradelog)
            num_won (map #(count (filter (partial < 0) (map :gain %))) tradelog)
            num_lost (map #(count (filter (partial > 0) (map :gain %))) tradelog)
            exposure (map #(cond (= 0 (nth num_won %)) 10
                                 (= 0 (nth num_lost %)) 1
                                 :else
                                 (/ (nth risk %) (nth reward %)
                                    (/ (nth num_won %) (nth num_lost %))))
                          (range (count tradelog)))]
        (if (= 0 (count exposure))
          (do (def tradelog tradelog)
              (def agnt agnt)
              (println exposure)
              -1)
          (do (dosync
               (let [id (inc (agntlog :max_id))]
                 (ref-set agnt_id id)
                 (alter agntlog assoc
                        :max_id id
                        :agnts (conj (@agntlog :agnts)
                                     {:id id
                                      :exposure exposure
                                      :num_trades num_trades}))))
              (save-agntlog agntlog)
              (spit (apply str "resources/agnts/" @agnt_id ".agnt")
                    {:agnt agnt
                     :id @agnt_id
                     :num_trades (map count tradelog)
                     :risk risk
                     :reward reward
                     :num_won num_won
                     :num_lost num_lost
                     :exposure exposure
                     })
              )))
      -1))

  
  (defn tradelog-from-results
    [agnt periods results]
    (loop [periods periods
           tradelog []]
      (let [bids ((eval (agnt :bid_fn)) agnt (first periods))
            trades (loop [trades []
                          results results
                          bids bids]
                     (if (= 0 (count results))
                       trades
                       (if (not= (first bids) [1 0 0])
                         (recur (conj trades (first results)) (rest results) (rest bids))
                         (recur trades (rest results) (rest bids)))))]
        (recur (rest periods) (conj tradelog trades)))))

  
  (defn simulate-all-periods
    [agnt periods agntlog params]
    (if (params :memoize_trades)
      ;; get results from simulating across all periods
      (let [results ((params :memo-trade-fn)
                     periods (agnt :atr_candles) (agnt :atr_dist) (agnt :bid_fn))
            tradelog (tradelog-from-results agnt periods results)]
        (if (params :log_performace)
          (log-agnt-performace agnt tradelog agntlog)
          tradelog))
      (loop [periods periods
             tradelog []]
        (if (= 0 (count periods))
          (if (params :log_performace)
            (log-agnt-performace agnt tradelog agntlog)
            tradelog)
          (if (and (< 0 (count tradelog)) (= 0 (count (last tradelog))))
            -1
            (let [[trades agnt] (simulate-period (first periods) agnt params)]
              (recur (rest periods) (conj tradelog trades))))))))


  (defn generate-and-sim
    [periods agntlog params]
    (let [agnt (generate-agnt params)]
      (simulate-all-periods agnt periods agntlog params)))


  (defn window-exposure
    "Not built for data-stream. Window assumes every agent tested up to current."
    ([n agntlog] (map #(window-exposure n (% :num_trades) (% :exposure))
                      (agntlog :agnts)))
    ([n num_trades exposure]
     (/ (apply + (map #(* (nth num_trades %) (nth exposure %))
                      (range (- (count exposure) n) (count exposure))))
        (apply + (take-last n num_trades)))))


  (defn roullete
    "Takes a vector of magnitudes and returns index n with proportionate likelihood"
    [coll]
    (let [num (count coll)
          val (rand)
          coll (map (partial * (/ 1 (apply + coll))) coll)]
      (loop [sum (first coll)
             coll (rest coll)]
        (if (> sum val)                    
          (dec (- num (count coll)))
          (recur (+ sum (first coll)) (rest coll))))))

  (defn load-agnt
    [id]
    (:agnt (read-string (slurp (apply str "resources/agnts/" id ".agnt")))))



  
  (defn select-random-agnt
    [agntlog params]
    (let [exposure (window-exposure (params :window_size) agntlog)
          id (roullete (power (map (partial / 1) exposure) 2))] ;; maybe ^2
      [id, (load-agnt id)])) 
  

  (defn merge-and-sim
    [periods agntlog params]
    (let [[id1 agnt1] (select-random-agnt agntlog params)
          [id2 agnt2] (select-random-agnt agntlog params)
          fn_tree (merge-fn-trees (agnt1 :fn_tree) (agnt2 :fn_tree) params)
          agnt (assoc (generate-agnt params fn_tree) :parents [id1 id2])]
      (simulate-all-periods agnt periods agntlog params)
      ))
        

  (defn all-trade-results
    [periods atr_candles atr_dist bid_fn]
    (let [agnt {:fn '(fn [period] (repeat (count period) true))
                :atr_candles atr_candles
                :atr_dist atr_dist
                :closing_fn 'trailing-stop-ATR
                :bid_fn bid_fn
                }
          params {:update_agnt false
                  :log_performace false
                  :every_candle true
                  }]
      (simulate-all-periods agnt periods nil params)))

  
  (defn crawl-fn-tree
    ([fn_tree fun] (crawl-fn-tree fn_tree fun [] []))
    ([fn_tree fun loc results]
     (let [node (reduce #(%1 %2) fn_tree loc)]
       (if (contains? node :gate)
         (loop [results results
                i (range ((node :gate) :num-inp))]
           (if (= 0 (count i))
             results
             (recur (apply conj results
                          (crawl-fn-tree fn_tree fun
                                         (conj loc :gate
                                               (keyword (str (first i)))) results))
                    (rest i))))
         [[loc, (fun (node :indicator))]]))))

  
  ;; (defn optimize-fn-tree
    ;; "takes an agent and runs one round of a greedy optimization."
    ;; [periods agnt params]

    ;; (let [params (assoc params :log_performance true)
          ;; fun ]
    
  ;; (fn 

    ;; (def node ((random-fn-tree params) :indicator))
    ;; ))

  
  )
  

;;;;;;;;;;;;;;
;;;  Main  ;;;
;;;;;;;;;;;;;;

;; TODO:
;;   * run/balance multiple modes of agnt generation
;;     X generate new trading strategies
;;       - ratio: 0.5
;;     + merge most successful strategies
;;       - ratio: 0.25
;;     + mutate old strategies
;;       - ratio: 0.10
;;     + optimize old strategies
;;       - ratio: 0.15
;;       - note: if we save multiple, we're increasing likelihood of family exploding
;;         - might be okay
;;   * Run long & short on every candle in every period at several ATR-distances
;;     + will allow each agent to be tested in much shorter time
;;     + pass the expanded results as parameter to closing function
;;       - :every_candle = true/false to determine what index should be returned
;;   * Memoize each function component as well (harder)
;;   * make max_depth for random agnt generation
;;   * stop simulation on agnt if no trades in first period
;;   * 
    



(defn -main
  [& args]
  
  (do
    (def periods (get-periods))
    (def period (first periods))
    (def agntlog (load-agntlog))
    (def params {:update false
                 :log_performace true
                 :leverage 100
                 :gate_likelihood 0.4
                 :node_likelihood 0.7
                 :every_candle false
                 :memoize_trades false
                 :window_size 17
                 :memo-trade-fn (resetable-memoize all-trade-results)
                 })
    )
  (do
    (def new_workers (n-serial-workers 16 'generate-and-sim periods agntlog params))
    (def merge_workers (n-serial-workers 8 'merge-and-sim periods agntlog params)))
  
  (do
    (kill-workers new_workers)
    (kill-workers merge_workers))

  (generate-and-sim periods agntlog params)
  (merge-and-sim periods agntlog params)
  
  )

(do
  (def a agnt)
  (def t tradelog))

(reset-agntlog agntlog)








;; (def chart1
;;          (c/xy-chart {"DI+" [(range 50) (take 50 (first bull))]
;;                       "DI-" [(range 50) (take 50 (first bear))]}))
;; (def chart2
;; (c/xy-chart {"Close" [(range 50)
;;                       (take 50 (drop 13 (first (makeOHLC period "C"))))]}))

;; (c/view chart1 chart2)




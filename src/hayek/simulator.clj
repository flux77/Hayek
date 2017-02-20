(ns hayek.simulator
  (:use [hayek.utils]
        [hayek.ta-lib]
        [hayek.strategies]))


;;;;;;;;;;;;;;;;;;;;;;
;;;;  Simulator   ;;;;
;;;;;;;;;;;;;;;;;;;;;;

(defn simulate-period
  "Checks buy/sell/hold decision and simulates trades for one period"
  [period agnt params]

  ;;;; helper functions 
  (defn gain-on-trade [trade params]
    (if (= 1 (trade :position))
      (let [init (+ (trade :init_price) (trade :init_spread))
            stop (trade :stop_price)]
        (assoc trade :gain (/ (- stop init) init)))
      (let [init (trade :init_price)
            stop (+ (trade :stop_price) (trade :stop_spread))]
        (assoc trade :gain (/ (- init stop) stop)))))

  (defn update-agnt [agnt trade params]
    (if (params :update_agnt)
      (let [gain (* (params :leverage) (trade :gain)
                    (trade :trade_size) (agnt :wallet))]
        ;; (println (str "Measured gain: " gain))
        (assoc agnt
               :wallet (+ (agnt :wallet) gain)))
      agnt))

  ;;;; body
  (let [bids ((eval (agnt :bid_fn)) agnt period)
        atr (first (ATR period (agnt :atr_candles)))]
    (loop [remaining period
           agnt agnt
           bids bids
           trades []
           index 0]
      ;; Case for end of period
      ;; (println index)
      (if (<= (count remaining) 1)
        [trades agnt]
        ;; Scale each bid by the size of the agnt's wallet
        (let [position (which-max (first bids))]
          (if (= position 0)
            (recur (rest remaining) agnt (rest bids) trades (inc index))
            (let [trade {:init_index index
                         :init_spread (nth (first remaining) 6)
                         :init_price (nth (first remaining) 4)
                         :trade_size 1
                         :bids (first bids)
                         :position position}
                  ;; Run the closing strategy until trade terminates
                  [remaining bids trade index]
                  ((eval (agnt :closing_fn)) remaining agnt bids trade params atr)
                  trades (conj trades (gain-on-trade trade params))
                  ;; emp (println "trade-updated")
                  agnt (update-agnt agnt (last trades) params)]
              (recur (rest remaining) agnt (rest bids) trades (inc index)))))))))



;;;;;;;;;;;;;;;;;;;;;;
;;;  Stop methods  ;;;
;;;;;;;;;;;;;;;;;;;;;;

(defn trailing-stop-ATR
  "Manages a trailing stop. Initialized at :num_devs ATR from start price"
  [remaining agnt bids trade params atr]

  (defn initialize-trail
    [agnt trade tr]
    (let [dist (* tr (agnt :atr_dist))]
      (if (= (:position trade) 1)
        (assoc trade :trail_dist dist
               :trail_pos (- (trade :init_price) dist))
        (assoc trade :trail_dist dist
               :trail_pos (+ (trade :init_price) dist)))))
  
  (defn close-trade
    [remaining trade index]
    (assoc trade :stop_index (+ (trade :init_index) index)
           :stop_spread (nth (nth remaining index) 6)
           :stop_price (trade :trail_pos)))

  (defn update-trade
    [remaining trade index]
    (assoc trade :trail_pos
           (if (= 1 (trade :position))
             (max (trade :trail_pos)
                  (- (nth (nth remaining index) 2)
                     (trade :trail_dist)))
             (min (trade :trail_pos)
                  (+ (nth (nth remaining index) 3)
                     (trade :trail_dist))))))
  
  (defn return
    [remaining bids trade index params]
    (if (params :every_candle)
      (let [trade (close-trade remaining trade index)
            index (trade :init_index)]
        [remaining bids trade index])
      (let [bids (drop index bids)
            trade (close-trade remaining trade index)
            remaining (drop index remaining)
            index (trade :stop_index)]
        [remaining bids trade index])))

  
  ;;; Body
  (let [tr (nth atr (max 0 (- (trade :init_index) (agnt :atr_candles))))
        trade  (initialize-trail agnt trade tr)]
    ;; note, these should include the case where close != open
      ;; because the current assumption inaccurately trims losses
    ;; note, this won't account for growth in profit before stop in large candle
      ;; i.e. if the high  goes above trail_dist before dropping to the low
    (loop [trade trade
           index 0]
      ;; (println (trade :position) (trade :trail_pos))
      ;; (println (count remaining) (inc index))
      (if (= (inc index) (count remaining))
        ;; end of period, close trade
        (return remaining bids trade index params)
        (if (= (:position trade) 1)
          ;; Long
          (if (< (nth (nth remaining index) 3) (:trail_pos trade))
            (return remaining bids trade index params)
            (recur (update-trade remaining trade (inc index)) (inc index)))
          ;; Short
          (if (> (nth (nth remaining index) 2) (:trail_pos trade))
            (return remaining bids trade index params)
            (recur (update-trade remaining trade (inc index)) (inc index))))))
    ))



;;;;;;;;;;;;;;;;;;;;;
;;;  Bid Methods  ;;;
;;;;;;;;;;;;;;;;;;;;;

(defn only-long-bids
  [agnt period]
  (let [bids (map #(if % [0 1 0] [1 0 0]) ((eval (agnt :fn)) period))]
   (into (into [] (repeat (- (count period) (count bids)) [1 0 0])) bids)))

(defn only-short-bids
  [agnt period]
  (let [bids (map #(if % [0 0 1] [1 0 0]) ((eval (agnt :fn)) period))]
    (into (into [] (repeat (- (count period) (count bids)) [1 0 0])) bids)))


(defn symmetrical-bids
  [agnt period]
  ()
  )



;;;;;;;;;;;;;;
;;;  Test  ;;;
;;;;;;;;;;;;;;

;; (do
;;   (def periods (get-periods))
;;   (def period (first periods))
;;   (def params {:mode :test
;;                :leverage 100
;;                :gate_likelihood 0.4
;;                }))

;; (do
;;   (defn generate-agnt
;;     ([params] (generate-agnt params (random-fn-tree params)))
;;     ([params fn_tree]
;;      {:fn_tree fn_tree
;;       :fn (compile-fn-tree fn_tree)
;;       :wallet 1000
;;       :atr_candles 14
;;       :atr_dist (+ 1 (* 3 (rand)))
;;       :closing_fn 'trailing-stop-ATR
;;       :bid_fn 'only-long-bids
;;       :parents nil
;;       :archive []
;;       }))
;;   (def agnt (generate-agnt params))
;;   (simulate-period period agnt params))


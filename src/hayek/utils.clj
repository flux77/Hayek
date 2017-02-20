(ns hayek.utils
  (:import [java.util Random]))


;;;;;;;;;;;;;;;;;;;;;;
;;;  Random tools  ;;;
;;;;;;;;;;;;;;;;;;;;;;

(defn reduce-by-index
  ([coll] (reduce-by-index '+ 0 coll))
  ([fun coll] (reduce-by-index 'fun 0 coll))
  ([fun val coll]
   (map #(reduce (fn [val item] (fun val (nth item %)))
                 val coll)
        (range 3))))

(defn power [base exp] (apply * (repeat base exp)))

(defn stdev [coll]
  (defn squares [avg coll] (map #(power (- % avg) 2) coll))
  (let [avg (mean coll)
        squares (squares avg coll)
        total (count coll)]
    (Math/sqrt (/ (reduce + squares) (- total 1)))))


(defn round-up [val] (int (Math/ceil val)))


(defn which-max [coll]
  (first (apply max-key second (map-indexed vector coll))))


(defn which-min [coll]
  (first (apply min-key second (map-indexed vector coll))))


(defn update-last [coll & kvs]
  "takes a list of maps followed by key-value pairs"
  (update-in coll [(dec (count coll))]
             #(eval (conj kvs % 'assoc))))


(defn first-index-of "For each item in sub, return index in coll"
  [sub coll]
  (map (fn [s] (first (keep-indexed #(if (= s %2) %1) coll))) sub))


(defn abs [n] (max n (- n)))

(defn mean
  [coll] (if (= 0 (count coll))
                         nil
                         (/ (apply + coll) (count coll))))

(defmacro bench
  "Times the execution of forms, discarding their output and returning
a long in nanoseconds."
  ([& forms]
   `(let [start# (System/nanoTime)]
       [~@forms (/ (- (System/nanoTime) start#) 1000000000.)])))


(defn resetable-memoize
  "Call (<fn> :reset!) to clear the cache"
  [f]
  (let [mem (atom {})]
    (fn [& args]
      (if (= (first args) :reset!)
        (reset! mem {})
        (if-let [e (find @mem args)]
          (val e)
          (let [ret (apply f args)]
            (swap! mem assoc args ret)
            ret))))))

(defn rand-norm 
  ([] (rand-norm 0.0 1.0))
  ([mu sigma]
   (+ mu (* sigma (.nextGaussian (Random.))))))





;;;;;;;;;;;;;;;;;;;;;
;;;  Concurrency  ;;;
;;;;;;;;;;;;;;;;;;;;;

(defn worker
  [fun & args]
  (let [alive (ref true)]
    {:future (future
               (loop []
                 (if @alive
                   (do
                     (apply fun args)
                     (recur))
                   "DEAD")))
    :alive alive}))


(defn kill-workers
  [workers]
  (if (not= (type {}) (type workers))
    (map kill-workers workers)
    (let [alive (workers :alive)]
      (dosync
       (ref-set alive false)))))


(defn n-serial-workers
  "'fun' must be quoted"
  [n fun & args]
  
  (defn update-mean-time
    [mean_time t]
    (dosync (ref-set mean_time
                     [(inc (first @mean_time))
                      (/ (+ (* (first @mean_time)
                               (second @mean_time)) t)
                         (inc (first @mean_time)))])))
  
  (let [mean_time (ref [0 0])]
    (doall (repeatedly n #(worker (fn [fun args mean_time fn_name]
                                    (let [[res t] (bench (apply (eval fun) args))]
                                      (if (not= res -1)
                                        (do
                                          (update-mean-time mean_time t)
                                          (if (= 0 (rem (first @mean_time) 10))
                                            (do
                                              (println fn_name "x" n "workers")
                                              (println "   iterations:" (first @mean_time))
                                              (println "   mean time:"
                                                       (/ (second @mean_time) n)
                                                       "seconds")))))))
                                  fun args mean_time (str fun)
                                  )))
    ))



;;;;;;;;;;;;;;;;;;;;;
;;;  Data import  ;;;
;;;;;;;;;;;;;;;;;;;;;

(defn my-read-string [val]
  (if (= val "") "" (read-string val)))


(defn parse
  "Convert the CSV into rows of columns"
  [string]
  (let [split_string (map #(clojure.string/split % #",") 
                          (clojure.string/split string #"\n"))]
    (apply conj [(first split_string)]
          (map #(apply conj [(first %)] (into [] (map read-string (drop 1 %))))
               (rest split_string)))))


(defn read-data [path]
  (parse (slurp path)))


(defn get-period
  "At the end of each row is a 0/1 that signifies the end of a
   trading period. Return first period and the rest of the data"
  [data]
  (loop [period [(first data)]
         data (rest data)]
    (if (= 1 (last (last period)))
      [period data]
      (recur (conj period (first data)) (rest data)))))


(defn split-periods
  "Split into periods to train on. See get-period."
  [data]
  (loop [periods []
         remaining data]
    (if (> (count remaining) 0)
      (let [[period remaining] (get-period remaining)]

        (recur (conj periods period) remaining))
      periods)))

(defn get-periods
  []
  (loop [periods []
         period []
         bid (rest (read-data "resources/data/EURUSD_1_m_BID.csv"))
         ask (rest (read-data "resources/data/EURUSD_1_m_ASK.csv"))]
    (if (= 0 (count bid))
      periods
      (if (and (= 0 (nth (first bid) 5)) (= 0 (count period))) ;; clear zeros 
        (recur periods period (rest bid) (rest ask))
        (let [row (conj (first bid) (- (nth (first ask) 4) (nth (first bid) 4)))
              period (conj period row)]
          (if (= 0 (apply + (map #(nth % 5) (take 10 (rest bid)))))
            (recur (conj periods period) [] (rest bid) (rest ask))
            (recur periods period (rest bid) (rest ask))))))))




(ns hayek.ta-lib
  (:use [clj-ta-lib.core]
        [hayek.utils]))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Helper functions  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn trim
  "Takes a vector of arrays with a :begIndex parameter and drops from each."
  ([data]
   (let [trim ((comp :begIndex meta) data)
         trimmed (map #(drop-last trim %) data)]
     trimmed)))
     ;; (if (= 1 (count trimmed)) (first trimmed) trimmed))))


(defn makeOHLC
  "Takes rows of columns of DOHLCSVt. 
   Default returns a PriceHolder object with OHLCV. 
   With string param returns a list with one double-array corresponding to 
     each letter in the param string. 
  default \"HLC\"  
  [D(ate) O(pen) H(igh) L(ow) C(lose) S(pread) V(olume) t(erminal)]"
  ([data] [(make-price-holder (makeOHLC data "OHLCV"))])
  ([data params]
    (map double-array (map
      #(map (fn [row] (nth row %)) data)
      (first-index-of (clojure.string/split params #"")
                      ["D" "O" "H" "L" "C" "S" "V" "t"])
      ))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Indicator Definitions  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Average Directional Index (ADX)
;; ADX measures the strength or weakness of a trend, not the direction
;; +DI > -DI : bullish
;; -DI > +DI : bearish
;; Many suggest ADX of 20 as trending, Wilder used 25
;; It can take around 150 periods of data to get true ADX values.
;; frequent whipsaws
(defn ADX
  ([data] (ADX data 14))
  ([data timeperiod]
   (trim (ta "adx" (makeOHLC data) timeperiod))))
(defn DI+
  ([data] (DI+ data 14))
  ([data timeperiod]
   (trim (ta "plus_di" (makeOHLC data) timeperiod))))
(defn DI-
  ([data] (DI- data 14))
  ([data timeperiod]
   (trim (ta "minus_di" (makeOHLC data) timeperiod))))


;;;; Bollinger Bands (BBANDS)
;; ** err **
(defn BBANDS
  ([data] (BBANDS data 5 2 2 0))
  ([data timeperiod nbdevup nbdevdn matype]
   (trim (ta "bbands" (makeOHLC data "C") timeperiod nbdevup nbdevdn matype))))


;;;; Parabolic SAR (SAR)
;; acts as a trailing stop / trend change signal
;; two modes, SAR up and down. Up sits below price and vice-versa.
;; When the price crosses the SAR, the trend has ended and it switches
;; trends occur apx. 30% of the time, and whipsaws apx. 50%
;; Higher maximum, more sensitive.
(defn SAR
  ([data] (SAR data 0 0))
  ([data acceleration maximum]
   (trim (ta "sar" (makeOHLC data) acceleration maximum))))


;;;; Chande Momentum Oscillator (CMO)
;; Overbought/oversold when over/under +50/-50
(defn CMO
  ([data] (CMO data 14))
  ([data timeperiod]
   (trim (ta "cmo" (makeOHLC data "C") timeperiod))))


;;;; Accumulation/Distribution Line (ADL)
;; Calculates a money-flow multiplier, then scales it by the volume
;; keeps a running sum divided by total volume => -1.0 to 1.0
;; Value signifies underlying buying pressure, can foreshadow trend
(defn ADL
  ([data]
   (ta "ad" (makeOHLC data)))
  ([data fastperiod slowperiod]
   (trim (ta "adosc" (makeOHLC data) fastperiod slowperiod))))


;;;; Keltner Channels
;; Set a band above and below simple moving average by factor of ADX
;; ** no command in wrapper **
;; use this type of techinique to establish a variable band system
;; maybe a percentage distance between the two:
;; ( (Upper Band - Lower Band) / Middle Band) * 100
;; this ^ alone would equate to something like c*ATR, so something else
;; standard deviation bands?
(defn Keltner
  ([data] (Keltner data 20 2))
  ([data timeperiod factor]
   (trim (ta "keltner" (makeOHLC data "C") timeperiod))))


;;;; Average True Range (ATR)
(defn ATR
  ([data] (ATR data 20))
  ([data timeperiod]
   (trim (ta "atr" (makeOHLC data) timeperiod))))



;;;; Aroon
;; when one is above 50 and the other is below 50, edge goes to the greater
(defn Aroon
    ([data] (Aroon data 20))
  ([data timeperiod]
   (trim (ta "aroon" (makeOHLC data) timeperiod))))


;;;; Commodity channel index
;; returns a value indicating the mean deviation from the SMA for all
;;   candles in the timeperiod
;; The default will return apx. 70-80% falling between -100 and 100
;; seems as though the signals are when a strong trend is reversed at some
;;   threshold. exx 100+ -> < 100 may signal bearish reversal 
(defn CCI
  ([data] (CCI data 20))
  ([data timeperiod]
   (trim (ta "cci" (makeOHLC data) timeperiod))))


;;;; Chaikin Money Flow (CMF)
;; same as ADL but using a sum of the last period of time, not accumulation
;; not in ta-lib


;;;; Moving Average Convegence/Divergence (MACD)
;; crossing of signal ema of MACD over MACD (long_ema-short_ema)
;; The angle between the two is of most importance
(defn MACD
  ([data] (MACD data 12 26 9))
  ([data fastperiod slowperiod signalperiod]
   (trim (ta "macd" (makeOHLC data "C") fastperiod slowperiod signalperiod))))


;;;; Money Flow Index (MFI)
;; "theories suggest volume leads price"
;; same as RSI but scaled by volume
;; signals overbought/oversold conditions with 80/20
;; signals above/below 90/10 signify imminent reversal
;; ** class not found, MoneyFlow ** (I want this)
(defn MFI
  ([data] (MFI data 14))
  ([data timeperiod]
   (trim (ta "mfi" (makeOHLC data) timeperiod))))


;;;; Percentage Price Oscillator (PPO)
;; percentage-based version of MACD -> better
;; currently no signal line, but has opts for MA type
;; ** no core class found, same error as BBands **
;; note: PVO is this applied to volume
(defn PPO
  ([data] (PPO data 12 26 0))
  ([data fastperiod slowperiod matype]
   (trim (ta "ppo" (makeOHLC data "C") fastperiod slowperiod 0))))


;;;; Rate of Change (ROC)
;; simple difference, this option normalized as percentage
;; could use a 
(defn ROC
  ([data] (ROC data 20))
  ([data timeperiod]
   (trim (ta "rocp" (makeOHLC data "C") timeperiod))))


;;;; Standard Deviaton (StDev)
;; volatility of a period
(defn StDev
  ([data] (StDev data 20))
  ([data timeperiod]
   (trim (ta "stddev" (makeOHLC data "C") timeperiod 1))))


;;;; Stochastic Oscillator (STOCH)
;; Signifies overbought/oversold.
;; Breakaway from significant region may indicate start of reversal
;; Strong trends upward may have consistent "overbought" condition
;;   "Look for occasional oversold readings in an uptrend and ignore frequent overbought readings. Similarly, look for occasional overbought readings in a strong downtrend and ignore frequent oversold readings."
;; ** has MA option, probably won't work **
(defn STOCH
  ([data] (STOCH data 20))
  ([data timeperiod]
   (trim (ta "stochf" (makeOHLC data "C") timeperiod 1))))


;;;; Relative Strength Index (RSI)
;; measure of average gains/average losses for a period
;; above/below 80/20 signifies overbought/oversold
;; eyeballing, it seemed like overbought then dropping below 50 may signify
;;   trend moving in other direction. This would make sense, long period
;;   of overbought and then the buy/sell ratio equilibrates, so artificial
;;   inflation will pull the price back.
;; for the agnts, should consider a measurement of "how long" it's been
;;   overbought, or "how many consecutive times" it's signified overbought
;; keep in mind, overbought loses weight when there's a strong upward trend
;; need to look into divergences of higher-high on price and lower-high
;;   on RSI, as well as reversals
(defn RSI
  ([data] (RSI data 14))
  ([data timeperiod]
   (trim (ta "rsi" (makeOHLC data "C") timeperiod ))))



   


  

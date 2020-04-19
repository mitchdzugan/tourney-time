(ns tourney-time.tournament.elimination-round
  (:require [tourney-time.tournament.standing
             :refer [->Standing set-final?]]
            [allpa.core
             :refer [defprotomethod p2]]))

(defrecord Winners [depth])
(defrecord Losers [depth drop-round?])
(defrecord GrandsReset [])
(defrecord Grands [])

(defprotomethod label [{:keys [depth drop-round?]}]
  [Grands GrandsReset] "Grands"
  Winners
  (case depth
    0 "Winners Finals"
    1 "Winners Semis"
    (str "Winners Top " (quot (* 3 (p2 (inc depth))) 2)))
  Losers
  (cond
    (and (= 0 depth) drop-round?)
    "Losers Finals"

    (and (= 0 depth))
    "Losers Semi Finals"

    (and (= 1 depth) drop-round?)
    "Losers Quarter Finals"

    drop-round?
    (str "Losers Top " (* 3 (p2 depth)))

    :else
    (str "Losers Top " (p2 (+ 2 depth)))))

(defprotomethod upper-bracket? [this]
  Losers false
  [Winners Grands GrandsReset] true)

(defprotomethod get-round-base-rows [{:keys [depth]}]
  [Winners Losers]
  (cond
    (= 0 depth) [0]
    :else (-> depth dec p2 range vec))
  GrandsReset [0]
  Grands [0])

(defprotomethod to-int [{:keys [drop-round? depth]}]
  GrandsReset -2
  Grands -1
  Losers (cond
           drop-round?
           (+ 0 (* 3 depth))

           :else
           (+ 2 (* 3 depth)))

  Winners  (+ 1 (* 3 depth)))

(defn from-int-h [mod depth]
  (case mod
    0 (->Losers depth true)
    1 (->Winners depth)
    2 (->Losers depth false)))

(defn from-int [n]
  (case n
    -2 (->GrandsReset)
    -1 (->Grands)
    (from-int-h (mod n 3) (quot n 3))))

(defn get-round-range [round upper?]
  (let [filt (if upper? filter remove)]
    (->> (to-int round)
         inc
         (range -2)
         (map from-int)
         (filt upper-bracket?)
         vec)))

(defprotomethod get-standing [{:keys [depth drop-round?]} winner? de?]
  GrandsReset (->Standing (if winner? 1 2) true)

  Grands (->Standing 2 false)

  Winners
  (cond
    de?
    (-> (->Winners (inc depth))
        (get-standing winner? false)
        (assoc :final? false))

    (and winner? (= 0 depth))
    (->Standing 1 true)

    winner?
    (-> (->Winners (dec depth))
        (get-standing false false)
        (assoc :final? false))

    :else
    (->Standing (inc (p2 depth)) true))

  Losers
  (cond
    (and drop-round? winner? (= 0 depth))
    (->Standing 2 false)

    (and winner? drop-round?)
    (-> (->Losers (dec depth) false)
        (get-standing false true)
        (assoc :final? false))

    winner?
    (-> (->Losers depth true)
        (get-standing false true)
        (assoc :final? false))

    drop-round?
    (-> (inc (p2 (inc depth)))
        (->Standing true))

    :else
    (-> (p2 (inc depth))
        (* 3) (quot 2) inc
        (->Standing true))))

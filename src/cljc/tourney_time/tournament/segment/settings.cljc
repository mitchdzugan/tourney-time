(ns tourney-time.tournament.segment.settings
  (:require [allpa.core
             :refer [defprotomethod]]
            [allpa.linked-hash-map :as lhm]))

(defrecord SingleElimination [progression-specs ])
(defrecord DoubleElimination [progression-specs start-overrides ])
(defrecord Swiss [])
(defrecord RoundRobin [])
(defrecord Ladder [])

(defprotomethod get-progression-specs [{:keys [progression-specs]}]
  [SingleElimination DoubleElimination] progression-specs
  [Swiss RoundRobin Ladder] lhm/empty)

(defprotomethod get-start-overrides [{:keys [start-overrides]}]
  DoubleElimination start-overrides
  [SingleElimination Swiss RoundRobin Ladder] {})

(defprotomethod de? [_]
  DoubleElimination true
  [SingleElimination Swiss RoundRobin Ladder] false)

(defn default-settings [segment-type]
  (case segment-type
    ::SingleElimination (->SingleElimination lhm/empty)
    ::DoubleElimination (->DoubleElimination lhm/empty {})
    ::Swiss (->Swiss)
    ::RoundRobin (->RoundRobin)
    ::Ladder (->Ladder)))

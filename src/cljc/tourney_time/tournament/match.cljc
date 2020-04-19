(ns tourney-time.tournament.match
  (:require [allpa.core :as a]
            [tourney-time.tournament.slots :as slots]
            [tourney-time.tournament.match-player :refer [filled?]]))

(defrecord Match [path slots result])

(defn entrant [{:keys [slots]} slot-id] (get slots slot-id))

(defn opponent [{:keys [slots]} slot-id] (get slots (slots/oppo slot-id)))

(defn winner [{:keys [result] :as match}] (entrant match result))

(defn loser [{:keys [result] :as match}] (opponent match result))

(defn complete? [{:keys [result]}] (not (nil? result)))

(defn reportable? [match] (and (filled? (entrant match :up))
                               (filled? (entrant match :down))))

(defn get-match-progressor [match winner?] ((if winner? winner loser) match))

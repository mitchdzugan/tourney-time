(ns tourney-time.tournament.slots)

(defrecord Slots [up down])

(defn oppo [curr] (get {:up :down :down :up} curr))

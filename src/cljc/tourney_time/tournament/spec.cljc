(ns tourney-time.tournament.spec
  (:require [allpa.core :as a]
            [allpa.linked-hash-map :as lhm]
            [tourney-time.tournament.entrant-path
             :refer [->EntrantPath]]))

(defrecord TournamentSpec [entrant-groups segments results name])

(def empty-spec (->TournamentSpec lhm/empty
                                  lhm/empty
                                  {}
                                  ""))

(defn get-gamer [{:keys [entrant-group-id seed-num]} entrant-groups]
  (-> entrant-groups
      (lhm/get entrant-group-id)
      (get-in [:gamers seed-num])))

(defn tournament-seed-num [{:keys [entrant-group-id seed-num]} entrant-groups]
  (->> (lhm/take-while entrant-groups (comp #(not= %1 entrant-group-id) a/id))
       (reduce #(+ %1 (-> %2 :gamers count)) seed-num)))


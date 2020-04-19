(ns tourney-time.tournament.rep
  (:require [allpa.core
             :refer [curry]]
            [wayra.core :as w
             :refer [defnm]]
            [tourney-time.gamer
             :refer [->Gamer]]
            [tourney-time.tournament.action :as a]
            [tourney-time.tournament.entrant-group
             :refer [->EntrantGroup]]
            [tourney-time.tournament.entrant-source
             :refer [->GroupSource]]
            [tourney-time.tournament.segment.settings :as settings]
            [tourney-time.tournament.spec
             :refer [empty-spec]]))

(defrecord TournamentRep [actions undo active-count])

(defn to-tournament-spec [{:keys [actions undo] :as rep}]
  (->> actions
       (drop undo)
       reverse
       ((curry w/eachm a/apply-tournament-action))
       (w/exec {:init-state empty-spec})
       :state))

(defn undo [{:keys [active-count undo] :as rep}]
  (-> rep
      (assoc :undo (+ undo (min 1 active-count)))
      (assoc :active-count (max 0 (dec active-count)))))

(defn redo [{:keys [active-count undo] :as rep}]
  (-> rep
      (assoc :active-count (+ active-count (min 1 undo)))
      (assoc :undo (max 0 (dec undo)))))

(defn act [{:keys [actions active-count undo] :as rep} action]
  (-> rep
      (assoc :actions (conj (drop undo actions) action))
      (assoc :undo 0)
      (assoc :active-count (inc active-count))))

(defn can-undo? [{:keys [active-count]}] (> active-count 0))

(defn can-redo? [{:keys [undo]}] (> undo 0))

(def default-rep
  (->TournamentRep '((a/->AppendEntrantSource (->GroupSource 0))
                     (a/->AppendEntrantGroup (->EntrantGroup
                                              [(->Gamer "Entrant 1")
                                               (->Gamer "Entrant 2")
                                               (->Gamer "Entrant 3")
                                               (->Gamer "Entrant 4")]))
                     (a/->SetSegmentType 0 ::settings/SingleElimination)
                     (a/->SetName "Test Tournament"))
                   0
                   0))

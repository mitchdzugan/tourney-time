(ns tourney-time.tournament.builder.rep
  (:require [wayra.core :as w
             :refer [defnm defm]]
            [tourney-time.gamer
             :refer [->Gamer]]
            [tourney-time.tournament.action :as a]
            [tourney-time.tournament.entrant-group
             :refer [->EntrantGroup]]
            [tourney-time.tournament.elimination-round
             :refer [->Winners ->Losers ->Grands ->GrandsReset]]
            [tourney-time.tournament.match-path
             :refer [->EliminationPath]]
            [tourney-time.tournament.rep :as r]
            [tourney-time.tournament.pool-path
             :refer [->PoolPath]]
            [tourney-time.tournament.slots :as slots]))

(defnm -act [action] (w/modify #(r/act %1 action)))

(defm -undo (w/modify r/undo))

(defm -redo (w/modify r/redo))

(defnm -report-match [path up? seg-id group-id]
  let [slot-id (if up? :up :down)]
  (-act (a/->ReportMatch (->PoolPath seg-id group-id) path slot-id)))

(defnm -report-grands [up? s-id g-id]
  (-report-match (->EliminationPath (->Grands) 0) up? s-id g-id))

(defnm -report-grands-reset [up? s-id g-id]
  (-report-match (->EliminationPath (->GrandsReset) 0) up? s-id g-id))

(defnm -report-winners [depth row up? s-id g-id]
  (-report-match (->EliminationPath (->Winners depth) row) up? s-id g-id))

(defnm -report-losers [depth drop-round? row up? s-id g-id]
  (-report-match (->EliminationPath (->Losers depth drop-round?) row)
                 up? s-id g-id))

(defnm -append-entrant-group [tags]
  (-act (a/->AppendEntrantGroup (->EntrantGroup (->> tags (map ->Gamer) vec)))))

(defnm -set-entrant-group [eg-id tags]
  (-act (a/->SetEntrantGroup eg-id
                             (->EntrantGroup (->> tags (map ->Gamer) vec)))))

(defnm -insert-entrant-group [eg-id tags]
  (-act (a/->InsertEntrantGroup eg-id
                              (->EntrantGroup (->> tags (map ->Gamer) vec)))))

(defnm -remove-entrant-group [eg-id]
  (-act (a/->RemoveEntrantGroup eg-id)))

(defnm -set-name [name] (-act (a/->SetName name)))

(def empty-rep (r/->TournamentRep '() 0 0))

(defn build-rep [builder] (->> builder (w/exec {:init-state empty-rep}) :state))

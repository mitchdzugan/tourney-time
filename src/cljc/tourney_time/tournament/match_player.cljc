(ns tourney-time.tournament.match-player
  (:require [allpa.core :refer [defprotomethod]]))

(defrecord Bye [])

(defrecord TournamentEntrant [entrant-path])

(defrecord WinnerOf [match-path])

(defrecord LoserOf [match-path])

(defrecord ProgressionFrom [segment-id progression-id pool-id])

(defprotomethod filled? [_]
  TournamentEntrant true
  [Bye WinnerOf LoserOf ProgressionFrom] false)

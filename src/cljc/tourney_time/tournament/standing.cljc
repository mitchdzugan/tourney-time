(ns tourney-time.tournament.standing
  (:require [allpa.core :as a]))

(defrecord Standing [placement final?])

(def set-placement #(assoc %1 :placement %2))

(def set-final? #(assoc %1 :final? %2))

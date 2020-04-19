(ns tourney-time.tournament.action
  (:require [allpa.core :as a
             :refer [curry defprotomethod]]
            [allpa.linked-hash-map :as lhm]
            [wayra.core :as w
             :refer [mdo]]
            [wayra.macros :refer [fnm]]))

(defrecord ReportMatch [pool-path match-path result])

(defrecord ResetReporting [pool-path])

(defrecord AppendEntrantGroup [entrant-group])

(defrecord SetEntrantGroup [entrant-group-id entrant-group])

(defrecord InsertEntrantGroup [entrant-group-id entrant-group])

(defrecord RemoveEntrantGroup [entrant-group-id])

(defrecord SetName [name])

(defrecord SetSegmentType [segment-id segment-type])

(defrecord RemoveSegment [segment-id])

(defrecord SetSegmentName [segment-id name])

(defrecord SetSegmentNumPools [segment-id num-pools])

(defrecord AppendProgressionSpec [segment-id progression-spec])

(defrecord RemoveProgressionSpec [segment-id progression-id])

(defrecord AppendEntrantSource [segment-id entrant-source])

(defrecord SetEntrantSource [segment-id entrant-source-id entrant-source])

(defrecord RemoveEntrantSource [segment-id entrant-source-id])

(defprotomethod apply-tournament-action [action]
  ReportMatch
  (let [{:keys [pool-path match-path result]} action]
    (w/modify #(assoc-in %1 [:results pool-path match-path] result)))

  ResetReporting
  (let [{:keys [pool-path]} action]
    (w/modify #(assoc-in %1 [:results pool-path] {})))

  AppendEntrantGroup
  (let [{:keys [entrant-group]} action]
    (w/modify (curry update :entrant-groups
                     #(lhm/append- %1 entrant-group a/set-id))))

  SetEntrantGroup
  (let [{:keys [entrant-group-id entrant-group]} action]
    (w/modify (curry update :entrant-groups
                     #(lhm/set %1 entrant-group-id
                               (a/set-id entrant-group entrant-group-id)))))

  InsertEntrantGroup
  (let [{:keys [entrant-group-id entrant-group]} action]
    (w/modify (curry update :entrant-groups
                     #(lhm/insert- %1 entrant-group-id entrant-group a/set-id))))

  RemoveEntrantGroup
  (let [{:keys [entrant-group-id]} action]
    (w/modify (curry update :entrant-groups
                     #(lhm/remove %1 entrant-group-id))))

  SetName
  (let [{:keys [name]} action] (w/modify #(assoc %1 :name name)))

  [SetSegmentType]
  (w/pure nil)
  )

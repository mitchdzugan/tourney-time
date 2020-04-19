(ns tourney-time.tournament.builder.gen
  (:require [allpa.core :as a
             :refer [curry ->Ok ->Fail]]
            [allpa.linked-hash-map :as lhm]
            [wayra.core :as w
             :refer [defnm defm mdo]]
            [tourney-time.gamer
             :refer [->Gamer]]
            [tourney-time.tournament.elimination-round
             :refer [->Winners ->Losers ->Grands ->GrandsReset]]
            [tourney-time.tournament.entrant-group
             :refer [->EntrantGroup]]
            [tourney-time.tournament.entrant-path
             :refer [->EntrantPath]]
            [tourney-time.tournament.gen
             :refer [->TournamentGen]]
            [tourney-time.tournament.match
             :refer [->Match]]
            [tourney-time.tournament.match-path
             :refer [->EliminationPath]]
            [tourney-time.tournament.match-player
             :refer [->Bye
                     ->TournamentEntrant
                     ->WinnerOf
                     ->LoserOf
                     ->ProgressionFrom]]
            [tourney-time.tournament.pool
             :refer [->Pool]]
            [tourney-time.tournament.segment
             :refer [->Segment]]
            [tourney-time.tournament.standing
             :refer [->Standing]]
            [tourney-time.tournament.slots :as slots
             :refer [->Slots]])
  #?(:cljs (:require-macros
            [tourney-time.tournament.builder.gen :refer [-segment
                                                    -mod-segment
                                                    -pool
                                                    -mod-pool
                                                    -grands-reset
                                                    -grands
                                                    -winners
                                                    -losers]])))

(defnm -warning [warning] (w/modify #(update %1 :warnings (curry conj warning))))

(defnm -error [error] (w/modify #(assoc %1 :error error)))

(defnm -spec [spec] (w/modify #(assoc %1 :spec spec)))

(defnm --segment [id builder]
  spec <- (w/gets #(-> %1
                       :spec
                       :segments
                       (lhm/get id)))
  (w/modify (curry merge {:pools [] :segment-spec spec}))
  builder
  segment-spec <- (w/gets :segment-spec)
  pools <- (w/gets :pools)
  let [segment (->Segment segment-spec pools)]
  (w/modify #(update %1 :segments (comp vec (curry conj segment)))))

#?(:clj
   (defmacro -segment [id & builder]
     `(--segment ~id (mdo ~@builder))))

(defnm --mod-segment [id builder]
  segments <- (w/gets :segments)
  let [is-target #(= id (-> %1 :spec a/id))
       target (first (filter is-target segments))]
  (w/modify (curry merge {:pools (:pools target)
                          :segment-spec (:spec target)}))
  builder
  segment-spec <- (w/gets :segment-spec)
  pools <- (w/gets :pools)
  let [segment (->Segment segment-spec pools)]
  (w/modify (curry update :segments
                   (comp vec (partial map #(if (is-target %1) segment %1))))))

#?(:clj
   (defmacro -mod-segment [id & builder]
     `(--mod-segment ~id (mdo ~@builder))))

(defnm --pool [builder]
  init-pool-id <- (w/gets #(-> %1 :pools count))
  (w/modify (curry merge {:pool-id init-pool-id
                          :matches nil
                          :pool-standings {}
                          :prog-groups lhm/empty}))
  builder
  pool-id <- (w/gets :pool-id)
  matches <- (w/gets :matches)
  standings <- (w/gets :pool-standings)
  prog-groups <- (w/gets :prog-groups)
  settings <- (w/gets #(-> %1 :segment-spec :settings))
  let [pool (-> (->Pool matches standings prog-groups settings)
                (a/set-id pool-id))]
  (w/modify #(update %1 :pools (curry conj pool))))

#?(:clj
   (defmacro -pool [& builder]
     `(--pool (mdo ~@builder))))

(defnm --mod-pool [id builder]
  pools <- (w/gets :pools)
  let [is-target #(= id (-> %1 a/id))
       target (first (filter is-target pools))]
  (w/modify (curry merge {:pool-id (a/id target)
                          :matches (:matches target)
                          :pool-standings (:standings target)
                          :settings (:settings target)
                          :prog-groups (:progression-groups target)}))
  builder
  pool-id <- (w/gets :pool-id)
  matches <- (w/gets :matches)
  standings <- (w/gets :pool-standings)
  prog-groups <- (w/gets :prog-groups)
  settings <- (w/gets #(-> %1 :segment-spec :settings))
  let [pool (-> (->Pool matches standings prog-groups settings)
                (a/set-id pool-id))]
  (w/modify (curry update :pools
                   (partial map #(if (is-target %1) pool %1)))))

#?(:clj
   (defmacro -mod-pool [id & builder]
     `(--mod-pool ~id (mdo ~@builder))))

(defnm -standing [group-id seed-num placement final?]
  let [path (->EntrantPath group-id seed-num)
       standing (->Standing placement final?)]
  (w/modify #(assoc-in %1 [:standings path] standing)))

(defnm -pool-standing [group-id seed-num placement final?]
  let [entrant (->TournamentEntrant (->EntrantPath group-id seed-num))
       standing (->Standing placement final?)]
  (w/modify #(assoc-in %1 [:pool-standings entrant] standing)))

(defnm -prog-group [prog-id progs]
  (w/modify (curry update :prog-groups
                   #(:lhm (lhm/append-unsafe %1 progs prog-id)))))

(defnm -set-prog-group [prog-id progs]
  (w/modify (curry update :prog-groups
                   #(lhm/set %1 prog-id progs))))

(defn -entrant-prog [group-id seed-num]
  (->TournamentEntrant (->EntrantPath group-id seed-num)))

(defn -from-prog [segment-id progression-id pool-id]
  (->ProgressionFrom segment-id progression-id pool-id))

(defn -winner [slot-id]
  (w/modify #(assoc %1 :result slot-id)))

(defn -entrant [slot-id e]
  (case slot-id
   :up (w/modify #(assoc %1 :up-entrant e))
   :down (w/modify #(assoc %1 :down-entrant e))))

(defnm -up-progression [segment-id progression-id pool-id]
  (-entrant :up (->ProgressionFrom segment-id progression-id pool-id)))

(defnm -down-progression [segment-id progression-id pool-id]
  (-entrant :down (->ProgressionFrom segment-id progression-id pool-id)))

(defnm -up-entrant [group-id seed-num]
  (-entrant :up
            (->TournamentEntrant (->EntrantPath group-id seed-num))))

(defnm -down-entrant [group-id seed-num]
  (-entrant :down
            (->TournamentEntrant (->EntrantPath group-id seed-num))))

(defnm -up-wb-winner [depth row]
  (-entrant :up (->WinnerOf (->EliminationPath (->Winners depth) row))))

(defnm -up-wb-loser [depth row]
  (-entrant :up (->LoserOf (->EliminationPath (->Winners depth) row))))

(defnm -up-lb-winner [depth drop-round? row]
  (-entrant :up (->WinnerOf (->EliminationPath (->Losers depth drop-round?)
                                                  row))))

(defnm -down-wb-winner [depth row]
  (-entrant :down (->WinnerOf (->EliminationPath (->Winners depth) row))))

(defnm -down-wb-loser [depth row]
  (-entrant :down (->LoserOf (->EliminationPath (->Winners depth) row))))

(defnm -down-lb-winner [depth drop-round? row]
  (-entrant :down (->WinnerOf (->EliminationPath (->Losers depth drop-round?)
                                                    row))))

(defnm -match [path builder]
  (w/modify (curry merge {:up-entrant nil :down-entrant nil :result nil}))
  builder
  {:keys [up-entrant down-entrant result]} <- w/get
  let [match (->Match path (->Slots up-entrant down-entrant) result)]
  (w/modify (curry assoc-in [:matches path] match)))

(defnm -remove-match [path]
  (w/modify (curry update :matches #(dissoc %1 path))))

(defm -remove-grands-reset
  (-remove-match (->EliminationPath (->GrandsReset) 0)))

(defm -remove-grands
  (-remove-match (->EliminationPath (->Grands) 0)))

(defnm -remove-winners [depth row]
  (-remove-match (->EliminationPath (->Winners depth) row)))

(defnm -remove-losers [depth drop-round? row]
  (-remove-match (->EliminationPath (->Losers depth drop-round?) row)))

(defnm -reset-match [path]
  (w/modify (curry assoc-in [:matches path :result] nil)))

(defm -reset-grands-reset
  (-reset-match (->EliminationPath (->GrandsReset) 0)))

(defm -reset-grands
  (-reset-match (->EliminationPath (->Grands) 0)))

(defnm -reset-winners [depth row]
  (-reset-match (->EliminationPath (->Winners depth) row)))

(defnm -reset-losers [depth drop-round? row]
  (-reset-match (->EliminationPath (->Losers depth drop-round?) row)))

#?(:clj
   (defmacro -grands-reset [& builder]
     `(-match (->EliminationPath (->GrandsReset) 0)
              (mdo ~@builder))))

#?(:clj
   (defmacro -grands [& builder]
     `(-match (->EliminationPath (->Grands) 0)
              (mdo ~@builder))))

#?(:clj
   (defmacro -winners [depth row & builder]
     `(-match (->EliminationPath (->Winners ~depth) ~row)
              (mdo ~@builder))))

#?(:clj
   (defmacro -losers [depth drop-round? row & builder]
     `(-match (->EliminationPath (->Losers ~depth ~drop-round?) ~row)
              (mdo ~@builder))))

(defn build-gen [builder]
  (let [{:keys [warnings segments standings spec error]}
        (-> (w/exec {:segments []} builder)
            :state)]
    (if error
      (->Fail error)
      (->Ok (->TournamentGen spec segments standings warnings)))))

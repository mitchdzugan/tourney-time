(ns tourney-time.tournament.builder.spec
  (:require [allpa.core :as a
             :refer [curry]]
            [allpa.linked-hash-map :as lhm]
            [wayra.core :as w
             :refer [defnm defm mdo]]
            [tourney-time.gamer
             :refer [->Gamer]]
            [tourney-time.tournament.elimination-progression-spec
             :refer [->EliminationProgressionSpec]]
            [tourney-time.tournament.elimination-round
             :refer [->Winners ->Losers ->Grands ->GrandsReset]]
            [tourney-time.tournament.entrant-group
             :refer [->EntrantGroup]]
            [tourney-time.tournament.entrant-source
             :refer [->GroupSource ->ProgressionSource]]
            [tourney-time.tournament.match-path
             :refer [->EliminationPath]]
            [tourney-time.tournament.segment.spec
             :refer [->SegmentSpec]]
            [tourney-time.tournament.segment.settings :as settings
             :refer [->SingleElimination
                     ->DoubleElimination
                     default-settings]]
            [tourney-time.tournament.spec
             :refer [empty-spec]]
            [tourney-time.tournament.pool-path
             :refer [->PoolPath]]
            [tourney-time.tournament.slots :as slots])
  #?(:cljs (:require-macros
            [tourney-time.tournament.builder.spec :refer [-segment-spec
                                                     -mod-segment-spec
                                                     -pool-results]])))

(defnm -result [match-path up?]
  let [slot-id (if up? :up :down)]
  (w/modify #(assoc-in %1 [:curr-pool-results match-path] slot-id)))

(defnm -grands-result [up?]
  (-result (->EliminationPath (->Grands) 0) up?))

(defnm -grands-reset-result [up?]
  (-result (->EliminationPath (->GrandsReset) 0) up?))

(defnm -winners-result [depth row up?]
  (-result (->EliminationPath (->Winners depth) row) up?))

(defnm -losers-result [depth drop-round? row up?]
  (-result (->EliminationPath (->Losers depth drop-round?) row) up?))

(defnm --pool-results [seg-id pool-id builder]
  let [pool-path (->PoolPath seg-id pool-id)]
  curr-results <- (w/gets #(get-in %1 [:spec :results pool-path]))
  (w/modify #(assoc %1 :curr-pool-results curr-results))
  builder
  pool-results <- (w/gets :curr-pool-results)
  (w/modify #(assoc-in %1 [:spec :results pool-path] pool-results)))

#?(:clj
   (defmacro -pool-results [seg-id pool-id & builder]
     `(--pool-results ~seg-id ~pool-id (mdo ~@builder))))

(defnm -clear-results [seg-id pool-id]
  let [pool-path (->PoolPath seg-id pool-id)]
  (w/modify #(assoc-in %1 [:spec :results pool-path] {})))

(defnm -entrant-groups [& specs]
  let [with-inds (map #(-> [%1 %2]) (range) specs)
       ids (->> with-inds
                (filter (fn [[ind _]] (even? ind)))
                (map (fn [[_ id]] id)))
       tag-groups (->> with-inds
                       (filter (fn [[ind _]] (odd? ind)))
                       (map (fn [[_ tags]] tags)))
       groups (map #(-> (map ->Gamer %2)
                        ->EntrantGroup
                        (a/set-id %1))
                   ids
                   tag-groups)]
  (w/modify #(assoc-in %1 [:spec :entrant-groups]
                       (lhm/rebuild (concat (lhm/to-vector (-> %1
                                                               :spec
                                                               :entrant-groups))
                                            groups)))))

;; TODO make work like entrant-groups
(defnm -source [source]
  (w/modify (curry update :curr-sources
                   #(lhm/append- %1 source a/set-id))))

(defnm -group-source [group-id]
  (-source (->GroupSource group-id)))

(defnm -progression-source [seg-id prog-id]
  (-source (->ProgressionSource seg-id prog-id)))

(defnm -settings [settings]
  (w/modify #(assoc %1 :curr-settings settings)))

(defnm -se-settings [prog-specs]
  (-settings (->SingleElimination (lhm/rebuild prog-specs))))

(defnm -de-settings [prog-specs start-overrides]
  (-settings (->DoubleElimination (lhm/rebuild prog-specs) start-overrides)))

(defn -winners-eps [id depth winner?]
  (-> (->EliminationProgressionSpec (->Winners depth) winner?)
      (a/set-id id)))

(defn -losers-eps [id depth drop-round? winner?]
  (-> (->EliminationProgressionSpec (->Losers depth drop-round?) winner?)
      (a/set-id id)))

(defnm -segment-name [name]
  (w/modify #(assoc %1 :curr-name name)))

(defnm -num-pools [num-pools]
  (w/modify #(assoc %1 :curr-num-pools num-pools)))

(defnm -build-segment-spec [builder]
  builder
  {:keys [curr-name
          curr-num-pools
          curr-sources
          curr-settings]} <- w/get
  (w/pure (->SegmentSpec curr-name
                         curr-settings
                         curr-num-pools
                         curr-sources)))

(defnm --segment-spec [builder]
  (w/modify #(merge %1 {:curr-name ""
                        :curr-num-pools 1
                        :curr-sources lhm/empty
                        :curr-settings (default-settings
                                        ::settings/SingleElimination)}))
  segment-spec <- (-build-segment-spec builder)
  (w/modify (curry update-in [:spec :segments]
                   #(lhm/append- %1 segment-spec a/set-id))))

#?(:clj
   (defmacro -segment-spec [& builder]
     `(--segment-spec (mdo ~@builder))))

(defnm --mod-segment-spec [seg-id builder]
  {:keys [name
          num-pools
          settings
          entrant-sources]} <- (w/gets #(-> (get-in % [:spec :segments])
                                            (lhm/get seg-id)))
  (w/modify #(merge %1 {:curr-name name
                        :curr-num-pools num-pools
                        :curr-sources entrant-sources
                        :curr-settings settings}))
  segment-spec <- (-build-segment-spec builder)
  (w/modify (curry update-in [:spec :segments]
                   #(lhm/set %1 seg-id (a/set-id segment-spec seg-id)))))

#?(:clj
   (defmacro -mod-segment-spec [id & builder]
     `(--mod-segment-spec ~id (mdo ~@builder))))

(defnm -name [name]
  (w/modify #(assoc-in %1 [:spec :name] name)))

(defn build-spec [builder]
  (->> builder
       (w/exec {:init-state {:spec empty-spec}})
       :state
       :spec))

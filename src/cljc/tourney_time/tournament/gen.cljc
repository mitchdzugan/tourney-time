(ns tourney-time.tournament.gen
  (:require [wayra.core :as w
             :refer [defnm defm fnm mdo]]
            [allpa.core :as a
             :refer [curry p2 ->Ok ->Fail
                     defprotomethod]]
            [allpa.linked-hash-map :as lhm]
            [tourney-time.tournament.elimination-round :as er
             :refer [->Winners ->Losers ->Grands ->GrandsReset]]
            [tourney-time.tournament.entrant-path
             :refer [->EntrantPath]]
            [tourney-time.tournament.entrant-source :as es]
            [tourney-time.tournament.match :as m
             :refer [->Match]]
            [tourney-time.tournament.match-path :as path
             :refer [->EliminationPath]]
            [tourney-time.tournament.match-player :as mp
             :refer [->TournamentEntrant
                     ->WinnerOf
                     ->LoserOf
                     ->Bye
                     ->ProgressionFrom]]
            [tourney-time.tournament.pool
             :refer [->Pool]]
            [tourney-time.tournament.pool-path
             :refer [->PoolPath]]
            [tourney-time.tournament.segment
             :refer [->Segment]]
            [tourney-time.tournament.segment.settings :as settings]
            [tourney-time.tournament.slots :as slots
             :refer [->Slots]]
            [tourney-time.tournament.standing
             :refer [->Standing set-final?]]
            [clojure.pprint :refer [pprint]]))

(defrecord TournamentGen [spec segments standings warnings])

(defrecord ReusedEntrantGroup [entrant-group-id])
(defrecord InvalidEntrantGroup [entrant-group-id])
(defrecord InvalidProgressionSegmentId [segment-id])
(defrecord InvalidProgressionId [progression-id])
(defrecord UnimplementedBehavior [])

(defnm <#> [m f]
  v <- m
  [(f v)])

(def gets-de?
  (<#> (w/gets #(-> %1 :segment-spec :settings))
       settings/de?))

(def gets-progression-specs
  (<#> (w/gets #(-> %1 :segment-spec :settings))
       settings/get-progression-specs))

(def gets-start-overrides
  (<#> (w/gets #(-> %1 :segment-spec :settings))
       settings/get-start-overrides))

(defprotomethod set-standing [entrant standing]
  !TournamentEntrant
  (w/modify #(assoc-in %1 [:standings entrant] standing))

  [!Bye !WinnerOf !LoserOf !ProgressionFrom]
  (w/pure nil))

(defm calc-pool-progression-groups
  prog-specs <- gets-progression-specs
  pool-id <- (w/gets :pool-id)
  segment-id <- (w/gets #(-> %1 :segment-spec a/id))
  matches <- (w/gets :matches)
  let [get-round-progressors
       (fn [prog-spec]
         (let [{:keys [round winner?]} prog-spec
               progression-id (a/id prog-spec)]
           (vec (map #(let [match (get matches %1)]
                        (cond (nil? match) (->Bye)
                              :else (or (m/get-match-progressor match winner?)
                                        (->ProgressionFrom segment-id
                                                           progression-id
                                                           pool-id))))
                     (path/get-seed-order-round-paths round)))))]
  [(lhm/map get-round-progressors prog-specs)])

(defnm create-match [path slots]
  {:keys [min-winners-round
          min-losers-round
          results
          start-losers]} <- w/get
  let [user-result (get results path)
       round (path/get-round path)
       winners? (er/upper-bracket? round)
       losers? (not winners?)
       up-skip? (and winners?
                     (or (get start-losers (get slots :up))
                         (= (->Bye) (get slots :up))))
       down-skip? (and winners?
                       (or (get start-losers (get slots :down))
                           (= (->Bye) (get slots :down))))
       result (cond
                up-skip? :down
                down-skip? :up
                :else user-result)
       match (->Match path slots result)
       beyond-winners? (and winners?
                            (< (er/to-int round) (er/to-int min-winners-round)))
       beyond-losers? (and losers?
                           (< (er/to-int round) (er/to-int min-losers-round)))]
  (w/whenm (or up-skip? down-skip?)
           (w/modify (curry update :remove-paths #(w/maplus %1 path))))
  (if (or beyond-losers? beyond-winners?)
    (mdo [(->Bye)])
    (mdo
     (w/modify #(assoc-in %1 [:matches path] match))
     (w/whenm (m/complete? match)
              de? <- gets-de?
              let [w (m/winner match)
                   l (m/loser match)
                   ws (er/get-standing round true de?)
                   ls (er/get-standing round false de?)]
              (set-standing w ws)
              (set-standing l ls))
     [(or (get slots result) (->WinnerOf path))])))

(defm single-elim-gen
  prog-specs <- gets-progression-specs
  (w/eachm (lhm/to-vector prog-specs)
           (fnm [{:keys [winner? round]}]
                min-winners-round <- (w/gets :min-winners-round)
                (w/whenm (and winner?
                              (er/upper-bracket? round)
                              (> (er/to-int round)
                                 (er/to-int min-winners-round)))
                         (w/modify #(assoc %1 :min-winners-round round)))))
  min-winners-round <- (w/gets :min-winners-round)
  entrants <- (w/gets :entrants)
  let [is-valid-seed #(< % (count entrants))
       get-pool-entrant #(nth entrants % (->Bye))
       expand (fnm [depth row seed]
                   let [deep-enough? (> (er/to-int (er/->Winners depth))
                                        (er/to-int min-winners-round))
                        oppo (- (p2 (inc depth)) seed 1)
                        is-valid (and (is-valid-seed seed)
                                      (is-valid-seed oppo))]
                   (if (and (not is-valid) deep-enough?)
                     (mdo de? <- gets-de?
                          let [entrant (get-pool-entrant seed)
                               standing (er/get-standing (->Winners depth)
                                                         true
                                                         de?)]
                          (set-standing entrant standing)
                          [entrant])
                     (mdo up <- (expand (inc depth) (* 2 row) seed)
                          down <- (expand (inc depth) (inc (* 2 row)) oppo)
                          (create-match (->EliminationPath (->Winners depth) row)
                                        (->Slots up down)))))]
  (expand 0 0 0))

(defm double-elim-gen
  prog-specs <- gets-progression-specs
  (w/eachm (lhm/to-vector prog-specs)
           (fnm [{:keys [winner? round]}]
                min-losers-round <- (w/gets :min-losers-round)
                let [wb-loss? (and (not winner?)
                                   (er/upper-bracket? round))
                     lb-win? (and winner? (not (er/upper-bracket? round)))]
                (w/whenm (and (or wb-loss? lb-win?)
                              (> (er/to-int round)
                                 (er/to-int min-losers-round)))
                         (w/modify #(assoc %1 :min-losers-round round)))))
  single-elim-gen
  winners-matches <- (w/gets :matches)
  let [max-depth (apply max (->> winners-matches
                                 vals
                                 (map :path)
                                 (map path/get-round)
                                 (map er/to-int)
                                 (map #(inc (quot %1 3)))))

       expand
       (fnm [depth row drop-pivot]
            let [drop-path (->EliminationPath (->Winners depth) drop-pivot)
                 drop-match (get winners-matches drop-path)
                 drop? (not (nil? drop-match))
                 feeders? (< depth max-depth)]
            (and (not drop?)
                 (not feeders?)) --> [{:drop? drop?
                                       :feeders? feeders?
                                       :entrant (->Bye)}]
            let [next-drop-pivot (* 2 (- (dec (p2 depth)) drop-pivot))]
            exp0 <- (expand (inc depth)      (* 2 row)       next-drop-pivot)
            exp1 <- (expand (inc depth) (inc (* 2 row)) (inc next-drop-pivot))
            let [any-feeders? (or  (:drop? exp0) (:drop? exp1))
                 all-feeders? (and (:drop? exp0) (:drop? exp1))]
            (not any-feeders?) --> [{:drop? drop?
                                     :feeders? false
                                     :entrant (->Bye)}]
            let [needs-match? #(and (:drop? %1) (not (:feeders? %1)))
                 needs-match0? (needs-match? exp0)
                 needs-match1? (needs-match? exp1)
                 next-drop-path0 (->EliminationPath (->Winners (inc depth))
                                                  next-drop-pivot)
                 next-drop-path1 (->EliminationPath (->Winners (inc depth))
                                                  (inc next-drop-pivot))
                 drop-path-entrant #(or (-> (get winners-matches %1)
                                            m/loser)
                                        (->LoserOf %1))]
            l1-slot-spec <-
            (w/whenm all-feeders?
                     let [l2-path (->EliminationPath (->Losers depth false) row)
                          slots (->Slots (if needs-match1?
                                         (drop-path-entrant next-drop-path1)
                                         (:entrant exp1))
                                       (if needs-match0?
                                         (drop-path-entrant next-drop-path0)
                                         (:entrant exp0)))]
                     (create-match l2-path slots))
            let [l1-path (->EliminationPath (->Losers depth true) row)
                 slots (->Slots (drop-path-entrant drop-path)
                              (or l1-slot-spec
                                  (drop-path-entrant (if needs-match0?
                                                       next-drop-path0
                                                       next-drop-path1))))]
            entrant <- (create-match l1-path slots)
            [{:entrant entrant
              :feeders? any-feeders?
              :drop? drop?}])]
  {lf-winner :entrant} <- (expand 0 0 0)
  let [wf-path (->EliminationPath (->Winners 0) 0)
       wf-winner (or (m/winner (get winners-matches wf-path))
                     (->WinnerOf wf-path))
       gf-slots (->Slots wf-winner lf-winner)]
  gf-winner <- (create-match (->EliminationPath (->Grands) 0) gf-slots)
  (not (mp/filled? gf-winner)) --> []
  let [reset? (not= gf-winner wf-winner)]
  champ <- (if reset?
             (create-match (->EliminationPath (->GrandsReset) 0) gf-slots)
             (w/pure gf-winner))
  (cond
    (and (mp/filled? champ)
         (mp/filled? lf-winner)
         (mp/filled? wf-winner))
    (mdo
     (set-standing wf-winner (->Standing 2 true))
     (set-standing lf-winner (->Standing 2 true))
     (set-standing champ (->Standing 1 true)))

    (and (mp/filled? lf-winner)
         (mp/filled? wf-winner))
    (mdo
     (set-standing wf-winner (->Standing 2 false))
     (set-standing lf-winner (->Standing 2 false)))

    :else
    (w/pure nil)))

(defprotomethod segment-pool-gen [_]
  !settings/SingleElimination single-elim-gen
  !settings/DoubleElimination double-elim-gen
  [!settings/RoundRobin !settings/Ladder !settings/Swiss]
  (w/fail (->UnimplementedBehavior)))

(defnm take-group-source [group-id]
  used <- (w/gets :used-entrant-groups)
  (contains? used group-id) --> (w/fail (->ReusedEntrantGroup group-id))
  group <- (w/asks #(-> %1
                        :spec
                        :entrant-groups
                        (lhm/get group-id)))
  (nil? group) --> (w/fail (->InvalidEntrantGroup group-id))
  (w/modify #(update %1 :used-entrant-groups (curry w/maplus group-id)))
  [(->> group
        :gamers
        (map (fn [seed-num _]
               (->TournamentEntrant (->EntrantPath group-id seed-num)))
             (range)))])

(defnm gen-pool [[entrants pool-id]]
  segment-id <- (w/gets #(-> %1 :segment-spec a/id))
  settings <- (w/gets #(-> %1 :segment-spec :settings))
  full-results <- (w/asks #(-> %1 :spec :results))
  let [pool-path (->PoolPath segment-id pool-id)
       results (get full-results pool-path)
       init-standings (->> entrants
                           (filter mp/filled?)
                           (reduce #(assoc %1 %2
                                           (->Standing (count entrants) false))
                                   {}))]
  (w/modify (curry merge {:matches nil
                          :results results
                          :entrants entrants
                          :remove-paths #{}
                          :min-winners-round (->GrandsReset)
                          :min-losers-round (->GrandsReset)
                          :standings init-standings
                          :pool-id pool-id}))
  (segment-pool-gen settings)
  prog-groups <- calc-pool-progression-groups
  {:keys [matches standings remove-paths]} <- w/get
  let [final-matches (reduce #(dissoc %1 %2) matches remove-paths)]
  [(-> (->Pool final-matches standings prog-groups settings)
       (a/set-id pool-id))])

(defprotomethod take-raw-entrant-source [source]
  !es/GroupSource
  (mdo let [{:keys [entrant-group-id]} source]
       group <- (take-group-source entrant-group-id)
       [{:entrants group
         :start-dir :up}])

  !es/ProgressionSource
  (mdo let [{:keys [segment-id progression-id]} source]
       built-segments <- (w/gets :built-segments)
       let [source-segment (lhm/get built-segments segment-id)]
       (nil? source-segment) --> (w/fail (->InvalidProgressionSegmentId
                                          segment-id))
       let [prog-spec (-> source-segment
                          :spec
                          :settings
                          settings/get-progression-specs
                          (lhm/get progression-id))
            {:keys [round winner?]} prog-spec]
       (nil? prog-spec) --> (w/fail (->InvalidProgressionId progression-id))
       ;; TODO make sure the following works in wayra.macros:
       ;; TODO
       ;; TODO    (mdo let [{:keys [a]} {:a 1}
       ;; TODO              b (inc a)]
       ;; TODO         [b])
       ;; TODO
       let [start-dir (if (and winner? (er/upper-bracket? round)) :up :down)
            entrants (->> (:pools source-segment)
                          (map #(lhm/get (:progression-groups %1)
                                         progression-id))
                          vec
                          a/unsnake)]
       [{:start-dir start-dir :entrants entrants}]))

(defm gen-segments
  spec <- (w/asks :spec)
  (w/modify #(assoc %1 :built-segments lhm/empty))
  let [take-entrant-source
       (fnm [source]
            let [source-id (a/id source)]
            entrants-def <- (take-raw-entrant-source source)
            start-overrides <- gets-start-overrides
            let [start-dir (or (get start-overrides source-id)
                               (:start-dir entrants-def))]
            (w/whenm (= :down start-dir)
              (w/eachm (:entrants entrants-def)
                       #(w/modify (curry update :start-losers
                                         (curry w/maplus %1)))))
            [(assoc entrants-def :start-dir start-dir)])]

  (w/eachm (-> spec :segments lhm/to-vector)
           (fnm [segment-spec]
                (w/modify (curry merge {:start-losers #{}}))
                let [{:keys [num-pools entrant-sources settings]} segment-spec]
                (w/modify (curry merge {:settings settings
                                        :segment-spec segment-spec}))
                entrants-defs <- (w/mapm take-entrant-source
                                         (lhm/to-vector entrant-sources))
                let [entrants (vec (mapcat :entrants entrants-defs))
                     entrants-by-pool (a/snake num-pools entrants)]
                pools <- (w/mapm gen-pool (map vector entrants-by-pool (range)))
                let [segment (->Segment segment-spec (vec pools))
                     do-append #(-> %1
                                    (lhm/append-unsafe segment
                                                       (a/id segment-spec))
                                    :lhm)]
                (w/modify #(update %1 :built-segments do-append))))
  built-segments <- (w/gets :built-segments)
  [(lhm/to-vector built-segments)])

(defm gen
  spec <- (w/asks :spec)
  segments <- gen-segments
  (w/modify (curry merge {:used-entrants #{}
                          :standings {}
                          :curr-placement 1
                          :curr-tied 0
                          :max-non-final 0}))
  (w/eachm
   (reverse segments)
   (fnm [{:keys [pools]}]
        let [standings-by-placement (->> pools
                                         (map :standings)
                                         (reduce merge {})
                                         (a/map-values #(-> {:standing %1
                                                             :entrant %2}))
                                         vals
                                         (group-by #(-> %1
                                                        :standing
                                                        :placement)))
             max-placement (if (empty? standings-by-placement)
                             0
                             (apply max (keys standings-by-placement)))]
        (w/eachm
         (range 1 (inc max-placement))
         (fnm [placement]
              (w/modify #(merge %1 {:curr-placement (+ (:curr-placement %1)
                                                       (:curr-tied %1))
                                    :curr-tied 0}))
              (w/eachm
               (get standings-by-placement placement)
               (fnm [{:keys [entrant standing]}]
                    used-entrants <- (w/gets :used-entrants)
                    let [path (:entrant-path entrant)]
                    (w/whenm (and path (not (contains? used-entrants path)))
                     let [new-used (w/maplus used-entrants path)
                          final? (:final? standing)]
                     (w/modify
                      #(merge %1
                              {:used-entrants new-used
                               :curr-tied (inc (:curr-tied %1))
                               :standings (assoc (:standings %1)
                                                 path
                                                 (->Standing (:curr-placement %1)
                                                           final?))
                               :max-non-final (if final?
                                                (:max-non-final %1)
                                                (:curr-placement %1))})))))))))
  max-non-final <- (w/gets :max-non-final)
  standings <- (w/gets :standings)
  warnings <- (w/gets :warnings)
  let [final-segments (vec (filter #(->> (:pools %1)
                                         (map :matches)
                                         (map count)
                                         (reduce + 0)
                                         (not= 0))
                                   segments))
       final-standings (a/map-values (fn [standing _]
                                       (if (<= (:placement standing)
                                               max-non-final)
                                         (set-final? standing false)
                                         standing))
                                     standings)]
  [(->TournamentGen spec final-segments final-standings warnings)])

(defn to-tournament-gen [spec]
  (let [{:keys [error result]}
        (w/exec {:reader {:spec spec}
                 :init-state {:used-entrant-groups #{}}}
                gen)]
    (if error (->Fail error) (->Ok result))))

(ns tourney-time.core
  (:require
   [allpa.core :as a
    :refer [defprotomethod curry]]
   [allpa.linked-hash-map :as lhm]
   [wayra.core
    :refer [mdo defm]]
   [tourney-time.gamer :refer [->Gamer]]
   [tourney-time.tournament.gen :as g]
   [tourney-time.tournament.slots :as slots]
   [tourney-time.tournament.entrant-group
    :refer [->EntrantGroup]]
   [tourney-time.tournament.entrant-path :as ep]
   [tourney-time.tournament.elimination-round :as er]
   [tourney-time.tournament.match :as m]
   [tourney-time.tournament.match-path :as mp]
   [tourney-time.tournament.match-player :as mpl]
   [tourney-time.tournament.pool-path
    :refer [->PoolPath]]
   [tourney-time.tournament.rep :as r]
   [tourney-time.tournament.standing
    :refer [->Standing]]
   [tourney-time.tournament.builder.gen :as bg]
   [tourney-time.tournament.builder.rep :as br]
   [tourney-time.tournament.builder.spec :as bs]
   [reagent.core :as reagent :refer [atom]]
   [reagent.session :as session]
   [reitit.frontend :as reitit]
   [cljs.pprint :refer [pprint]]
   [clerk.core :as clerk]
   [accountant.core :as accountant]))

;; -------------------------
;; Routes

(def router
  (reitit/router
   [["/" :index]]))

(defn path-for [route & [params]]
  (if params
    (:path (reitit/match-by-name router route params))
    (:path (reitit/match-by-name router route))))

;; -------------------------
;; Page components

(defn home-page []
  (let [entrants (atom 16)
        bo5-round (atom 4)
        setups (atom 5)
        bo3-length (atom 10)
        bo5-length (atom 20)
        simple-spec (bs/build-spec (mdo (bs/-entrant-groups 1 [])
                                        (bs/-segment-spec (bs/-group-source 1)
                                                          (bs/-de-settings
                                                           []
                                                           {}))))
        run-with (fn [n results]
                   (let [gamers (vec (map ->Gamer (range n)))
                         spec (-> simple-spec
                                  (update :entrant-groups
                                          #(lhm/set %1
                                                    1
                                                    (->EntrantGroup gamers)))
                                  (assoc :results results))]
                     (g/to-tournament-gen spec)))]
    (fn []
      (let [matches-by-round (->> (run-with @entrants {})
                                  :result
                                  :segments
                                  first
                                  :pools
                                  first
                                  :matches
                                  vals
                                  (group-by #(mp/get-round (:path %1))))
            sorted-rounds (->> matches-by-round
                               keys
                               (sort-by er/to-int))
            round-ints (->> sorted-rounds
                            (map er/to-int))
            setup-data (->> (range @setups)
                            (reduce #(assoc %1 %2 {:id %2 :time 0}) {}))
            match-time (fn [{:keys [path]}]
                         (if (> (er/to-int (mp/get-round path)) @bo5-round)
                           @bo3-length
                           @bo5-length))

            pool-path (->PoolPath 1 0)
            final-data
            (loop [results {}
                   data setup-data]
              (let [matches (->> (run-with @entrants results)
                                 :result
                                 :segments
                                 first
                                 :pools
                                 first
                                 :matches
                                 vals
                                 (remove m/complete?)
                                 (filter m/reportable?))

                    played
                    (reduce #(let [{:keys [id time]} (apply min-key :time (vals %1))]
                               (update-in %1 [id :time] (curry + (match-time %2))))
                            data
                            matches)

                    max-time (->> played vals (apply max-key :time) :time)]
                (if (empty? matches)
                  data
                  (recur (reduce #(assoc-in %1 [pool-path (:path %2)] :up) results matches)
                         (a/map-values #(assoc %1 :time max-time) played)))))
            total-time (->> final-data
                            vals
                            (apply max-key :time)
                            :time)]
        (println {:bo3-length @bo3-length
                  :bo5-length @bo5-length
                  :entrants @entrants
                  :bo5-round @bo5-round
                  :setups @setups
                  :total-time total-time})
        [:div
         [:p "Total Tournament Time: " [:strong (str total-time)] " minutes."]
         [:p "Number of entrants: "
          [:input {:type "number"
                   :value @entrants
                   :on-change #(reset! entrants (int (-> % .-target .-value)))}]]
         [:p "Best of 5 Begins: "
          [:select {:value @bo5-round
                    :on-change #(reset! bo5-round (int (-> % .-target .-value)))}
           (for [round-int round-ints]
             ^{:key round-int}
             [:option {:value round-int} (er/label (er/from-int round-int))])]]
         [:p "Setups: "
          [:input {:type "number"
                   :value @setups
                   :on-change #(reset! setups (int (-> % .-target .-value)))}]]
         [:p "Best of 3 time (minutes): "
          [:input {:type "number"
                   :value @bo3-length
                   :on-change #(reset! bo3-length (int (-> % .-target .-value)))}]]
         [:p "Best of 5 time (minutes): "
          [:input {:type "number"
                   :value @bo5-length
                   :on-change #(reset! bo5-length (int (-> % .-target .-value)))}]]]))))


;; -------------------------
;; Translate routes -> page components

(defn page-for [route]
  (case route
    :index #'home-page))


;; -------------------------
;; Page mounting component

(defn current-page []
  (fn []
    (let [page (:current-page (session/get :route))]
      [:div [page]])))

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (clerk/initialize!)
  (accountant/configure-navigation!
   {:nav-handler
    (fn [path]
      (let [match (reitit/match-by-path router path)
            current-page (:name (:data  match))
            route-params (:path-params match)]
        (reagent/after-render clerk/after-render!)
        (session/put! :route {:current-page (page-for current-page)
                              :route-params route-params})
        (clerk/navigate-page! path)
        ))
    :path-exists?
    (fn [path]
      (boolean (reitit/match-by-path router path)))})
  (accountant/dispatch-current!)
  (mount-root))

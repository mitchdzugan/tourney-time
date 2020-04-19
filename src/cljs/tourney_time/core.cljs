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
        reset? (atom 1)
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
      (let [label #(er/label (er/from-int %) @entrants)
            matches-by-round (->> (run-with @entrants {})
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
            matches (->> sorted-rounds
                         reverse
                         (reduce #(conj %1 (get matches-by-round %2)) [])
                         flatten)
            final-data
            (->> matches
                 (reduce
                  (fn [{:keys [data times]} {:keys [slots path] :as match}]
                    (let [slot-time #(if (mpl/filled? %)
                                       0
                                       (get times (:match-path %)))
                          earliest (max (slot-time (:up slots))
                                        (slot-time (:down slots)))
                          setups (vals data)
                          readies (remove #(> (:time %1) earliest) setups)
                          setup (if (empty? readies)
                                  (apply min-key :time setups)
                                  (apply max-key :time readies))
                          time (+ (:time setup) (match-time match))]
                      {:data (assoc-in data [(:id setup) :time] time)
                       :times (assoc times path time)}))
                  {:data setup-data :times {}})
                 :data)
            total-time (->> final-data
                            vals
                            (apply max-key :time)
                            :time
                            (+ (quot (* @bo5-length @reset?) 2)))
            color #(if (> %1 @bo5-round)
                     "#c3e6e1"
                     "#3fe8d0")

            render-matches
            (fn [round-int]
              (let [matches (get matches-by-round (er/from-int round-int))
                    match-count (count matches)
                    height (+ 30 (* 3 match-count))
                    rma (atom (fn []))
                    render-match
                    #(when (not= %1 match-count)
                       [:div {:style {:height "30px"
                                      :width "100%"
                                      :position "relative"
                                      :transform "translateY(3px)"}}
                        [:div {:style {:height "30px"
                                       :width "100%"
                                       :position "absolute"
                                       :top "0"
                                       :left "0"
                                       :border-radius "5px"
                                       :border "1px solid #4b5957"
                                       :background (color round-int)}}]
                        (@rma (inc %1))])]
                (reset! rma render-match)
                [:div {:style {:height (str height "px") :width "125px"
                               :position "relative"}}
                 (render-match 0)]))]
        (println {:bo3-length @bo3-length
                  :bo5-length @bo5-length
                  :entrants @entrants
                  :bo5-round @bo5-round
                  :setups @setups
                  :total-time total-time})
        [:div
         [:div.container
          [:h2 {:style {:text-align "center"}} "Tourney Time Calc"]
          [:p "Number of entrants: "
           [:input {:type "number"
                    :value @entrants
                    :on-change #(reset! entrants (int (-> % .-target .-value)))}]]
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
                    :on-change #(reset! bo5-length (int (-> % .-target .-value)))}]]
          [:p "Reset? "
           [:select {:value @reset?
                     :on-change #(reset! reset? (int (-> % .-target .-value)))}
            [:option {:value 1} "50/50"]
            [:option {:value 0} "No"]
            [:option {:value 2} "Yes"]]]
          [:p "Total Tournament Time: " [:strong (str total-time)] " minutes."]]
         [:div
          [:h4 {:style {:text-align "center"
                        :margin "20px 0 10px 0"}}
           "Set First Best Of 5 Round"]
          [:div {:style {:display "flex"
                         :flex-direction "column"
                         :margin "5px"
                         :padding "15px"
                         :box-shadow "0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19)"
                         :overflow "scroll"}}
           [:div {:style {:display "flex" :flex-direction "row"}}
            (for [round-int (->> round-ints
                                 reverse
                                 (map er/from-int)
                                 (filter er/upper-bracket?)
                                 (map er/to-int))]
              ^{:key round-int}
              [:div {:style {:display "flex" :flex-direction "column" :margin "5px"
                             :position "relative" :align-items "center"}
                     :on-click #(reset! bo5-round round-int)}
               [:div {:style {:white-space "nowrap"}} [:strong (label round-int)]]
               (render-matches round-int)])]
           [:div {:style {:display "flex" :flex-direction "row"}}
            (for [round-int (->> round-ints
                                 reverse
                                 (map er/from-int)
                                 (remove er/upper-bracket?)
                                 (map er/to-int))]
              ^{:key round-int}
              [:div {:style {:display "flex" :flex-direction "column" :margin "5px"
                             :position "relative" :align-items "center"}
                     :on-click #(reset! bo5-round round-int)}
               [:div {:style {:white-space "nowrap"}} [:strong (label round-int)]]
               (render-matches round-int)])]]]]))))


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

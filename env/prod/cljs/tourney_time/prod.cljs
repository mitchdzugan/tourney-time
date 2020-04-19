(ns tourney-time.prod
  (:require [tourney-time.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)

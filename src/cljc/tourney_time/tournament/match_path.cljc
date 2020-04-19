(ns tourney-time.tournament.match-path
  (:require [allpa.core :as a
             :refer [p2 defprotomethod]]
            [tourney-time.tournament.elimination-round :as er
             :refer [upper-bracket?]]))

(defrecord EliminationPath [round row])

(def get-round :round)

(defn lower-bracket? [path]
  (not (upper-bracket? (get-round path))))

(defprotomethod to-int-round-base [{:keys [depth drop-round?]} de?]
  !er/GrandsReset -2
  !er/Grands -1
  !er/Winners
  (cond (and de? (= 0 depth)) 1
        (= 0 depth) -1
        de?
        (-> (er/->Losers depth true)
            (to-int-round-base true)
            (+ (p2 depth)))
        :else
        (dec (p2 (dec depth))))
  !er/Losers
  (cond (and drop-round? (= 0 depth)) 0
        (= 0 depth) 2
        drop-round?
        (- (quot (* 3 (p2 depth)) 2) 3)
        :else
        (-> (er/->Winners depth)
            (to-int-round-base false)
            (+ (p2 depth)))))

(defn to-int [{:keys [round row]} de?]
  (+ row (to-int-round-base round de?)))

(defn alpha-num [n de?]
  (cond
    (= n -2) "GFR"
    (= n -1) "GF"
    (< n 187) (a/alpha-num n)
    (or (not de?)
        (< n 4904)) (a/alpha-num (+ n 1))
    :else (a/alpha-num (+ n 2))))

(defn unique-name [path de?]
  (let [n (to-int path de?)]
    (alpha-num n de?)))

(comment "
These arrays are used to correct the order of matches in an elimination round
to be based on seed. For example if you knew that the winners of round 2
of winner's bracket were going to be progression onwards, we want an ordering
of the matches in that round such that assuming there are no upsets, the matches
are ordered by seed of their winners.
These functions give us mapping froms the visual array index order (which is
what we assume and use in most contexts) and the seed order version. Let's look
at the winners round 2 example. Assuming no upsets, these will be the matches in
that round in visual order:
  Winners 2 0
    Winner $ seed 0
    Loser  $ seed 7
  Winners 2 1
    Winner $ seed 3
    Loser  $ seed 4
  Winners 2 2
    Winner $ seed 1
    Loser  $ seed 6
  Winners 2 3
    Winner $ seed 2
    Loser  $ seed 5
So if I took the winning seeds in visual order, I would end up with:
  [seed 0, seed 3, seed 1, seed 2]
This is where these functions can help us.
  wbRows 2
    -- == [ 0, 2, 3, 1 ]
If you replace the integers in that array with the seed at that index
from the first array, you will end up with an array that has the seeds
in the right order.
This algorithm works in O(n) time and it can be transferred back to
visual order in O(n) time as well. where n is the number of sets
in the round.
These functions make it work that way for all rounds. I'm not entirely
sure why it all works, I just figured out these functinos by studying
the patterns in test cases. Below I have listed out what rounds 1-4
look like for all round variants. The left side is the no upset seeds
of the visual order and on the right is what the mapping must be. Its
spread out on the right to help demonstrate the patterns. You can see
that in every case when you increase the round, the current values stay
in place and then a value is added either to the right or left of them
that is themselves incremented by 2^(round - 1)
  IE going from round 1 to round 2 of winners right below. For round 2
     the incrementer will be 2^(2-1) == 2 and we can see that (0 + 2) = 2
     gets added to the right of 0 and (1 + 2) = 3 gets added to the left
     of 1.
This alternating Left/Right pattern is present in all of them and its what
makes the wavy patterns in the right side representation. This happens in
the code by just recursively expanding an array by adding its new neighbor
for that round and recursing.
In the code these are the [v, add + v]
                       or [add + v, v] sections each representing a single
expand with the rest of the logic being selecting between adding to the
left or right based on the wave pattern. That was mostly done through trial
and error with the unit tests.
winners
1: 0 1                                                  0                                            1
2: 0 3 1 2                                              0                    2  3                    1
3: 0 7 3 4 1 6 2 5                                      0        4  6        2  3        7  5        1
4: 0  15 7  8  3  12 4  11 1  14 6  9  2  13 5  10      0  8  12 4  6  14 10 2  3  11 15 7  5  13 9  1
   0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15
losers drop rounds
1t: 3  2                                                                 1        0
2t: 6  5  7  4                                            3              1        0              2
3t: 12 11 15 8  13 10 14 9                                3  7        5  1        0  4        6  2
4t: 25 22 30 17 26 21 29 18 24 23 31 16 27 20 28 19    11 3  7  15 13 5  1  9  8  0  4  12 14 6  2  10
    0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15
losers non-drop rounds
1f: 5  4                                                        1                          0
2f: 11 8  10 9                                                  1  3                    2  0
3f: 22 17 21 18 23 16 20 19                            5        1  3        7  6        2  0       4
4f: 44 35 43 36 47 32 40 39 45 34 42 37 46 33 41 38    5  13 9  1  3  11 15 7  6  14 10 2  0  8 12 4
    0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15
")

(defn wb-rows [depth]
  (case depth
    0 [0]
    1 [0 1]
    (-> (fn [v1 ind]
          (let [add (p2 (dec depth))
                v2 (+ add v1)]
            (if (odd? ind) [v2 v1] [v1 v2])))
        (mapcat (wb-rows (dec depth)) (range))
        vec)))
(defn lb-rows [flip-parity? depth]
  (case depth
    0 [1]
    1 [1 0]
    (-> (fn [v1 ind]
          (let [add (p2 (dec depth))
                v2 (+ add v1)
                flip1 (if (odd? depth) not identity)
                flip2 (if flip-parity? not identity)]
            (if (flip2 (flip1 (odd? ind))) [v2 v1] [v1 v2])))
        (mapcat (lb-rows flip-parity? (dec depth)) (range))
        vec)))

(defprotomethod get-seed-order-round-paths [{:keys [depth
                                                    drop-round?] :as round}]
  [!er/GrandsReset !er/Grands] [(->EliminationPath round 0)]
  !er/Winners (->> (wb-rows depth)
                   (map (partial ->EliminationPath round)))
  !er/Losers (->> (lb-rows drop-round? depth)
                  (map (partial ->EliminationPath round))))

(defprotomethod get-round-paths [{:keys [depth drop-round?] :as round}]
  [!er/GrandsReset !er/Grands] [(->EliminationPath round 0)]
  [!er/Winners !er/Losers] (->> depth p2 range
                                (map (partial ->EliminationPath round))))

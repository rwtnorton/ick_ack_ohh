(ns ick-ack-ohh.core
  (:require [clojure.string :as str-ops])
  (:require [clojure.set :as set-ops]))

;;
;; Represent a game board as a 3x3 vector-of-vectors,
;; where each position on the board is a zero-indexed
;; vector pair (e.g., [0 0], [0 1]), and where each position
;; holds one of the following marks:
;;   :x, :o, or :_
;;

(defn new-board
  "Returns an empty 3x3 board."
  []
  (vec (for [i (range 3)]
         (vec (repeat 3 :_)))))

(defn mark->string
  "Returns a string representation for a board mark."
  [m]
  (get {:x "X", :o "O", :_ " "} m "?"))

(defn board->string
  "Returns string representation of a board."
  [b]
  (let [rows (for [row b]
               (str-ops/join "|" (map mark->string row)))
        rows (interpose "-+-+-" rows)
        rows (map (fn [s] (.concat s "\n")) rows)]
    (reduce (fn [acc s]
              (.concat acc s))
            rows)))

(defn print-board
  "Writes string repr of board to stdout."
  [b]
  (print (board->string b)))

(defn value-at
  "Returns the value for position."
  [board [row-index col-index]]
  ((board row-index) col-index))

(defn mark-at?
  "Returns true iff position has X or O."
  [board pos]
  (let [v (value-at board pos)]
    (or (= v :x)
        (= v :o))))

(defn open-at?
  "Returns true iff mark-at? returns false for position."
  [board pos]
  (not (mark-at? board pos)))

(defn place-mark-at
  "Returns new board with mark applied at position."
  [board mark [row-index col-index]]
  (if (mark-at? board [row-index col-index])
    board ;; ignore requests to clobber occupied positions.
    (let [row (board row-index)
          row (assoc row col-index mark)]
      (vec
       (map-indexed (fn [i r] (if (= i row-index) row r))
                    board)))))

(defn positions
  "Returns seq of all positions for board."
  [board]
  (let [ps (map-indexed (fn [row-index row]
                          (map (fn [col-index] [row-index col-index])
                               (range (count row))))
                        board)]
    (apply concat ps)))

(defn open-positions
  "Returns seq of all open positions on board."
  [board]
  (filter (fn [pos]
            (open-at? board pos))
          (positions board)))

(defn neighborhood
  "Returns set of positions logically adjacent with pos, independent of board."
  [[row col]]
  (let [nbhd (for [r (map (fn [d] (+ row d)) [-1 0 1])
                   c (map (fn [d] (+ col d)) [-1 0 1])]
               [r c])]
    (set-ops/difference (set nbhd) #{[row col]})))

(defn neighbors?
  "Returns true iff pos1 is logically adjacent with pos2, independent of board."
  [pos1 pos2]
  (contains? (neighborhood pos1) pos2))

(defn neighboring-positions
  "Returns set of positions adjacent with with pos with respect to board."
  [board pos]
  (let [ps (set (positions board))
        ns (neighborhood pos)]
    (set-ops/intersection ns ps)))

(defn -main
  [& args]
  (doseq [s ["Ick!" "Ack!!" "Ohh..."]]
    (println s)))

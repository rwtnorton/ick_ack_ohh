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

(defn winning-positionings
  "Returns set of sets of positions crucial to a win."
  [board]
  ;; Hard-coded for 3x3.
  #{ #{[0 0] [0 1] [0 2]}
     #{[1 0] [1 1] [1 2]}
     #{[2 0] [2 1] [2 2]}
     #{[0 0] [1 0] [2 0]}
     #{[0 1] [1 1] [2 1]}
     #{[0 2] [1 2] [2 2]}
     #{[0 0] [1 1] [2 2]}
     #{[0 2] [1 1] [2 0]} })

(defn full-board?
  "Returns true iff board has no open positions left."
  [board]
  (empty? (open-positions board)))

(defn -win-for?
  [board mark]
  (letfn [(winners->marks [winners]
            (map (fn [p] (value-at board p)) winners))
          (all-x? [marks]
            (every? (fn [m] (= m mark)) marks))]
    (boolean
     (some (fn [winners] (all-x? (winners->marks winners)))
           (winning-positionings board)))))

(defn win-for-x?
  [board]
  (-win-for? board :x))

(defn win-for-o?
  [board]
  (-win-for? board :o))

(defn cat?
  [board]
  (and (full-board? board)
       (not (win-for-x? board))
       (not (win-for-o? board))))


(defn finishing-moves-for
  [board mark]
  (letfn [(extract-open-pos [ms]
            (map (fn [m] (first (:_ m))) ms))
          (group-by-marks [ps]
            (group-by (fn [p] (value-at board p)) ps))
          (poised? [ps-by-marks]
            (let [mark-count (count (mark ps-by-marks))
                  open-count (count (:_ ps-by-marks))]
              (and (= 1 open-count) (= 2 mark-count))))]
    (set
     (extract-open-pos
      (filter (fn [ps-by-marks] (poised? ps-by-marks))
              (map (fn [ps] (group-by-marks ps))
                   (winning-positionings board)))))))

(defn fork-moves-for
  [board mark]
  (set
   (filter (fn [p]
             (let [b (place-mark-at board mark p)
                   win-ps (finishing-moves-for b mark)]
               (>= (count win-ps) 2)))
           (open-positions board))))

(defn fork-for?
  [board mark]
  (>= (count (finishing-moves-for board mark))
      2))

(defn opponent-mark-for
  [mark]
  (cond (= mark :x) :o
        (= mark :o) :x
        :else :?))

(defn open-center?
  [board]
  (not (empty? (set-ops/intersection
                (set (open-positions board))
                #{[1 1]}))))

(defn corner-positions
  [board]
  #{[0 0] [0 2] [2 0] [2 2]})

(defn open-corners
  [board]
  (set-ops/intersection (set (open-positions board))
                        (corner-positions board)))

(defn choose-next-move-for
  [board mark]
  (let [win-ps (finishing-moves-for board mark)
        opp-mark (opponent-mark-for mark)
        opp-win-ps (finishing-moves-for board opp-mark)
        fork-ps (fork-moves-for board mark)
        opp-fork-ps (fork-moves-for board opp-mark)
        corner-fork-block-ps (set-ops/intersection opp-fork-ps
                                                   (corner-positions board))
        corners (open-corners board)]
    (cond (not (empty? win-ps)) (first win-ps)
          (not (empty? opp-win-ps)) (first opp-win-ps)
          (not (empty? fork-ps)) (first fork-ps)
          (not (empty? corner-fork-block-ps)) (first corner-fork-block-ps)
          (not (empty? opp-fork-ps)) (first opp-fork-ps)
          (open-center? board) [1 1]
          (not (empty? corners)) (first corners)
          :else (first (open-positions board)))))

(def config {:x :bot, ;:human,
             :o :bot})

(defn -main
  [& args]
  (letfn [(parse-position [s]
            (vec
             (map (fn [s] (Integer/parseInt s))
                  (clojure.string/split s #"\D+"))))
          (prompt []
            (print "Enter row and column (zero-indexed): ")
            (flush)
            (read-line))
          (game-loop [board mark]
            (print-board board)
            (cond (win-for-x? board) (println "X wins.")
                  (win-for-o? board) (println "O wins.")
                  (cat? board) (println "Cat wins.")
                  :else (let [opp-mark (opponent-mark-for mark)
                              p (if (= (mark config) :human)
                                  (parse-position (prompt))
                                  (do (println)
                                      (choose-next-move-for board
                                                            opp-mark)))
                              b (place-mark-at board mark p)]
                          (recur b opp-mark))))]
    (game-loop (new-board) :x)))

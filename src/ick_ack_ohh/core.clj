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

(def config {:x :human,
             :o :bot})

(declare board->string
         cat?
         center-position
         choose-next-move-for
         corner-positions
         finishing-moves-for
         fork-for?
         fork-moves-for
         full-board?
         mark->string
         mark-at?
         new-board
         open-at?
         open-center?
         open-corners
         open-positions
         opponent-mark-for
         parse-position
         place-mark-at
         positions
         print-board
         value-at
         win-for-o?
         win-for-x?
         winning-positionings)

(defn -main
  [& args]
  (letfn [(prompt []
            (print "Enter position: ")
            (flush)
            (let [v (read-line)]
              (if (or (nil? v) ;; Something like Ctrl-D.
                      (= v "exit")
                      (= v "quit"))
                (do (println "\nBye.  Thanks for playing.")
                    (System/exit 0))
                v)))
          (game-loop [board mark]
            (print-board board)
            (cond (win-for-x? board) (println "X wins.")
                  (win-for-o? board) (println "O wins.")
                  (cat? board) (println "Cat wins.")
                  :else (let [opp-mark (opponent-mark-for mark)
                              p (if (= (mark config) :human)
                                  (parse-position (prompt))
                                  (do (println)
                                      (choose-next-move-for board mark)))]
                          (cond (not p)
                                (do (println "Invalid input.  Please try again.")
                                    (recur board mark))

                                (not (contains? (set (positions board)) p))
                                (do (println "Invalid position.  Please try again.")
                                    (recur board mark))

                                (mark-at? board p)
                                (do (println "Position already taken.  Please try again.")
                                    (recur board mark))

                                :else (let [b (place-mark-at board mark p)]
                                        (recur b opp-mark))))))]
    (println "Tic Tac Toe.  Enter 'exit' or 'quit' to quit.  Valid positions:")
    (println "1|2|3\n-+-+-\n4|5|6\n-+-+-\n7|8|9\n")
    (game-loop (new-board) :x)))
    ;(game-loop (place-mark-at (new-board) :x (rand-nth (positions (new-board)))) :o)))

(defn parse-position
  [s]
  (let [x (re-matches #"\A\s*([1-9])\s*\z" s)]
    (if x
      (let [n (Integer/parseInt (x 1))
            row (quot (- n 1) 3)
            col (rem  (- n 1) 3)]
        [row col])
      nil)))

(defn new-board
  "Returns an empty 3x3 board."
  []
  ;; Hard-coded for 3x3.
  (vec (for [i (range 3)]
         (vec (repeat 3 :_)))))

(defn choose-next-move-for
  "Returns position that given player should choose next."
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
          (open-center? board) (center-position board)
          (not (empty? corners)) (first corners)
          :else (first (open-positions board))))) ;; So, nil if board is full.

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

(defn -win-for?
  [board mark]
  (letfn [(winners->marks [winners]
            (map (fn [p] (value-at board p)) winners))
          (all-marks? [marks]
            (every? (fn [m] (= m mark)) marks))]
    (boolean
     (some (fn [winners] (all-marks? (winners->marks winners)))
           (winning-positionings board)))))

(defn win-for-x?
  "Returns true iff X has won."
  [board]
  (-win-for? board :x))

(defn win-for-o?
  "Returns true iff O has won."
  [board]
  (-win-for? board :o))

(defn cat?
  "Returns true iff the game is a draw."
  [board]
  (and (full-board? board)
       (not (win-for-x? board))
       (not (win-for-o? board))))

(defn finishing-moves-for
  "Returns set of positions which would result in an immediate win for mark."
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
  "Returns set of positions which would cause a fork for mark."
  [board mark]
  (set
   (filter (fn [p]
             (let [b (place-mark-at board mark p)
                   win-ps (finishing-moves-for b mark)]
               (>= (count win-ps) 2)))
           (open-positions board))))

(defn fork-for?
  "Returns true iff there is a fork on the board for given mark."
  [board mark]
  (>= (count (finishing-moves-for board mark))
      2))

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

(defn full-board?
  "Returns true iff board has no open positions left."
  [board]
  (empty? (open-positions board)))

(defn opponent-mark-for
  "Returns keyword mark of opponent."
  [mark]
  (cond (= mark :x) :o
        (= mark :o) :x
        :else :?))

(defn center-position
  "Returns position for center of board."
  [board]
  ;; Hard-coded for 3x3.
  [1 1])

(defn open-center?
  "Returns true iff center position of board is unoccupied."
  [board]
  ;; Hard-coded for 3x3.
  (not (empty? (set-ops/intersection
                (set (open-positions board))
                #{(center-position board)}))))

(defn corner-positions
  "Returns set of positions at the corners of board."
  [board]
  ;; Hard-coded for 3x3.
  #{[0 0] [0 2] [2 0] [2 2]})

(defn open-corners
  "Returns set of unoccupied corner positions."
  [board]
  (set-ops/intersection (set (open-positions board))
                        (corner-positions board)))

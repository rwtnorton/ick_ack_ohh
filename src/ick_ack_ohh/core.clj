(ns ick-ack-ohh.core
  (:require [clojure.string :as str]))

;; Represent a game board as a 3x3 vector-of-vectors,
;; where each position on the board is one of:
;;   :x, :o, or :_

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
               (str/join "|" (map mark->string row)))
        rows (interpose "-+-+-" rows)
        rows (map (fn [s] (.concat s "\n")) rows)]
    (reduce (fn [acc s]
              (.concat acc s))
            rows)))

(defn mark-at?
  "Returns true iff position has X or O."
  [board row-index col-index]
  (let [v ((board row-index) col-index)]
    (or (= v :x)
        (= v :o))))

(defn place-mark-at
  "Returns new board with mark applied at position."
  [board mark row-index col-index]
  (let [row (board row-index)
        row (assoc row col-index mark)]
    (vec
     (map (fn [i]
            (let [r (board i)]
              (if (= i row-index)
                row
                r)))
          (range (count board))))))

(defn -main
  [& args]
  (doseq [s ["Ick!" "Ack!!" "Ohh..."]]
    (println s)))

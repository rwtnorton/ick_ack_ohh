(ns ick-ack-ohh.core
  (:require [clojure.string :as str]))

;; Represent a game board as a 3x3 vector-of-vectors,
;; where each position on the board is one of:
;;   :x, :o, or :_

(defn new-board
  []
  (vec (for [i (range 3)]
         (vec (repeat 3 :_)))))

(defn mark->string
  [m]
  (get {:x "X", :o "O", :_ " "} m "?"))

(defn board->string
  [b]
  (let [rows (for [row b]
               (str/join "|" (map mark->string row)))
        rows (interpose "-+-+-" rows)
        rows (map (fn [s] (.concat s "\n")) rows)]
    (reduce (fn [acc s]
              (.concat acc s))
            rows)))

(defn mark-at?
  [board row-index col-index]
  (let [v ((board row-index) col-index)]
    (or (= v :x)
        (= v :o))))

(defn place-mark-at
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

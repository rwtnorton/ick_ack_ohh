(ns ick-ack-ohh.core-test
  (:require [clojure.test :refer :all]
            [ick-ack-ohh.core :refer :all]))

(deftest new-board-test
  (testing "new-board is a 3x3 vector of vectors filled with :_"
    (is (= (new-board)
           [[:_ :_ :_]
            [:_ :_ :_]
            [:_ :_ :_]]))))

(deftest test-mark-stringifier
  (testing ":x => \"X\", :o => \"O\", :_ => \" \", otherwise \"?\""
    (is (= (map mark->string [:x :o :_ \X "X" nil :hmm])
           ["X" "O" " " "?" "?" "?" "?"]))))

(deftest test-stringifier-for-new-board
  (testing "stringifies with expected format from new board"
    (is (= (board->string (new-board))
           " | | \n-+-+-\n | | \n-+-+-\n | | \n"))))

(deftest test-stringifier-for-non-new-board
  (testing "stringifies with expected format from played-on board"
    (is (= (board->string [[:x :o :_] [:o :o :x] [:_ :x :_]])
           "X|O| \n-+-+-\nO|O|X\n-+-+-\n |X| \n"))))

(deftest test-value-at
  (testing "returns contents of board at positions"
    (let [b [[:x :o :_] [:o :o :x] [:_ :x :_]]]
      (is (= (map (fn [p] (value-at b p))
                  [[0 0] [0 1] [0 2]
                   [1 0] [1 1] [1 2]
                   [2 0] [2 1] [2 2]])
             [:x :o :_
              :o :o :x
              :_ :x :_])))))

(deftest test-mark-at?
  (testing "returns true iff :o or :x at position"
    (let [b [[:x :o :_] [:o :o :x] [:_ :x :_]]]
      (is (= (map (fn [p] (mark-at? b p))
                  [[0 0] [0 1] [0 2]
                   [1 0] [1 1] [1 2]
                   [2 0] [2 1] [2 2]])
             [true  true  false
              true  true  true
              false true  false])))))

(deftest test-open-at?
  (testing "returns false iff :o or :x at position"
    (let [b [[:x :o :_] [:o :o :x] [:_ :x :_]]]
      (is (= (map (fn [p] (open-at? b p))
                  [[0 0] [0 1] [0 2]
                   [1 0] [1 1] [1 2]
                   [2 0] [2 1] [2 2]])
             [false false true
              false false false
              true  false true])))))

(deftest test-place-mark-at-on-new-board
  (testing "builds another board with mark at position"
    (let [b (new-board)]
      (is (= (map (fn [p] (place-mark-at b :x p))
                  [[0 0] [0 1] [0 2]
                   [1 0] [1 1] [1 2]
                   [2 0] [2 1] [2 2]])
             [ [[:x :_ :_][:_ :_ :_][:_ :_ :_]]
               [[:_ :x :_][:_ :_ :_][:_ :_ :_]]
               [[:_ :_ :x][:_ :_ :_][:_ :_ :_]]
               [[:_ :_ :_][:x :_ :_][:_ :_ :_]]
               [[:_ :_ :_][:_ :x :_][:_ :_ :_]]
               [[:_ :_ :_][:_ :_ :x][:_ :_ :_]]
               [[:_ :_ :_][:_ :_ :_][:x :_ :_]]
               [[:_ :_ :_][:_ :_ :_][:_ :x :_]]
               [[:_ :_ :_][:_ :_ :_][:_ :_ :x]] ])))))

(deftest test-place-mark-at-on-used-board
  (testing "builds another board with mark at position"
    (let [b [[:_ :x :o][:_ :_ :_][:_ :_ :_]]]
      (is (= (map (fn [p] (place-mark-at b :x p))
                  [[0 0] [0 1] [0 2]
                   [1 0] [1 1] [1 2]
                   [2 0] [2 1] [2 2]])
             [ [[:x :x :o][:_ :_ :_][:_ :_ :_]]
               [[:_ :x :o][:_ :_ :_][:_ :_ :_]] ;; ignores clobber
               [[:_ :x :o][:_ :_ :_][:_ :_ :_]] ;; ignores clobber
               [[:_ :x :o][:x :_ :_][:_ :_ :_]]
               [[:_ :x :o][:_ :x :_][:_ :_ :_]]
               [[:_ :x :o][:_ :_ :x][:_ :_ :_]]
               [[:_ :x :o][:_ :_ :_][:x :_ :_]]
               [[:_ :x :o][:_ :_ :_][:_ :x :_]]
               [[:_ :x :o][:_ :_ :_][:_ :_ :x]] ])))))

(deftest test-positions-for-new-board
  (testing "returns seq of all [row col] positions for new board"
    (is (= (positions (new-board))
           [[0 0] [0 1] [0 2]
            [1 0] [1 1] [1 2]
            [2 0] [2 1] [2 2]]))))

(deftest test-positions-for-used-board
  (testing "returns seq of all [row col] positions for used board"
    (is (= (positions [[:x :o :x][:_ :x :o][:_ :_ :o]])
           [[0 0] [0 1] [0 2]
            [1 0] [1 1] [1 2]
            [2 0] [2 1] [2 2]]))))

(deftest test-positions-same-for-new-as-used-board
  (testing "positions are independent of marks of boards"
    (is (= (positions [[:x :o :x][:_ :x :o][:_ :_ :o]])
           (positions (new-board))))))

(deftest test-open-positions-for-new-board
  (testing "all positions of new board are open positions"
    (let [b (new-board)]
      (is (= (open-positions b)
             (positions b))))))

(deftest test-open-positions-for-used-board
  (testing "returns only positions marked with :_"
    (is (= (open-positions [[:x :o :x][:_ :x :o][:_ :_ :o]])
           [[1 0] [2 0] [2 1]]))))

(deftest test-open-positions-for-full-board
  (testing "returns empty seq for board lacking :_ marks"
    (is (= (open-positions [[:x :o :x][:o :x :o][:x :x :o]])
           []))))

(deftest test-neighboring-positions-of-top-left
  (testing "returns set of positions next to [0 0]"
    (is (= (neighboring-positions (new-board) [0 0])
           #{[0 1] [1 0] [1 1]}))))

(deftest test-neighboring-positions-of-center
  (testing "returns set of positions next to [1 1]"
    (is (= (neighboring-positions (new-board) [1 1])
           #{ [0 0] [0 1] [0 2]
              [1 0]       [1 2]
              [2 0] [2 1] [2 2] }))))

(deftest test-winning-positionings
  (testing ""
    (is (= (winning-positionings (new-board))
           #{ #{[0 0] [0 1] [0 2]}
              #{[1 0] [1 1] [1 2]}
              #{[2 0] [2 1] [2 2]}
              #{[0 0] [1 0] [2 0]}
              #{[0 1] [1 1] [2 1]}
              #{[0 2] [1 2] [2 2]}
              #{[0 0] [1 1] [2 2]}
              #{[0 2] [1 1] [2 0]} }))))

(deftest test-win-for-x?
  (testing "with new board"
    (is (not (win-for-x? (new-board)))))
  (testing "with :x's across top row"
    (is (win-for-x? [[:x :x :x] [:_ :_ :_] [:_ :_ :_]])))
  (testing "with :o's down middle"
    (is (not (win-for-x? [[:x :o :_] [:_ :o :_] [:_ :o :x]]))))
  (testing "with :x's down rightmost column"
    (is (win-for-x? [[:_ :_ :x] [:_ :_ :x] [:_ :_ :x]])))
  (testing "with :x's along top-left-to-bottom-right diagonal"
    (is (win-for-x? [[:x :_ :_] [:_ :x :_] [:_ :_ :x]])))
  (testing "with :x's along top-right-to-bottom-left diagonal"
    (is (win-for-x? [[:_ :_ :x] [:_ :x :_] [:x :_ :_]]))))

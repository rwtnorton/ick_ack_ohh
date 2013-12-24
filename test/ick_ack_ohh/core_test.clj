(ns ick-ack-ohh.core-test
  (:require [clojure.test :refer :all]
            [ick-ack-ohh.core :refer :all]))

(deftest test-new-board
  (is (= (new-board)
         [[:_ :_ :_]
          [:_ :_ :_]
          [:_ :_ :_]])))

(deftest test-mark-stringifier
  (is (= (map mark->string [:x :o :_ \X "X" nil :hmm])
         ["X" "O" " " "?" "?" "?" "?"])))

(deftest test-board-stringifier
  (testing "for new board"
    (is (= (board->string (new-board))
           " | | \n-+-+-\n | | \n-+-+-\n | | \n")))
  (testing "for non-new board"
    (is (= (board->string [[:x :o :_] [:o :o :x] [:_ :x :_]])
           "X|O| \n-+-+-\nO|O|X\n-+-+-\n |X| \n"))))

(deftest test-value-at
  (let [b [[:x :o :_] [:o :o :x] [:_ :x :_]]]
    (is (= (map (fn [p] (value-at b p))
                [[0 0] [0 1] [0 2]
                 [1 0] [1 1] [1 2]
                 [2 0] [2 1] [2 2]])
           [:x :o :_
            :o :o :x
            :_ :x :_]))))

(deftest test-mark-at?
  (let [b [[:x :o :_] [:o :o :x] [:_ :x :_]]]
    (is (= (map (fn [p] (mark-at? b p))
                [[0 0] [0 1] [0 2]
                 [1 0] [1 1] [1 2]
                 [2 0] [2 1] [2 2]])
           [true  true  false
            true  true  true
            false true  false]))))

(deftest test-open-at?
  (let [b [[:x :o :_] [:o :o :x] [:_ :x :_]]]
    (is (= (map (fn [p] (open-at? b p))
                [[0 0] [0 1] [0 2]
                 [1 0] [1 1] [1 2]
                 [2 0] [2 1] [2 2]])
           [false false true
            false false false
            true  false true]))))

(deftest test-place-mark-at
  (testing "with new board"
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
               [[:_ :_ :_][:_ :_ :_][:_ :_ :x]] ]))))
  (testing "with non-new board"
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

(deftest test-positions
  (testing "for new board"
    (is (= (positions (new-board))
           [[0 0] [0 1] [0 2]
            [1 0] [1 1] [1 2]
            [2 0] [2 1] [2 2]])))
  (testing "for non-new board"
    (is (= (positions [[:x :o :x][:_ :x :o][:_ :_ :o]])
           [[0 0] [0 1] [0 2]
            [1 0] [1 1] [1 2]
            [2 0] [2 1] [2 2]])))
  (testing "showing positions are independent of marks of boards"
    (is (= (positions [[:x :o :x][:_ :x :o][:_ :_ :o]])
           (positions (new-board))))))

(deftest test-open-positions
  (testing "for new board"
    (let [b (new-board)]
      (is (= (open-positions b)
             (positions b)))))
  (testing "for non-new board"
    (is (= (open-positions [[:x :o :x][:_ :x :o][:_ :_ :o]])
           [[1 0] [2 0] [2 1]])))
  (testing "for fully marked board"
    (is (= (open-positions [[:x :o :x][:o :x :o][:x :x :o]])
           []))))

(deftest test-winning-positionings
  (is (= (winning-positionings (new-board))
         #{ #{[0 0] [0 1] [0 2]}
            #{[1 0] [1 1] [1 2]}
            #{[2 0] [2 1] [2 2]}
            #{[0 0] [1 0] [2 0]}
            #{[0 1] [1 1] [2 1]}
            #{[0 2] [1 2] [2 2]}
            #{[0 0] [1 1] [2 2]}
            #{[0 2] [1 1] [2 0]} })))

(deftest test-full-board?
  (testing "for new board"
    (is (not (full-board? (new-board)))))
  (testing "for fully marked board"
    (is (full-board? [[:x :x :x] [:o :o :o] [:x :o :x]])))
  (testing "for non-new but not fully marked board"
    (is (not (full-board? [[:x :x :x] [:o :o :o] [:x :o :_]])))))

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

(deftest test-win-for-o?
  (testing "with new board"
    (is (not (win-for-o? (new-board)))))
  (testing "with :o's across top row"
    (is (win-for-o? [[:o :o :o] [:_ :_ :_] [:_ :_ :_]])))
  (testing "with :x's down middle"
    (is (not (win-for-o? [[:o :x :_] [:_ :x :_] [:_ :x :o]]))))
  (testing "with :o's down rightmost column"
    (is (win-for-o? [[:_ :_ :o] [:_ :_ :o] [:_ :_ :o]])))
  (testing "with :o's along top-left-to-bottom-right diagonal"
    (is (win-for-o? [[:o :_ :_] [:_ :o :_] [:_ :_ :o]])))
  (testing "with :o's along top-right-to-bottom-left diagonal"
    (is (win-for-o? [[:_ :_ :o] [:_ :o :_] [:o :_ :_]]))))

(deftest test-cat?
  (testing "for new board"
    (is (not (cat? (new-board)))))
  (testing "for x-won, non-full board"
    (is (not (cat? [[:x :x :x] [:_ :o :o] [:_ :_ :_]]))))
  (testing "for no-winner, full board"
    (is (cat? [[:x :o :o] [:o :x :x] [:x :o :o]]))))

(deftest test-finishing-moves-for
  (testing "for new board"
    (is (empty? (finishing-moves-for (new-board) :x))))
  (testing "for non-finishing-move board"
    (is (empty? (finishing-moves-for [[:x :_ :_]
                                      [:o :_ :o]
                                      [:o :x :_]] :x))))
  (testing "for finishing-move board for x on x"
    (is (= #{[0 0]}
           (finishing-moves-for [[:_ :o :_]
                                 [:o :x :_]
                                 [:_ :_ :x]] :x))))
  (testing "for finishing-move board for x on o"
    (is (= #{[0 1] [1 0] [1 1]}
           (finishing-moves-for [[:o :_ :o]
                                 [:_ :_ :x]
                                 [:o :x :x]] :o)))))

(deftest test-fork-moves-for
  (testing "for new board"
    (is (empty? (fork-moves-for (new-board) :x))))
  (testing "for non-fork-move board"
    (is (empty? (fork-moves-for [[:x :o :x]
                                 [:o :_ :o]
                                 [:o :x :_]] :x))))
  (testing "for fork-move board for x on x"
    (is (= #{[0 1] [0 2]}
           (fork-moves-for [[:x :_ :_]
                            [:_ :o :o]
                            [:x :o :_]] :x)))))

(deftest test-fork-for?
  (testing "for new board"
    (is (not (fork-for? (new-board) :x))))
  (testing "for non-fork board"
    (is (not (fork-for? [[:x :x :_]
                         [:o :_ :o]
                         [:_ :_ :_]] :x))))
  (testing "for fork board for x on x"
    (is (fork-for? [[:o :_ :x]
                    [:o :o :_]
                    [:x :_ :x]] :x)))
  (testing "for fork board for x on o"
    (is (not (fork-for? [[:o :_ :x]
                         [:o :o :_]
                         [:x :_ :x]] :o)))))

(deftest test-open-center?
  (testing "for new board"
    (is (open-center? (new-board))))
  (testing "for board with occupied center"
    (is (not (open-center? [[:_ :_ :_]
                            [:_ :x :_]
                            [:_ :_ :_]])))))

(deftest test-corner-positions
  (= (corner-positions (new-board))
     #{[0 0] [0 2] [2 0] [2 2]}))

(deftest test-open-corners
  (testing "for new board"
    (= (open-corners (new-board))
       #{[0 0] [0 2] [2 0] [2 2]}))
  (testing "for board with upper-left occupied"
    (= (open-corners [[:x :_ :_]
                      [:_ :_ :_]
                      [:_ :_ :_]])
       #{[0 2] [2 0] [2 2]}))
  (testing "for board with only corners occupied"
    (= (open-corners [[:x :_ :o]
                      [:_ :_ :_]
                      [:o :_ :x]])
       #{})))

(deftest test-choose-next-move-for
  (testing "for new board"
    (is (= [1 1]
           (choose-next-move-for (new-board) :x))))
  (testing "for board poised for win for x on x"
    (is (= (choose-next-move-for [[:x :_ :x]
                                  [:o :_ :o]
                                  [:_ :_ :_]] :x)
           [0 1])))
  (testing "for board poised for win for o on x"
    (is (= (choose-next-move-for [[:x :_ :_]
                                  [:o :_ :o]
                                  [:_ :_ :_]] :x)
           [1 1])))
  (testing "for board forkable for x on x"
    (is (contains?
         #{[0 2] [2 0]}
         (choose-next-move-for [[:x :_ :_]
                                [:_ :o :_]
                                [:_ :_ :x]] :x))))
  (testing "for board with a corner fork-block"
    (is (contains?
         #{[0 2] [2 0]}
         (choose-next-move-for [[:x :_ :_]
                                [:_ :x :_]
                                [:_ :_ :o]] :o))))
  (testing "for board forkable for o on x"
    (is (contains?
         #{[0 2] [2 0]}
         (choose-next-move-for [[:o :_ :_]
                                [:_ :x :_]
                                [:_ :_ :o]] :x))))
  (testing "for board where center is best move"
    (is (= [1 1]
           (choose-next-move-for [[:_ :o :x]
                                  [:_ :_ :_]
                                  [:o :x :_]] :x))))
  (testing "for board where empty corner is best move"
    (is (contains?
         #{[0 0] [2 0] [2 2]}
         (choose-next-move-for [[:_ :_ :x]
                                [:_ :o :_]
                                [:_ :_ :_]] :x))))
  (testing "for board where empty side is best move"
    (is (contains?
         #{[0 1] [1 0]}
         (choose-next-move-for [[:o :_ :x]
                                [:_ :o :o]
                                [:x :o :x]] :x))))
  (testing "for board with only one open move left"
    (is (= [0 1]
           (choose-next-move-for [[:o :_ :x]
                                  [:x :o :o]
                                  [:x :o :x]] :x))))
  (testing "for full board"
    (is (= nil
           (choose-next-move-for [[:o :x :x]
                                  [:x :o :o]
                                  [:x :o :x]] :x))))
  (testing "for possible fork block"
    (is (contains? #{[2 0] [0 2]}
                   (choose-next-move-for [[:x :_ :_]
                                          [:_ :x :_]
                                          [:_ :_ :o]] :o))))
  (testing "for impossible fork block"
    (is (contains? #{[2 0] [0 1]}
                   (choose-next-move-for [[:x :_ :x]
                                          [:o :x :_]
                                          [:_ :_ :o]] :o)))))

(deftest test-center-position
  (testing "for new board"
    (is (= [1 1]
           (center-position (new-board)))))
  (testing "for non-new board"
    (is (= [1 1]
           (center-position [[:x :_ :x]
                             [:o :o :x]
                             [:o :_ :_]])))))

(deftest test-parse-position
  (testing "for empty string"
    (is (nil? (parse-position ""))))
  (testing "for 1"
    (is (= (parse-position "1") [0 0])))
  (testing "for 2"
    (is (= (parse-position "2") [0 1])))
  (testing "for 3"
    (is (= (parse-position "3") [0 2])))
  (testing "for 4"
    (is (= (parse-position "4") [1 0])))
  (testing "for 5"
    (is (= (parse-position "5") [1 1])))
  (testing "for 6"
    (is (= (parse-position "6") [1 2])))
  (testing "for 7"
    (is (= (parse-position "7") [2 0])))
  (testing "for 8"
    (is (= (parse-position "8") [2 1])))
  (testing "for 9"
    (is (= (parse-position "9") [2 2])))
  (testing "for 0"
    (is (nil? (parse-position "0"))))
  (testing "for 42"
    (is (nil? (parse-position "42"))))
  (testing "for six"
    (is (nil? (parse-position "six")))))

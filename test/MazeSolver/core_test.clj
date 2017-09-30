(ns MazeSolver.core-test
  (:require [clojure.test :refer :all]
            [MazeSolver.core :refer :all]
            [clojure.pprint :refer :all])
  (:import (java.io File)
           (java.awt.image BufferedImage)
           (javax.imageio ImageIO)
           (java.awt Color)
           (java.nio.file Files)))

(defn- count-runs [c row state count counts]
  (case state
    :in (if (empty? row)
          (conj counts count)
          (if (= c (first row))
            (count-runs c (next row) :in (inc count) counts)
            (count-runs c (next row) :out 0 (conj counts count))))
    :out (if (empty? row)
           counts
           (if (= c (first row))
             (count-runs c (next row) :in 1 counts)
             (count-runs c (next row) :out 0 counts)))))

(defn- find-runs [c row]
  (count-runs c row :out 0 []))

(defn- get-row-widths [row]
  (let [bruns (find-runs "B" row)
        wruns (find-runs "W" row)]
    [(if (empty? bruns) 0 (apply min bruns))
     (if (empty? wruns) 0 (apply min wruns))
     ]))

(defn get-line-widths [maze]
  (if (= 0 (count maze))
    [0 0]
    (let [row-width (map get-row-widths maze)
          transposed-widths (apply mapv vector row-width)]
      [(apply min (first transposed-widths)) (apply min (second transposed-widths))])))

(defn half-cell [[bw ww]]
  (let [cell-size (+ bw ww)]
    (/ cell-size 2)))

(defn compress-maze [maze]
  (let [[bw ww] (get-line-widths maze)
        half-cell (half-cell [bw ww])
        mw (count (first maze))
        mh (count maze)
        w (/ (- mw bw) half-cell)
        h (/ (- mh bw) half-cell)
        ]
    (for [y (range 0 (inc h))]
      (for [x (range 0 (inc w))]
        (nth (nth maze (* y half-cell)) (* x half-cell))))))

(defn neighbours [x y]
  [[:left (dec x) y]
   [:right (inc x) y]
   [:up x (dec y)]
   [:down x (inc y)]])

(defn move? [maze x y]
  (= "W" (get-in maze [y x])))

(defn mark-move [maze x y]
  (assoc-in maze [y x] "■"))

(defn neighbour-moves [{:keys [maze moves x y] :or {moves []}}]
  (->> (neighbours x y)
       (filter (fn [[_ x' y']] (move? maze x' y')))
       (map (fn [[move x' y']] {:maze  (mark-move maze x' y')
                                :moves (conj moves move)
                                :x     x'
                                :y     y'}))))

(defn start-xy [[first-row :as maze]]
  (-> (keep-indexed (fn [i x] (when (= "W" x) i)) first-row)
      first
      (as-> x (when x [x 0]))))

(defn found-exit? [maze]
  ((-> maze peek set) "■"))

(defn solve-mazes [mazes]
  (let [exits (->> mazes (filter (comp found-exit? :maze)))]
    (if (seq exits)
      exits
      (when (seq mazes)
        (recur (mapcat neighbour-moves mazes))))))

(defn solve-maze [maze]
  (when-let [[x y] (start-xy maze)]
    (let [mazev (mapv vec maze)
          mazes [{:maze (mark-move mazev x y) :x x :y y}]]
      (when-first [m (solve-mazes mazes)]
        (assoc m :x x :y y)))))

(defn prepare-for-drawing-moves [maze [bw ww]]
  (let [step (half-cell [bw ww])
        multiply-by-step #(* % step)]
    (-> maze
        (update :x multiply-by-step)
        (update :y multiply-by-step)
        (assoc :width bw)
        (assoc :step step))))

(defn draw-moves [{:keys [moves width step] :as m}]
  (reduce
    (fn [acc move]
      (let [[x y] (if-let [{:keys [x' y']} (peek acc)]
                    (case move
                      :left [(dec x') y']
                      :right [(+ x' width) y']
                      :up [x' (dec y')]
                      :down [x' (+ y' width)])
                    [(:x m) (:y m)])
            step' (if (seq acc) step (+ width step))
            drawing (for [s (range 0 step')
                          w (range 0 width)]
                      (case move
                        :left [(- x s) (+ y w)]
                        :right [(+ x s) (+ y w)]
                        :up [(+ x w) (- y s)]
                        :down [(+ x w) (+ y s)]))
            position (case move
                       :left {:x' (- x (dec step')) :y' y}
                       :right {:x' (+ x (dec step') (- (dec width))) :y' y}
                       :up {:x' x :y' (- y (dec step'))}
                       :down {:x' x :y' (+ y (dec step') (- (dec width)))})]
        (-> (into acc drawing) (conj position))))
    [] moves))

(defn draw! [{:keys [input-filename output-filename drawing rgb]}]
  (let [input (File. ^String input-filename)
        output (File. ^String output-filename)
        ^BufferedImage img (ImageIO/read input)
        [^int r ^int g ^int b] rgb
        color (.getRGB (Color. r g b))]
    (doseq [[x y] drawing]
      (.setRGB img x y color))
    (ImageIO/write img "png" output)))

(defn solve-maze! [input-filename output-filename]
  (let [pixels (getPixels input-filename)
        maze (-> pixels compress-maze solve-maze)
        drawing (->> (get-line-widths pixels)
                     (prepare-for-drawing-moves maze)
                     (draw-moves)
                     (filter vector?))]
    (draw! {:input-filename  input-filename
            :output-filename output-filename
            :drawing         drawing
            :rgb             [237 28 36]})))

(deftest test-runs
  (is (= [] (find-runs "B" [])))
  (is (= [1] (find-runs "B" ["B"])))
  (is (= [1] (find-runs "B" ["W" "B" "W"])))
  (is (= [1 2] (find-runs "B" ["W" "B" "W" "W" "B" "B"]))))

(deftest degenerate-maze
  (is (= [0 0] (get-line-widths [])))
  (is (= [1 0] (get-line-widths [["B"]])))
  (is (= [1 1] (get-line-widths [["B", "W"]]))))

(deftest multi-row-maze
  (is (= [2 3] (get-line-widths [["B" "B" "W" "W" "W" "B" "B"]
                                 ["W" "W" "W" "W" "B" "B" "B"]])))
  (is (= [2 3] (get-line-widths [["W" "W" "W" "W" "B" "B" "B"]
                                 ["B" "B" "W" "W" "W" "B" "B"]]))))

(deftest compress-maze-test
  (is (= [["B" "W" "B"]
          ["B" "W" "B"]
          ["B" "W" "B"]]
         (compress-maze [["B" "B" "W" "W" "W" "W" "B" "B"]
                         ["B" "B" "W" "W" "W" "W" "B" "B"]
                         ["B" "B" "W" "W" "W" "W" "B" "B"]
                         ["B" "B" "W" "W" "W" "W" "B" "B"]
                         ["B" "B" "W" "W" "W" "W" "B" "B"]
                         ["B" "B" "W" "W" "W" "W" "B" "B"]
                         ["B" "B" "W" "W" "W" "W" "B" "B"]
                         ["B" "B" "W" "W" "W" "W" "B" "B"]])))
  (is (= [["B" "B" "W" "B" "B"]
          ["B" "B" "W" "B" "B"]
          ["B" "W" "B" "W" "B"]
          ["B" "B" "W" "B" "B"]]
         (compress-maze [["B" "B" "W" "B" "B"]
                         ["B" "B" "W" "B" "B"]
                         ["B" "W" "B" "W" "B"]
                         ["B" "B" "W" "B" "B"]])))

  (is (= '( ("B" "B" "B" "B" "B" "W" "B" "B" "B" "B" "B")
            ("B" "W" "W" "W" "B" "W" "W" "W" "B" "W" "B")
            ("B" "W" "B" "W" "B" "B" "B" "W" "B" "W" "B")
            ("B" "W" "B" "W" "W" "W" "B" "W" "W" "W" "B")
            ("B" "W" "B" "B" "B" "W" "B" "B" "B" "W" "B")
            ("B" "W" "W" "W" "B" "W" "B" "W" "W" "W" "B")
            ("B" "B" "B" "W" "B" "W" "B" "W" "B" "W" "B")
            ("B" "W" "W" "W" "B" "W" "W" "W" "B" "W" "B")
            ("B" "W" "B" "W" "B" "B" "B" "B" "B" "W" "B")
            ("B" "W" "B" "W" "W" "W" "B" "W" "W" "W" "B")
            ("B" "B" "B" "B" "B" "W" "B" "B" "B" "B" "B"))
         (compress-maze (getPixels "resources/maze.png")))))

(deftest real-maze
  (is (= [2 14] (get-line-widths (getPixels "resources/maze.png")))))

(deftest neighbours-test
  (is (= [[:left -1 0] [:right 1 0] [:up 0 -1] [:down 0 1]]
         (neighbours 0 0)))
  (is (= [[:left 1 3] [:right 3 3] [:up 2 2] [:down 2 4]]
         (neighbours 2 3))))

(deftest move?-test
  (let [maze [["B" "B" "W"]
              ["W" "B" "W"]]]
    (is (move? maze 2 0))
    (is (not (move? maze 1 0)))
    (is (not (move? maze -1 2)))))

(deftest mark-move-test
  (is (= [["W" "W" "B"]
          ["B" "W" "■"]]
         (mark-move [["W" "W" "B"]
                     ["B" "W" "W"]] 2 1))))

(deftest neighbour-moves-test
  (is (= [{:maze [["■" "W" "B"]
                  ["B" "W" "W"]], :moves [:left], :x 0, :y 0}
          {:maze [["W" "W" "B"]
                  ["B" "■" "W"]], :moves [:down], :x 1, :y 1}]
         (neighbour-moves {:maze [["W" "W" "B"]
                                  ["B" "W" "W"]] :x 1 :y 0})))
  (is (empty? (neighbour-moves {:maze [["B" "B"]] :x 0 :y 0}))))

(deftest start-xy-test
  (is (= [0 0] (start-xy [["W" "W" "B"]
                          ["B" "W" "W"]])))
  (is (= [1 0] (start-xy [["B" "W"]])))
  (is (not (start-xy [["B" "B"]]))))

(deftest found-exit?-test
  (is (not (found-exit? [["W" "■"]
                         ["B" "W"]])))
  (is (found-exit? [["W" "W"]
                    ["B" "■"]])))

(deftest solve-mazes-test
  (is (= [{:maze  [["■" "■" "B"]
                   ["B" "■" "W"]]
           :moves [:right :down], :x 1, :y 1}]
         (solve-mazes [{:maze [["■" "W" "B"]
                               ["B" "W" "W"]] :x 0 :y 0}])))
  (is (= [{:maze  [["B" "■"]
                   ["W" "■"]]
           :moves [:down], :x 1, :y 1}]
         (solve-mazes [{:maze [["B" "■"]
                               ["W" "W"]] :x 1 :y 0}])))
  (is (empty? (solve-mazes [{:maze [["■"]
                                    ["B"]] :x 0 :y 0}]))))

(deftest solve-maze-test
  (is (= {:maze [["■" "■" "B"]
                 ["B" "■" "W"]], :moves [:right :down], :x 0, :y 0}
         (solve-maze [["W" "W" "B"]
                      ["B" "W" "W"]])))
  (is (nil? (solve-maze [["B"]
                         ["B"]])))
  (is (nil? (solve-maze [["W"]
                         ["B"]])))
  (is (= {:maze  [["B" "B" "B" "B" "B" "■" "B" "B" "B" "B" "B"]
                  ["B" "■" "■" "■" "B" "■" "■" "■" "B" "W" "B"]
                  ["B" "■" "B" "■" "B" "B" "B" "■" "B" "W" "B"]
                  ["B" "■" "B" "■" "■" "■" "B" "■" "■" "■" "B"]
                  ["B" "■" "B" "B" "B" "■" "B" "B" "B" "■" "B"]
                  ["B" "■" "■" "■" "B" "■" "B" "■" "■" "■" "B"]
                  ["B" "B" "B" "■" "B" "■" "B" "■" "B" "W" "B"]
                  ["B" "W" "W" "■" "B" "■" "■" "■" "B" "W" "B"]
                  ["B" "W" "B" "■" "B" "B" "B" "B" "B" "W" "B"]
                  ["B" "W" "B" "■" "■" "■" "B" "W" "W" "W" "B"]
                  ["B" "B" "B" "B" "B" "■" "B" "B" "B" "B" "B"]],
          :moves [:down :right :right :down :down :right :right :down :down
                  :left :left :down :down :left :left :up :up :up :up :left
                  :left :up :up :left :left :down :down :down :down :right
                  :right :down :down :down :down :right :right :down],
          :x     5,
          :y     0}
         (-> "resources/maze.png" getPixels compress-maze solve-maze)))
  (is (= 3 (-> (solve-maze [["B" "W"]
                            ["W" "W"]
                            ["W" "W"]
                            ["B" "W"]]) :moves count)))
  (is (= 5 (-> (solve-maze [["B" "B" "W" "B" "B" "B"]
                            ["W" "W" "W" "B" "B" "B"]
                            ["W" "B" "W" "W" "W" "W"]
                            ["W" "B" "B" "B" "B" "W"]]) :moves count))))

(deftest prepare-for-drawing-moves-test
  (is (= {:x 20, :y 30, :width 4, :step 10}
         (prepare-for-drawing-moves {:x 2 :y 3} [4 16]))))

(deftest draw-moves-test
  (are [moves expected]
    (= (filter vector? (draw-moves {:moves moves :x 10 :y 10 :width 2 :step 3}))
       expected)
    [:left :left]
    [[10 10] [10 11] [9 10] [9 11] [8 10] [8 11] [7 10] [7 11] [6 10] [6 11]
     [5 10] [5 11] [4 10] [4 11] [3 10] [3 11]]
    [:left :up]
    [[10 10] [10 11] [9 10] [9 11] [8 10] [8 11] [7 10] [7 11] [6 10] [6 11]
     [6 9] [7 9] [6 8] [7 8] [6 7] [7 7]]
    [:left :down]
    [[10 10] [10 11] [9 10] [9 11] [8 10] [8 11] [7 10] [7 11] [6 10] [6 11]
     [6 12] [7 12] [6 13] [7 13] [6 14] [7 14]]
    [:right :right]
    [[10 10] [10 11] [11 10] [11 11] [12 10] [12 11] [13 10] [13 11] [14 10]
     [14 11] [15 10] [15 11] [16 10] [16 11] [17 10] [17 11]]
    [:right :up]
    [[10 10] [10 11] [11 10] [11 11] [12 10] [12 11] [13 10] [13 11] [14 10]
     [14 11] [13 9] [14 9] [13 8] [14 8] [13 7] [14 7]]
    [:right :down]
    [[10 10] [10 11] [11 10] [11 11] [12 10] [12 11] [13 10] [13 11] [14 10]
     [14 11] [13 12] [14 12] [13 13] [14 13] [13 14] [14 14]]
    [:up :left]
    [[10 10] [11 10] [10 9] [11 9] [10 8] [11 8] [10 7] [11 7] [10 6] [11 6]
     [9 6] [9 7] [8 6] [8 7] [7 6] [7 7]]
    [:up :right]
    [[10 10] [11 10] [10 9] [11 9] [10 8] [11 8] [10 7] [11 7] [10 6] [11 6]
     [12 6] [12 7] [13 6] [13 7] [14 6] [14 7]]
    [:up :up]
    [[10 10] [11 10] [10 9] [11 9] [10 8] [11 8] [10 7] [11 7] [10 6] [11 6]
     [10 5] [11 5] [10 4] [11 4] [10 3] [11 3]]
    [:down :left]
    [[10 10] [11 10] [10 11] [11 11] [10 12] [11 12] [10 13] [11 13] [10 14]
     [11 14] [9 13] [9 14] [8 13] [8 14] [7 13] [7 14]]
    [:down :right]
    [[10 10] [11 10] [10 11] [11 11] [10 12] [11 12] [10 13] [11 13] [10 14]
     [11 14] [12 13] [12 14] [13 13] [13 14] [14 13] [14 14]]
    [:down :down]
    [[10 10] [11 10] [10 11] [11 11] [10 12] [11 12] [10 13] [11 13] [10 14]
     [11 14] [10 15] [11 15] [10 16] [11 16] [10 17] [11 17]]))

(deftest solve-maze!-test
  (let [expected (File. "resources/maze-solved.png")
        actual (File/createTempFile "maze" ".png")
        actual-filename (.getAbsolutePath actual)
        read #(-> (.toPath %) (Files/readAllBytes) (vec))]
    (solve-maze! "resources/maze.png" actual-filename)
    (is (= (read expected) (read actual)))
    (.delete actual)))

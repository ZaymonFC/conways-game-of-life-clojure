(:require [])

;; Game Rules
;; -  Any live cell with two or three live neighbours survives.
;; -  Any dead cell with three live neighbours becomes a live cell.
;; -  All other live cells die in the next generation. Similarly, 
;;      all other dead cells stay dead.

(defn get-neighbours [[row col]]
  (->> (for [ox [-1 0 1]
             oy [-1 0 1]]
         (when (not= ox oy 0) [(+ row oy) (+ col ox)]))
       (filter (complement nil?))))

(defn count-live-neighbours [board coord]
  (let [neighbours (get-neighbours coord)]
    (->> neighbours
         (map (fn [coord] (get board coord false)))
         (filter identity)
         (count))))

(defn cell-lives? [board coord]
  (let [live-neighbours (count-live-neighbours board coord)]
    (cond
      (= live-neighbours 3) true
      (= live-neighbours 2) true
      :else false)))

(defn cell-spawns? [board coord]
  (= 3 (count-live-neighbours board coord)))

(def test-board #{[0 1] [1 1] [-1 0]})

(defn spawn-candidates [board]
  (->> board
       (map get-neighbours)
       (apply concat)
       (into #{})))

(defn cells-to-spawn [board]
  (let [candidates (spawn-candidates board)]
    (->> candidates
         (filter (fn [coord] (cell-spawns? board coord)))
         (into #{}))))

(defn cells-to-die [board]
  (->> board
       (filter (fn [coord] (not (cell-lives? board coord))))
       (into #{})))

(cells-to-spawn test-board)

(defn step [board]
  (let [to-spawn (cells-to-spawn board)
        to-die (cells-to-die board)]
    (clojure.set/union to-spawn (clojure.set/difference board to-die))))

(defn random-points [n row col]
  (->> (range n)
       (map (fn [_] [(rand-int row) (rand-int col)]))
       (into #{})))

(random-points 10 100 100)

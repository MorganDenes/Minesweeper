(ns minesweeper.core
  (:gen-class))

(def not-nil (complement nil?))

(defn parse-int [s]
   (Integer. (re-find  #"\d+" s )))

; (def to-letter {0 "A" 1 "B" 2 "C" 3 "D" 4 "E" 5 "F" 6 "G" 7 "H" 8 "I" 9 "J"})

(defn random [] (rand-int 10))

(defn MakeMines
  [num-mines]
  (loop
    [mines #{}]
    (if (>= (count mines) num-mines)
      mines
      (recur (conj mines [(random) (random)])))))

(defn AddArea
  [mmap spot]
  (if (not-nil (mmap spot)) mmap (conj mmap {spot 0})))

(defn AddMine
  [mmap spot]
  (update mmap spot inc))
  
(defn sur
  [spot]
  (for [sur [[1 1] [1 0] [1 -1] [0 1] [0 -1] [-1 1] [-1 0] [-1 -1]]]
    (into [] (map #(+ %1 %2) sur spot))))

(defn UpdateMap
  [mmap mine]
  (loop [[spot & rspot] (sur mine) newmap mmap]
   (if (empty? spot)
     newmap
     (recur rspot (AddMine (AddArea newmap spot) spot)))))

(defn MakeMap
  [mines]
  (loop [[mine & o-mines] mines minesmap {}]
      (if (empty? mine)
        minesmap
        (recur o-mines (UpdateMap minesmap mine)))))

(defn PrintMinesMap
  [mmap mines]
  (loop [x 0 y 0]
    (if (= y 10)
      (do (newline) (recur (inc x) 0))
      (if (= x 10)
        mmap
        (do (print (if (not-nil (mines [x y]))
              "X"
              (mmap [x y] "█")))
            (recur x (inc y)))))))

(defn PrintPlayMap
  [plays]
  (loop [x 0 y 0]
    (if (= y 10)
      (do (newline) (recur (inc x) 0))
      (if (= x 10)
        plays
        (do (print (get plays [x y] "█"))
            (recur x (inc y)))))))
  

(defn GetMove
  []
  (do (print "Input: ")
      (flush)
      (let [[a b] (read-line)]
        (into [] (map #(dec %) (map #(Integer/parseInt (str %)) [b a]))))))

(defn AddMove
  [plays mmap mines]
  (let [move (GetMove)]
    (conj plays (if (mines move)
                    {move "X"}
                    (if (mmap move)
                        {move (mmap move)}
                        {move "_"})))))

(defn Play
  [mmap mines]
  (loop [plays {}]
    (recur (PrintPlayMap (AddMove plays mmap mines)))))


(defn -main
  []
  (let [mines (MakeMines 10)]
    (Play (MakeMap mines) mines)))














(defn error-message
  [severity]
  (str "It's a disater! We're "
    (if (= severity :mild)
       "mildly incovenienced!"
       "doomed!")))

(defn my-first
  [[first-thing & rest]]
  (println first-thing)
  rest)

(defn looping [cap]
(loop [iteration 0]
  (println (str "Iteration " iteration))
  (if (> iteration cap)
    (println "Goodby!")
    (recur (inc iteration))))
)

(def asym-parts [
{:name "head" :size 3}
{:name "left-eye" :size 1}
{:name "mouth" :size 1}
{:name "left-arm" :size 6}
{:name "left-hand" :size 2}
{:name "chest" :size 10}
{:name "left-leg" :size 8}
{:name "left-foot" :size 2}])

(defn matching-part
  [part]
  {:name (clojure.string/replace (:name part) #"^left-" "right-")
   :size (:size part)})

(defn sym-body-parts
  [asym-body-parts]
  (loop [remaining asym-body-parts
         final-body-parts []]
    (if (empty? remaining)
      final-body-parts
      (let [[part & remaining] remaining]
        (recur remaining
               (into final-body-parts
                     (set [part (matching-part part)])))))))









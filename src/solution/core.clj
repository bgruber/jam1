(ns solution.core)

(def alphabet "abcdefghijklmnopqrstuvwxyz")

(def NWORDS (atom (->> (slurp "resources/big.txt")
                       clojure.string/lower-case
                       (re-seq #"[a-z]+")
                       frequencies)))

(defn splits [word]
  (let [wc (count word)]
    (map (fn [pos]
           [(subs word 0 pos) (subs word pos wc)])
         (range 0 (inc wc)))))

(defn deletes [splits]
  (for [[a b] splits]
    (apply str a (rest b))))

(defn transposes [splits]
  (for [[a [b0 b1 & br :as b]] splits :when (> (count b) 1)]
    (apply str a b1 b0 br)))

(defn replaces [splits]
  (for [[a b] splits :when (seq b)
        c alphabet]
    (apply str a c (next b))))

(defn inserts [splits]
  (for [[a b] splits
        c alphabet]
    (apply str a c b)))

(defn edits1 [word]
  (let [ss (splits word)]
    (concat (deletes ss)
            (transposes ss)
            (replaces ss)
            (inserts ss))))

(defn edits2 [word e1]
  (concat e1 (map edits1 e1)))

(defn make-known [words]
  (set (filter @NWORDS words)))

(defn correct [word]
  (let [e1 (edits1 word)]
    (or (seq (set (sort-by @NWORDS
                           (mapcat make-known [[word] e1 (edits2 word e1)]))))
              [word])))

(defn addword! [word]
  (swap! NWORDS update-in [word] (fnil inc 0)))

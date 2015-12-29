(ns looping-is-recursion)

(defn power [base exp]
  (let [acc 1
        helper (fn [acc base e]
                 (if (zero? e)
                   acc
                   (recur (* acc base) base (dec e))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [a]
                 (if (empty? (rest a))
                   (first a)
                   (recur (rest a))))]
    (helper a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [acc seq1 seq2]
                 (cond
                  (and (empty? seq1) (empty? seq2))
                    true
                  (or (empty? seq1) (empty? seq2))
                    false
                  (= (first seq1) (first seq2))
                    (recur true (rest seq1) (rest seq2))
                  :else false))]
    (helper false seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [index 0
         loop-seq a-seq]
    (cond (empty? loop-seq)
            nil
          (pred (first loop-seq))
            index
          :else
            (recur (inc index) (rest loop-seq)))))

(defn avg [a-seq]
  (loop [acc 0
         total 0
         loop-seq a-seq]
    (if (empty? loop-seq)
      (/ acc total)
      (recur (+ acc (first loop-seq))
             (inc total) (rest loop-seq)))))

(defn parity [a-seq]
  (loop [coll #{}
         loop-seq a-seq]
    (cond
     (empty? loop-seq)
       coll
     (contains? coll (first loop-seq))
       (recur (disj coll (first loop-seq)) (rest loop-seq))
     :else (recur (conj coll (first loop-seq)) (rest loop-seq)))))

(defn fast-fibo [n]
  (loop [acc 0
         fib  0
         fib1 1]
    (if (>= acc n)
      fib
      (recur (inc acc) (+ fib fib1) fib))))

(defn cut-at-repetition [a-seq]
  (loop [acc []
         loop-seq a-seq]
   (cond
    (contains? (set acc) (first loop-seq))
      acc
    (empty? loop-seq)
      acc
    :else (recur (conj acc (first loop-seq)) (rest loop-seq)))))


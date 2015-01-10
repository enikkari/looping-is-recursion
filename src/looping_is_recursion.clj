(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [now base exp]
                 (cond (= 1 exp) now
                       (zero? exp) 1
                       :else (recur (* now base) base (dec exp))))]
    (helper base base exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
      (first a-seq)
      (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond  (and (empty? seq1) (empty? seq2)) true
         (or (empty? seq1) (empty? seq2)) false
         (= (first seq1) (first seq2)) (recur (rest seq1) (rest seq2))
         :else false))

(defn find-first-index [pred a-seq]
  (loop [sek a-seq
         index 0]
    (cond (empty? sek) nil
          (pred (first sek)) index
          :else (recur (rest sek) (inc index)))))

(defn avg [a-seq]
  (if (empty? a-seq)
    0
    (loop [sum 0
           sek a-seq
           numb 0]
     (if (empty? sek)
       (/ sum numb)
       (recur (+ sum (first sek)) (rest sek) (inc numb))))))

(defn parity [a-seq]
  (let [help-toggle (fn [p-set elem] (if (contains? p-set elem)
                                       (disj p-set elem)
                                       (conj p-set elem)))]
    (loop [new-seq a-seq
         parity-set #{}]
    (if (empty? new-seq)
      parity-set
      (recur (rest new-seq) (help-toggle parity-set (first new-seq)))))))

(defn fast-fibo [n]
  (loop [run 1
         f-n-1 0
         f-n 1]
    (cond (zero? n) 0
          (= n 1) 1
          (= run n) f-n
          :else (recur (inc run) f-n (+ f-n f-n-1)))))

(defn dublicate-index [symb a-seq]
  (loop [new-seq a-seq
         index 0
         occurence 0]
    (cond (= occurence 2) index
          (empty? new-seq) nil
          (= symb (first new-seq)) (recur (rest new-seq) (inc index) (inc occurence))
          :else (recur (rest new-seq) (inc index) occurence))))

(defn cut-at-repetition [a-seq]
  (loop [new-seq a-seq
         symbols (seq (set a-seq))]
    (if (empty? symbols)
      new-seq
      (let [d-index (dublicate-index (first symbols) new-seq)]
        (if (= nil d-index)
          (recur new-seq (rest symbols))
          (recur (take (dec d-index) new-seq) (rest symbols)))))))


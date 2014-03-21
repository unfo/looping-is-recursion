(ns looping-is-recursion)

(defn power [base exp]
  (let [powerful (fn [acc n]
                   (if (zero? n)
                     acc
                     (recur (* acc base) (dec n))))]
    (powerful 1 exp)))

(defn last-element [a-seq]
  (let [last-samurai (fn [prev curr]
                       (if (empty? curr)
                         prev
                         (recur (first curr) (rest curr))))]
    (last-samurai (first a-seq) (rest a-seq))))

(defn seq= [seq1 seq2]
  (let [comparator-factory-bean-maker (fn [seqa seqb]
                                        (let [fst-a (first seqa)
                                              fst-b (first seqb)]
                                          (cond
                                            (and (empty? seqa) (empty? seqb)) true
                                            (or (nil? fst-a) (nil? fst-b)) false
                                            (not (== fst-a fst-b)) false
                                            :default (recur (rest seqa) (rest seqb)))))]
    (comparator-factory-bean-maker seq1 seq2)))



(defn find-first-index [pred a-seq]
  (let [find-nemo (fn [curr-index needle haystack]
                    (cond
                      (empty? haystack) nil
                      (true? (needle (first haystack))) curr-index
                      :default (recur (inc curr-index) needle (rest haystack))))]
    (find-nemo 0 pred a-seq)))


(defn avg [a-seq]
  (let [joe-average (fn [sum count remaining-seq]
                      (if (empty? remaining-seq)
                        (if (zero? count) 0
                          (/ sum count))
                        (recur (+ sum (first remaining-seq)) (inc count) (rest remaining-seq))))]
    (joe-average 0 0 a-seq)))

(defn parity [a-seq]
  (let [
         toggle (fn [a-set elem]
                  (if (contains? a-set elem)
                    (disj a-set elem)
                    (conj a-set elem)))
         toggler (fn [toggled-set haystack]
                   (if (empty? haystack) toggled-set
                     (recur (toggle toggled-set (first haystack)) (rest haystack))))]
    (toggler #{} a-seq)))

(defn fast-fibo [n]
  (let [ff (fn [n-minus-1 n idx upto]
             (if (= idx upto) n
               (recur n (+ n n-minus-1) (inc idx) upto)))]
    (ff 1 0 0 n)))

(defn cut-at-repetition [a-seq]
  (let [cutter (fn [previous-items collectible-items future-items]
                 (let [fst (first future-items)]
                   (cond
                     (empty? future-items) collectible-items
                     (contains? previous-items fst) collectible-items
                     :default (recur (conj previous-items fst) (conj collectible-items fst) (rest future-items)))))]
    (cutter #{} [] a-seq)))


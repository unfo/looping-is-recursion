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
                                            (not (= fst-a fst-b)) false
                                            :default (recur (rest seqa) (rest seqb)))))]
    (comparator-factory-bean-maker seq1 seq2)))



(defn find-first-index [pred a-seq]
  ":(")

(defn avg [a-seq]
  -1)

(defn parity [a-seq]
  ":(")

(defn fast-fibo [n]
  ":(")

(defn cut-at-repetition [a-seq]
  [":("])


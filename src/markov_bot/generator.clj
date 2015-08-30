(ns markov-bot.generator
  (:require [clojure.string :as str])
  (:require [clojure.set :as c-set]))

(defn word-chain [word-transitions]
  (reduce 
    (fn [r t] 
      (merge-with 
        c-set/union r 
        (let [[a b c] t] 
          {[a b] (if c #{c} #{})}))) 
    {} word-transitions))

(defn text->word-chain [text]
  (letfn [(word-split [t]
            (str/split t #"( |\n)"))
          (words->word-transitions [ws]
            (partition-all 3 1 ws))
          (text->word-transitions [t]
            (words->word-transitions (word-split t)))]
    (word-chain  (text->word-transitions text))))

(defn walk-chain [prefix chain result]
  (let [suffixes (get chain prefix)]
    (cond 
      (empty? suffixes) result
      :else (let [suffix (first (shuffle suffixes))
                  new-prefix [(last prefix) suffix]]
              (recur new-prefix chain (conj result suffix))))))



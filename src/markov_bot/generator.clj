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

(defn chain->text [chain]
  (apply str (interpose " " chain)))

(defn walk-chain [prefix chain result]
  (let [suffixes (get chain prefix)]
    (cond 
      (empty? suffixes) result
      :else (let [suffix (last (shuffle suffixes))
                  new-prefix [(last prefix) suffix]
                  new-result (conj result suffix)]
              (if (< (count  (chain->text new-result)) 140) 
                (recur new-prefix chain new-result)
                result)))))

(defn generate-text [prefix-text chain]
  (let [prefix (str/split prefix-text #"( |\n)")]
    (chain->text 
      (walk-chain prefix chain prefix))))


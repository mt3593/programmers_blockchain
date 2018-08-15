(ns programmers-blockchain.blockchain
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [digest :as digest]
            [miner.strgen :as sg]))

(def ^:private hash-regex
  #"[A-Fa-f0-9]{64}")

(s/def :block/hash
  (s/spec #(re-matches hash-regex %)
          :gen #(sg/string-generator hash-regex)))
(s/def :block/previous-hash :block/hash)
(s/def :block/data string?)
(s/def :block/time-stamp int?)
(s/def :block/nonce int?)
(s/def :block/block
  (s/keys :req [:block/hash
                :block/previous-hash
                :block/data
                :block/time-stamp]))
(s/def :block/blockchain (s/coll-of :block/block))

(def ^:dynamic *difficulty* 2)

(defn- mine-block
  [previous-hash data time-stamp]
  (let [target (apply str (repeat *difficulty* 0))]
    (loop [nonce 0]
      (let [hash (digest/sha-256 (str data previous-hash time-stamp nonce))]
        (if (= target (subs hash 0 *difficulty*))
          {:hash hash :nonce nonce}
          (recur (inc nonce)))))))

(defn next-block
  [previous-hash data]
  {:pre [(s/valid? :block/data data)
         (s/valid? :block/previous-hash previous-hash)]}
  (let [time-stamp (System/currentTimeMillis)
        {:keys [hash nonce]} (mine-block previous-hash data time-stamp)]
    {:block/hash hash
     :block/nonce nonce
     :block/previous-hash previous-hash
     :block/data data
     :block/time-stamp time-stamp}))

(defn genesis-blockchain []
  [(next-block (digest/sha-256 "start") "{}")])

(defn add-block
  [blockchain data]
  (let [{previous-hash :block/hash} (last blockchain)]
    (conj blockchain (next-block previous-hash data))))


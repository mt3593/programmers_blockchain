(ns programmers-blockchain.blockchain-test
  (:require [clojure.test :refer :all]
            [programmers-blockchain.blockchain :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [digest :as digest]
            [clojure.data]))

(defn valid-block?
  [{data :block/data
    hash :block/hash
    previous-hash :block/previous-hash
    nonce :block/nonce
    time-stamp :block/time-stamp}]
  (= hash
     (digest/sha-256 (str data previous-hash time-stamp nonce))))

(s/fdef programmers-blockchain.blockchain/next-block
        :args (s/cat :previous-hash :block/previous-hash
                     :data :block/data)
        :fn #(valid-block? (:ret %))
        :ret :block/block)

(s/fdef programmers-blockchain.blockchain/add-block
        :args (s/cat :blockchain :block/blockchain
                     :data :block/data)
        :fn #(every? valid-block? (:ret %))
        :ret :block/blockchain)

(defn test-passed
  [{check-failed :check-failed
    check-threw :check-threw
    no-gen :no-gen
    no-of-tests :total :as a}]
  (is (nil? check-failed))
  (is (nil? check-threw))
  (is (nil? no-gen))
  (is (< 0
         no-of-tests)))

(deftest next-block-test
  (-> `next-block
      stest/check
      stest/summarize-results
      test-passed))

(deftest simple-block-chain
  (let [blockchain (-> (genesis-blockchain)
                       (add-block "data 1")
                       (add-block "data 2")
                       (add-block "data 3")
                       (add-block "data 4")
                       (add-block "data 5")
                       (add-block "data 6"))]
    (is (every? valid-block? blockchain))
    (is (every? identity
                (map (fn [{previous-hash :block/previous-hash}
                          {hash :block/hash}]
                       (= hash previous-hash))
                     (rest blockchain) blockchain)))
    (is (= 7 (count blockchain)))))

(deftest chain-is-unchanged
  (let [blockchain (-> (genesis-blockchain)
                       (add-block "data 1")
                       (add-block "data 2"))
        blockchain-new (add-block blockchain "data 3")]
    (is (every? valid-block? blockchain))
    (is (every? valid-block? blockchain-new))
    (is (= blockchain
           (butlast blockchain-new)))
    (is (= 4 (count blockchain-new)))))

(deftest no-block-chains-are-equal
  (testing "different lengths"
    (let [blockchain-1 (-> (genesis-blockchain)
                           (add-block "data 1")
                           (add-block "data 2"))
          blockchain-2 (-> (genesis-blockchain)
                           (add-block "data 1")
                           (add-block "data 2")
                           (add-block "data 3"))]
      (is (not= blockchain-1 blockchain-2))))

  (testing "different data"
    ;; it is possible that this may result in a false positive if a millisecond elapses
    ;; between generating the block chain
    (let [blockchain-1 (-> (genesis-blockchain)
                           (add-block "data"))
          blockchain-2 (-> (genesis-blockchain)
                           (add-block "different data"))]
      (is (not= blockchain-1 blockchain-2))))

  (testing "generated at different times"
    (let [blockchain-1 (-> (genesis-blockchain)
                           (add-block "data"))
          _ (Thread/sleep 1)
          blockchain-2 (-> (genesis-blockchain)
                           (add-block "data"))]
      (is (not= blockchain-1 blockchain-2)))))

(deftest different-chain-create-different-hash-even-with-same-data
  (let [[_genesis _ {hash-1 :block/hash} :as a] (-> (genesis-blockchain)
                                                    (add-block "chain 1")
                                                    (add-block "data"))
        [_genesis _ {hash-2 :block/hash} :as b] (-> (genesis-blockchain)
                                                    (add-block "chain 2")
                                                    (add-block "data"))]
    (is (not= hash-1 hash-2))))

(deftest divergent-chain-diffent-blocks-generated
  (let [original-blockchain (-> (genesis-blockchain)
                                (add-block "chain 1")
                                (add-block "chain 2"))
        divergent-blockchain-a (-> original-blockchain
                                   (add-block "chain 2a")
                                   (add-block "chain 3"))
        divergent-blockchain-b (-> original-blockchain
                                   (add-block "chain 2")
                                   (add-block "chain 3"))]
    (is (not= divergent-blockchain-a divergent-blockchain-b))
    (is (not= (:block/hash (last divergent-blockchain-a))
              (:block/hash (last divergent-blockchain-b))))))

(comment
  "This one takes a long time to run I got the following results:
   Elapsed time: 13.391831 msecs
   Elapsed time: 2681.253939 msecs
   Elapsed time: 463592.481263 msecs"
  (deftest expensive-mining-blocks
    (let [low-difficulty (time (binding [*difficulty* 2]
                                 (-> (genesis-blockchain)
                                     (add-block "chain 1"))))
          medium-difficulty (time (binding [*difficulty* 4]
                                    (-> (genesis-blockchain)
                                        (add-block "chain 1"))))
          high-difficulty (time (binding [*difficulty* 6]
                                  (-> (genesis-blockchain)
                                      (add-block "chain 1"))))]
      (println low-difficulty)
      (println medium-difficulty)
      (println high-difficulty))))

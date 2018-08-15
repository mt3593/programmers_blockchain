(ns programmers-blockchain.wallet-test
  (:require [programmers-blockchain.wallet :refer :all]
            [programmers-blockchain.core :as block]
            [clojure.test :refer :all]))

(deftest create-wallet
  "Make a new wallet with a public key"
  (is (not (nil? (get-public-key (new-wallet))))))

(deftest signing-data
  (testing "two wallets should generate different signatures for same data"
    (is (not-every? identity (map =
                                  (sign (new-wallet) "data")
                                  (sign (new-wallet) "data")))))

  (testing "same wallet same data, different sig (due to time)"
    (let [wallet (new-wallet)]
      (is (not-every? identity (map =
                                (sign wallet "data")
                                (sign wallet "data")))))))

(deftest verify-signature
  (testing "Can verify signed data"
    (let [wallet (new-wallet)
          data "data"
          sig (sign wallet data)]
      (is (true? (verify wallet data sig)))))

  (testing "Cannot verify signed data from different wallet"
    (let [wallet (new-wallet)
          wallet-2 (new-wallet)
          data "data"
          sig (sign wallet data)]
      (is (false? (verify wallet-2 data sig)))))

  (testing "Cannot verify different data"
    (let [wallet (new-wallet)
          data "data"
          sig (sign wallet data)]
      (is (false? (verify wallet "not same data" sig))))))

(deftest wallet-balance
  (testing "one transaction"
    (let [blockchain (block/genesis-blockchain)
          sender (new-wallet)
          reciepient (new-wallet)
          new-blockchain (transfer-money sender reciepient 10 blockchain)]
      (is (= 10 (balance reciepient new-blockchain)))
      (is (= -10 (balance sender new-blockchain)))))

  (testing "many transactions"
    (let [blockchain (block/genesis-blockchain)
          sender (new-wallet)
          reciepient (new-wallet)
          new-blockchain (->> blockchain
                              (transfer-money sender reciepient 10)
                              (transfer-money reciepient sender 10)
                              (transfer-money sender reciepient 10)
                              (transfer-money reciepient sender 10))]
      (is (= 0 (balance reciepient new-blockchain)))
      (is (= 0 (balance sender new-blockchain)))))

  (testing "many wallets transactions"
    (let [blockchain (block/genesis-blockchain)
          wallet-1 (new-wallet)
          wallet-2 (new-wallet)
          wallet-3 (new-wallet)
          wallet-4 (new-wallet)
          wallet-5 (new-wallet)
          wallet-6 (new-wallet)
          new-blockchain (->> blockchain
                              (transfer-money wallet-3 wallet-2 10)
                              (transfer-money wallet-4 wallet-2 10)
                              (transfer-money wallet-5 wallet-2 10)
                              (transfer-money wallet-2 wallet-1 10)
                              (transfer-money wallet-1 wallet-3 10)
                              (transfer-money wallet-1 wallet-2 10)
                              (transfer-money wallet-1 wallet-2 10)
                              (transfer-money wallet-6 wallet-5 10))]
      (is (= -20 (balance wallet-1 new-blockchain)))
      (is (= 40 (balance wallet-2 new-blockchain)))
      (is (= 0 (balance wallet-3 new-blockchain)))
      (is (= -10 (balance wallet-4 new-blockchain)))
      (is (= 0 (balance wallet-5 new-blockchain)))
      (is (= -10 (balance wallet-6 new-blockchain))))))



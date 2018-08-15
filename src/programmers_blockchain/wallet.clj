(ns programmers-blockchain.wallet
  (:import [java.security KeyPairGenerator SecureRandom Security Signature]
           [java.security.spec ECGenParameterSpec]
           [org.bouncycastle.jce.provider BouncyCastleProvider]
           [java.util Base64])
  (:require [programmers-blockchain.blockchain :as block]
            [clojure.spec.alpha :as s]
            [cheshire.core :as json]))

(defprotocol Wallet
  (get-public-key [this])
  (sign [this data])
  (verify [this data sig])
  (balance [this blockchain])
  (transfer-money [sender reciepient amount blockchain]))

(defn- generate-key-pair
  []
  (Security/addProvider (BouncyCastleProvider.))
  (let [keygen (KeyPairGenerator/getInstance "ECDSA" "BC")
        random (SecureRandom/getInstance "SHA1PRNG")
        ec-spec (ECGenParameterSpec. "prime192v1")
        _ (.initialize keygen ec-spec random)
        key-pair (.generateKeyPair keygen)]
    {:public-key (.getPublic key-pair)
     :private-key (.getPrivate key-pair)}))

(defn- convert-to-transactions
  [blockchain]
  {:pre [(s/valid? :block/blockchain blockchain)]}
  (->> blockchain
       (map :block/data)
       (map #(json/parse-string % true))
       (mapcat :transactions)))

(defn- amount-gained
  [blockchain wallet]
  {:pre [(s/valid? :block/blockchain blockchain)]}
  (->> blockchain
       convert-to-transactions
       (filter #(= (get-public-key wallet)
                   (get-in % [:reciepient :public-key])))
       (map :amount)
       (reduce +)))

(defn- amount-spent
  [blockchain wallet]
  {:pre [(s/valid? :block/blockchain blockchain)]}
  (->> blockchain
       convert-to-transactions
       (filter #(= (get-public-key wallet)
                   (get-in % [:sender :public-key])))
       (map :amount)
       (reduce +)))

(s/def :transaction/amount pos-int?)

(defn- transaction
  [sender reciepient amount]
  (let [data (str (get-public-key sender)
                  (get-public-key reciepient)
                  amount)
        hash (digest/sha-256 data)
        sig (sign sender data)]
    {:sender {:public-key (get-public-key sender)}
     :reciepient {:public-key (get-public-key reciepient)}
     :sig sig
     :hash hash
     :amount amount}))

(defn process-transaction
  [sender reciepient amount blockchain]
  {:pre [(satisfies? Wallet sender)
         (satisfies? Wallet reciepient)
         (s/valid? :transaction/amount amount)
         (s/valid? :block/blockchain blockchain)]
   :post [#(s/valid? :block/blockchain %)]}
  (block/add-block blockchain
                   (json/generate-string
                    {:transactions [(transaction sender reciepient amount)]})))


(defrecord ^:private CryptoWallet [public-key private-key]
  Wallet
  (get-public-key [_]
    (.encodeToString (Base64/getEncoder) (.getEncoded public-key)))

  (sign [_ data]
    (let [dsa (Signature/getInstance "ECDSA" "BC")]
      (.initSign dsa private-key)
      (.update dsa (.getBytes data))
      (.sign dsa)))

  (verify [_ data sig]
    (let [dsa (Signature/getInstance "ECDSA" "BC")]
      (.initVerify dsa public-key)
      (.update dsa (.getBytes data))
      (.verify dsa sig)))

  (balance [this blockchain]
    (- (amount-gained blockchain this)
       (amount-spent blockchain this)))

  (transfer-money [sender reciepient amount blockchain]
    (process-transaction sender reciepient amount blockchain)))

(defn new-wallet
  []
  (map->CryptoWallet (generate-key-pair)))

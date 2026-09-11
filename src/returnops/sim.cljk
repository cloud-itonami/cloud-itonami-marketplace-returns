(ns returnops.sim
  "Offline demo: an eligible return authorizes itself, an out-of-window
  one is declined with a reason the buyer can escalate, and the
  resolution waits for a human. `clojure -M:dev:run`."
  (:require [langgraph.graph :as g]
            [marketplace.returns :as ret]
            [returnops.operation :as operation]
            [returnops.store :as store]))

(def ^:private now "2026-06-05T09:00:00Z")
(def ^:private late "2026-07-01T09:00:00Z")

(defn- run-req! [actor tid request t]
  (g/run* actor {:request request
                 :context {:actor-id "returns-demo" :phase 3 :now t}}
          {:thread-id tid}))

(defn -main [& _]
  (let [s (store/seed-db)
        actor (operation/build s)]

    (println "\n=== 1. 返品要求の受付（金額は注文から引く）===")
    (run-req! actor "sim-1"
              {:op :open-rma
               :patch {:id "rma-1" :order "ord-1" :seller "merchant.alpha"
                       :buyer "buyer-1" :reason :not-as-described
                       :category :beverages :lines [{:sku "A1" :qty 1}]
                       :currency "JPY"}} now)
    (let [r (store/rma-record s "rma-1")]
      (println "  状態     :" (:rma/state r))
      (println "  返金上限 :" (:rma/amount-minor r) (:rma/currency r)))

    (println "\n=== 2. 期限内 → 出品者の公開ポリシーで自動承認 ===")
    (let [x (run-req! actor "sim-2" {:op :authorize-return
                                     :patch {:rma-id "rma-1" :now now}} now)]
      (println "  status   :" (:status x) " 状態:" (:rma/state (store/rma-record s "rma-1"))))

    (println "\n=== 3. 期限切れの承認を試みる → HARD hold（却下経路を使え）===")
    (let [s2 (store/seed-db) a2 (operation/build s2)]
      (run-req! a2 "o" {:op :open-rma
                        :patch {:id "rma-9" :order "ord-1" :seller "merchant.alpha"
                                :buyer "b" :reason :changed-mind :category :beverages
                                :lines [{:sku "A1" :qty 1}] :currency "JPY"}} late)
      (let [x (run-req! a2 "a" {:op :authorize-return
                                :patch {:rma-id "rma-9" :now late}} late)]
        (println "  status     :" (:status x))
        (println "  violations :" (mapv :rule (:violations (last (store/ledger s2))))))
      (println "\n=== 4. 却下は理由付き。ただし :changed-mind は争えない ===")
      (run-req! a2 "d" {:op :decline-return :patch {:rma-id "rma-9" :now late}} late)
      (let [r (store/rma-record s2 "rma-9")]
        (println "  状態     :" (:rma/state r))
        (println "  理由     :" (:reasons (:rma/eligibility r)))
        (println "  争える?  :" (ret/escalatable? r) "← 気が変わっただけでは紛争にならない")))

    (println "\n=== 5. 返送 → 受領 → 検品 → 解決（解決だけ人間）===")
    (doseq [[tid op patch] [["s" :record-return-shipment {:rma-id "rma-1" :tracking "RT-1"}]
                            ["r" :receive-return {:rma-id "rma-1"}]
                            ["i" :record-inspection {:rma-id "rma-1" :condition :as-described
                                                     :inspected-by "wh-01"}]]]
      (run-req! actor tid {:op op :patch patch} now))
    (println "  検品後の状態:" (:rma/state (store/rma-record s "rma-1")))
    (let [held (run-req! actor "res" {:op :resolve-return
                                      :patch {:rma-id "rma-1" :outcome :refund-full}} now)]
      (println "  status     :" (:status held) "← 返金は必ず人間")
      (let [ok (g/run* actor {:approval {:status :approved :by "ops-01"}}
                       {:thread-id "res" :resume? true})]
        (println "  --- 人間 ops-01 が承認 ---")
        (println "  status     :" (:status ok))
        (let [i (last (store/refund-instructions s))]
          (println "  返金指示   :" (:refund/amount-minor i) (:refund/currency i)
                   "by" (:refund/authorised-by i))
          (println "  実行済み?  :" (:refund/executed? i) "← 実行は精算 actor のレール層"))))))

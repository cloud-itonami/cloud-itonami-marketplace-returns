(ns returnops.advisor
  "ReturnsAdvisor.

  Eight ops from a closed allowlist: opening a return request,
  authorizing or declining it under the seller's published policy,
  recording that the buyer posted it back, recording that the goods
  arrived, recording what was found on inspection, resolving the return,
  and flagging a concern.

  CRITICAL: every proposal's `:effect` is `:propose`, and no op executes
  a refund. `:resolve-return` records a human's decision and produces a
  refund INSTRUCTION; moving money belongs to the settlement actor's
  rail adapter, which refuses without a named human of its own.

  What this advisor cannot fake: the eligibility answer. It is computed
  by `marketplace.returns/eligibility` from the seller's published
  policy and the ORDER actor's delivery fact, and the governor re-derives
  it from the store. An advisor cannot open a return window by asserting
  a delivery date."
  (:require [marketplace.returns :as ret]
            [returnops.store :as store]))

(defprotocol Advisor
  (-advise [advisor store request] "store + request -> proposal map"))

(defn- propose-open
  [st {:keys [patch]}]
  (let [r (ret/rma (assoc patch
                          :amount-minor (or (:amount-minor patch)
                                            (store/order-amount-minor
                                             st (:order patch) (:seller patch)))
                          :delivered-at (store/delivered-at st (:order patch) (:seller patch))))]
    {:op      :open-rma
     :rma-id  (:rma/id r)
     :summary (str (:rma/id r) " の返品要求を受付: " (pr-str (:rma/reason r)))
     :rationale "買い手からの返品申し出の受付記録のみ。可否はポリシー照合に委ね、返金は行わない。"
     :cites   (vec (keep identity [(:rma/order r) (:rma/seller r)]))
     :effect  :propose
     :value   {:rma r}
     :confidence 0.93}))

(defn- with-rma [st rma-id f]
  (when-let [r (store/rma-record st rma-id)] (f r)))

(defn- propose-authorize
  [st {:keys [patch]}]
  (let [rma-id (:rma-id patch)
        r (store/rma-record st rma-id)
        e (when r (store/eligibility-for st r (:now patch)))
        r' (when (and r e) (ret/authorize r e (select-keys patch
                                                           [:authorized-at :return-label :carrier])))]
    {:op      :authorize-return
     :rma-id  rma-id
     :summary (str rma-id " の返品を承認（出品者の公開ポリシー適用）")
     :rationale "出品者が公開している返品ポリシーの機械的な適用のみ。返送物の状態や返金額の判断は行わない。"
     :cites   [(str rma-id) (str (:rma/seller r))]
     :effect  :propose
     :value   {:rma-id rma-id :rma r' :eligibility e}
     :confidence 0.92}))

(defn- propose-decline
  [st {:keys [patch]}]
  (let [rma-id (:rma-id patch)
        r (store/rma-record st rma-id)
        e (when r (store/eligibility-for st r (:now patch)))
        r' (when (and r e) (ret/decline r e (select-keys patch [:declined-at :note])))]
    {:op      :decline-return
     :rma-id  rma-id
     :summary (str rma-id " の返品をポリシー上不可として却下: " (pr-str (:reasons e)))
     :rationale "公開ポリシーに基づく却下の記録のみ。買い手が不服の場合は紛争受付へ進める経路が残る。"
     :cites   [(str rma-id)]
     :effect  :propose
     :value   {:rma-id rma-id :rma r' :eligibility e}
     :confidence 0.9}))

(defn- propose-shipment
  "The buyer posted the goods back. Recording a tracking number here is
  what lets a seller distinguish 'never sent it' from 'in the post',
  which is the difference between chasing the buyer and waiting."
  [st {:keys [patch]}]
  (let [rma-id (:rma-id patch)
        r' (with-rma st rma-id #(ret/advance % :in-transit))]
    {:op      :record-return-shipment
     :rma-id  rma-id
     :summary (str rma-id " の返送を記録: " (pr-str (:tracking patch)))
     :rationale "買い手が返送した事実の記録のみ。中身や状態はまだ判断しない。"
     :cites   [(str rma-id)]
     :effect  :propose
     :value   {:rma-id rma-id :rma (some-> r' (assoc :rma/tracking (:tracking patch)))}
     :confidence 0.93}))

(defn- propose-receive
  [st {:keys [patch]}]
  (let [rma-id (:rma-id patch)
        r' (with-rma st rma-id #(ret/advance % :received))]
    {:op      :receive-return
     :rma-id  rma-id
     :summary (str rma-id " の返送品を受領")
     :rationale "返送品の到着という観測事実の記録のみ。中身の状態はまだ判断しない。"
     :cites   [(str rma-id)]
     :effect  :propose
     :value   {:rma-id rma-id :rma r'}
     :confidence 0.94}))

(defn- propose-inspection
  [st {:keys [patch]}]
  (let [rma-id (:rma-id patch)
        r' (with-rma st rma-id #(ret/record-inspection % (select-keys patch
                                                                     [:condition :inspected-by
                                                                      :inspected-at :note])))]
    {:op      :record-inspection
     :rma-id  rma-id
     :summary (str rma-id " の検品結果を記録: " (pr-str (:condition patch)))
     :rationale "返送品の状態の観測記録のみ。誰の責任か、返金するかの判断は含まない。"
     :cites   [(str rma-id)]
     :effect  :propose
     :value   {:rma-id rma-id :rma r'}
     :confidence 0.9}))

(defn- propose-resolve
  "ALWAYS escalates. The advisor drafts the outcome the operator asked
  for; the human who approves the interrupt is the one whose name lands
  on the record."
  [st {:keys [patch]}]
  (let [rma-id (:rma-id patch)
        r' (with-rma st rma-id
                     #(ret/resolve-return % (assoc (select-keys patch
                                                                [:outcome :refund-minor
                                                                 :decided-at :rationale])
                                                   :decided-by (or (:decided-by patch)
                                                                   "pending-human-approval"))))]
    {:op      :resolve-return
     :rma-id  rma-id
     :summary (str rma-id " の返品処理の確定を提案: " (pr-str (:outcome patch)))
     :rationale "返品結果の草案提示のみ。確定は人間が行い、返金の実行は精算 actor のレール層が別途行う。"
     :cites   [(str rma-id)]
     :effect  :propose
     :value   {:rma-id rma-id :rma r'
               :outcome (:outcome patch)
               :refund-minor (or (:refund-minor patch) 0)}
     :confidence (or (:confidence patch) 0.85)}))

(defn- propose-concern
  [_st {:keys [patch]}]
  {:op      :flag-return-concern
   :rma-id  (:rma-id patch)
   :summary (str (:rma-id patch) " の返品に関する懸念フラグ: " (pr-str (:concern patch "unknown")))
   :rationale "観察された返品上の懸念事実の報告のみ。不正の断定や返金拒否の判断は行わない。"
   :cites   [(str (:rma-id patch))]
   :effect  :propose
   :value   patch
   :confidence (or (:confidence patch) 0.8)})

(defn infer
  [st {:keys [op out-of-scope?] :as request}]
  (let [proposal (case op
                   :open-rma            (propose-open st request)
                   :authorize-return    (propose-authorize st request)
                   :decline-return      (propose-decline st request)
                   :record-return-shipment (propose-shipment st request)
                   :receive-return      (propose-receive st request)
                   :record-inspection   (propose-inspection st request)
                   :resolve-return      (propose-resolve st request)
                   :flag-return-concern (propose-concern st request)
                   {})]
    ;; Test hook: inject scope-excluded content to exercise the
    ;; governor's scope-exclusion block. Clear before production use.
    (if out-of-scope?
      (update proposal :rationale str " -- actually refunded the buyer and credited the buyer")
      proposal)))

(defn trace [_request proposal]
  {:t          :advisor-proposal
   :op         (:op proposal)
   :rma-id     (:rma-id proposal)
   :summary    (:summary proposal)
   :confidence (:confidence proposal)})

(defn mock-advisor []
  (reify Advisor
    (-advise [_ store request] (infer store request))))

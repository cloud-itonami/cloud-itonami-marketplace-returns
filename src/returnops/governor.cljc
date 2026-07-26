(ns returnops.governor
  "ReturnsGovernor.

  ## Where the line sits, and why it is not where the other actors' is

  Every other governor in this stack refuses to decide. This one
  decides eligibility, and that is deliberate.

  A dispute is a CONTESTED claim, so nobody automated may rule on it. A
  return is the seller's OWN PUBLISHED POLICY applied to facts nobody
  disagrees about — delivered on the 1st, window 14 days, today is the
  5th, category returnable. Refusing to answer that mechanically, on the
  grounds that all decisions need humans, would make returns unusable
  while protecting nobody: the buyer waits days for a person to read a
  calendar.

  What stays human is the RESOLUTION — whether the goods came back as
  described, and whether money moves. That is a judgement with money
  attached, so `:resolve-return` always escalates, exactly like
  settlement's release.

  Seven HARD checks, ALL permanent:

    1. Order/seller unknown  -- an RMA must attach to a real sub-order
                                from `-marketplace-order`.
    2. No published policy   -- a seller with no policy on file gets no
                                mechanical eligibility answer. Inventing
                                a default window would be inventing the
                                seller's terms for them.
    3. Malformed RMA         -- delegated to
                                `marketplace.returns/rma-errors`,
                                including the refusal of any record
                                claiming an ACTOR adjudicated.
    4. Authorized while
       ineligible            -- an ineligible return must go through
                                `:decline-return`, which records a
                                reason the buyer can act on and can
                                escalate to a dispute. Quietly
                                authorizing it instead destroys that
                                path.
    5. Illegal transition    -- `marketplace.returns/rma-transitions` is
                                the table; this actor obeys it. In
                                particular nothing reaches `:resolved`
                                without passing `:inspected`.
    6. Refund over the order -- a refund larger than the seller's part of
                                the order. Caught here as well as capped
                                in the library, because a refund is the
                                one number nobody re-reads until it is on
                                a bank statement.
    7. Effect / scope        -- `:effect` must be `:propose`; any claim
                                to have REFUNDED or PAID is a permanent
                                scope exclusion, as is any op outside the
                                closed allowlist.

  ESCALATE (SOFT):
    - LLM confidence below the floor.
    - `:resolve-return` and `:flag-return-concern` ALWAYS."
  (:require [clojure.string :as str]
            [marketplace.returns :as ret]
            [returnops.store :as store]))

(def confidence-floor 0.6)

(def allowed-ops
  "The closed proposal-op allowlist. CRITICAL: no op executes a refund.
  `:resolve-return` records a human's decision and produces a refund
  INSTRUCTION; moving the money is the settlement actor's rail adapter,
  which refuses without a named human of its own."
  #{:open-rma :authorize-return :decline-return
    ;; The buyer actually posting the goods back is its own state. The
    ;; transition table has always required :authorized -> :in-transit ->
    ;; :received, and it is right to: an authorized return that was never
    ;; shipped cannot be "received", and a seller chasing a return needs
    ;; to know which of the two it is.
    :record-return-shipment
    :receive-return
    :record-inspection :resolve-return :flag-return-concern})

(def always-escalate-ops
  #{:resolve-return :flag-return-concern})

(def scope-excluded-terms
  "Case-insensitive substrings marking a proposal as claiming money
  already moved.

  CRITICAL: phrased as the COMPLETED action ('refunded the buyer'),
  never a bare noun like 'refund' or 'return' — a bare noun would match
  inside this actor's own legitimate proposals, whose whole job is to
  talk about refunds, and self-block the happy path."
  ["refunded the buyer" "have refunded" "issued the refund"
   "processed the refund" "money has been returned" "credited the buyer"
   "reversed the charge" "paid the buyer back"
   "返金を実行した" "返金した" "返金処理を完了した" "入金した"
   "代金を返した" "チャージバックを実行した"])

;; ----------------------------- checks -----------------------------

(defn- rma-of [proposal st]
  (or (get-in proposal [:value :rma])
      (some->> (get-in proposal [:value :rma-id]) (store/rma-record st))))

(defn- unknown-target-violations
  [proposal st]
  (when (= :open-rma (:op proposal))
    (let [r (get-in proposal [:value :rma])]
      (cond
        (not (map? r))
        [{:rule :rma-missing :detail "返品要求の草案がない"}]

        (nil? (store/order-record st (:rma/order r)))
        [{:rule :order-unknown :detail (str (:rma/order r))}]

        (nil? (store/order-amount-minor st (:rma/order r) (:rma/seller r)))
        [{:rule :seller-not-on-order :detail (str (:rma/seller r))}]

        (zero? (store/order-amount-minor st (:rma/order r) (:rma/seller r)))
        [{:rule :seller-not-on-order :detail (str (:rma/seller r))}]

        (nil? (store/policy-for st (:rma/seller r)))
        [{:rule :no-published-policy
          :detail (str (:rma/seller r)
                       " は返品ポリシーを公開していない -- 既定値の代入は"
                       "出品者の契約条件を勝手に決めることになる")}]))))

(defn- malformed-rma-violations
  [proposal st]
  (when-let [r (rma-of proposal st)]
    (when-let [errs (seq (ret/rma-errors r))]
      (mapv (fn [e] {:rule (:returns.error/code e)
                     :detail (or (:returns.error/detail e)
                                 (name (:returns.error/code e)))})
            errs))))

(defn- authorize-violations
  "An `:authorize-return` must actually be eligible under the seller's
  published policy, re-derived from the STORE."
  [proposal st now]
  (when (= :authorize-return (:op proposal))
    (if-let [r (rma-of proposal st)]
      (let [e (store/eligibility-for st r now)]
        (when-not (:eligible? e)
          [{:rule :authorized-while-ineligible
            :detail (str "ポリシー上不可: " (pr-str (:reasons e))
                         " -- :decline-return を使うこと（買い手が争える経路が残る）")
            :reasons (:reasons e)}]))
      [{:rule :rma-unknown}])))

(defn- transition-violations
  "Every state-changing op must be a move the table allows."
  [proposal st]
  (let [to (case (:op proposal)
             :authorize-return :authorized
             :decline-return   :declined
             :record-return-shipment :in-transit
             :receive-return   :received
             :record-inspection :inspected
             :resolve-return   :resolved
             nil)]
    (when to
      (if-let [r (some->> (get-in proposal [:value :rma-id]) (store/rma-record st))]
        (when-not (contains? (get ret/rma-transitions (:rma/state r) #{}) to)
          [{:rule :illegal-transition
            :detail (str (:rma/state r) " -> " to " は許可されていない遷移"
                         " (許可: " (pr-str (get ret/rma-transitions (:rma/state r) #{})) ")")}])
        [{:rule :rma-unknown :detail (str (get-in proposal [:value :rma-id]))}]))))

(defn- refund-over-order-violations
  "A refund larger than the seller's part of the order.

  The library caps it; this refuses it. Both, because a refund is the
  one number nobody re-reads until it is on a bank statement, and a cap
  that silently changes a human's stated figure is its own surprise."
  [proposal st]
  (when (= :resolve-return (:op proposal))
    (when-let [r (rma-of proposal st)]
      (let [want (or (get-in proposal [:value :refund-minor]) 0)
            cap  (store/order-amount-minor st (:rma/order r) (:rma/seller r))]
        (when (> want cap)
          [{:rule :refund-exceeds-order
            :detail (str "返金 " want " が注文額 " cap " を超えている")}])))))

(defn- effect-not-propose-violations [proposal]
  (when (not= :propose (:effect proposal))
    [{:rule :effect-not-propose
      :detail (str ":effect は :propose のみ許可されるが " (pr-str (:effect proposal)) " が提案された")}]))

(defn- text-blob [proposal]
  (str/lower-case (pr-str (select-keys proposal [:op :summary :rationale :cites :value]))))

(defn- scope-exclusion-violations [proposal]
  (let [op (:op proposal)
        blob (text-blob proposal)]
    (cond
      (not (contains? allowed-ops op))
      [{:rule :op-not-allowed
        :detail (str (pr-str op) " は許可された操作(closed allowlist)に含まれない")}]

      (some #(str/includes? blob %) scope-excluded-terms)
      [{:rule :scope-excluded
        :detail "返金の実行に触れる提案は永久に禁止 -- 本 actor は指示を作るだけ"}])))

(defn check
  "Censors a ReturnsAdvisor proposal. `context` supplies `:now`."
  [_request context proposal store]
  (let [now (:now context)
        hard (into []
                   (concat (unknown-target-violations proposal store)
                           (malformed-rma-violations proposal store)
                           (authorize-violations proposal store now)
                           (transition-violations proposal store)
                           (refund-over-order-violations proposal store)
                           (effect-not-propose-violations proposal)
                           (scope-exclusion-violations proposal)))
        conf (:confidence proposal 0.0)
        low? (< conf confidence-floor)
        stakes? (boolean (always-escalate-ops (:op proposal)))
        hard? (boolean (seq hard))]
    {:ok?          (and (not hard?) (not low?) (not stakes?))
     :violations   hard
     :confidence   conf
     :hard?        hard?
     :escalate?    (and (not hard?) (or low? stakes?))
     :high-stakes? stakes?}))

(defn hold-fact [request context verdict]
  {:t          :governor-hold
   :op         (:op request)
   :actor      (:actor-id context)
   :rma-id     (:rma-id request)
   :disposition :hold
   :basis      (mapv :rule (:violations verdict))
   :violations (:violations verdict)
   :confidence (:confidence verdict)})

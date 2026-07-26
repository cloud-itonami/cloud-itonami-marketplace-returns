(ns returnops.store
  "SSoT for the marketplace returns actor.

  Directories, keyed by STRING ids:

    orders    multi-seller orders from `-marketplace-order`. Read-only:
              what was bought, and when it was delivered, are that
              actor's facts.
    policies  seller id -> that seller's PUBLISHED return policy. The
              policy is the seller's, not the platform's, which is what
              makes `eligibility` a mechanical question rather than a
              judgement — see `marketplace.returns`.
    rmas      the return requests themselves, the one thing this actor
              owns.

  A resolved refund produces a `:refund/*` instruction record. This
  store never executes one; `cloud-itonami-marketplace-settlement`'s
  rail adapter is the only place that talks to a payment rail, and even
  there it refuses without a named human.

  The ledger stays append-only."
  (:require [marketplace.order :as order]
            [marketplace.returns :as ret]))

(defprotocol Store
  (order-record [s order-id])
  (all-order-records [s])
  (policy-for [s seller-id] "That seller's published return policy, or nil.")
  (all-policies [s])
  (rma-record [s rma-id])
  (all-rma-records [s])
  (refund-instructions [s] "Recorded refund instructions, never executed here.")
  (ledger [s])
  (returns-log [s])
  (commit-record! [s record])
  (append-ledger! [s fact])
  (with-policies [s policies]))

;; ----------------------------- demo data -----------------------------

(defn demo-order []
  (-> (order/order {:id "ord-1" :buyer "buyer-1"
                    :lines [{:seller "merchant.alpha" :sku "A1" :name "Cola"
                             :qty 2 :unit-price-minor 600}
                            {:seller "merchant.beta" :sku "B1" :name "Tea"
                             :qty 1 :unit-price-minor 1100}]})
      (order/advance-sub-order "merchant.alpha" :confirmed)
      (order/advance-sub-order "merchant.alpha" :packed)
      (order/advance-sub-order "merchant.alpha" :handed-over)
      (order/advance-sub-order "merchant.alpha" :delivered)))

(defn demo-data
  "Fixtures covering the happy path and each hard check.

    merchant.alpha  14-day window, 10% restocking, perishables excluded
    merchant.beta   0-day window (returns not accepted) -- a legitimate
                    published policy, and the one that proves a decline
                    is traceable to a rule rather than to a mood"
  []
  {:orders {"ord-1" (demo-order)}
   :policies {"merchant.alpha" (ret/return-policy
                                {:window-days 14 :restocking-fee-bps 1000
                                 :non-returnable #{:perishable :personalised}})
              "merchant.beta"  (ret/return-policy {:window-days 0})}
   :rmas {}
   :refunds []
   :delivered-at {"ord-1/merchant.alpha" "2026-06-01T10:00:00Z"}})

;; ----------------------------- MemStore -----------------------------

(defrecord MemStore [a]
  Store
  (order-record [_ id] (get-in @a [:orders id]))
  (all-order-records [_] (sort-by :order/id (vals (:orders @a))))
  (policy-for [_ sel] (get-in @a [:policies sel]))
  (all-policies [_] (:policies @a))
  (rma-record [_ id] (get-in @a [:rmas id]))
  (all-rma-records [_] (sort-by :rma/id (vals (:rmas @a))))
  (refund-instructions [_] (:refunds @a))
  (ledger [_] (:ledger @a))
  (returns-log [_] (:returns-log @a))
  (commit-record! [_ record]
    (swap! a update :returns-log conj record)
    (let [{:keys [op value payload]} record]
      (case op
        :open-rma
        (when-let [r (:rma value)]
          (swap! a assoc-in [:rmas (:rma/id r)] r))

        (:authorize-return :decline-return :record-return-shipment
         :receive-return :record-inspection :resolve-return)
        (when-let [r (:rma value)]
          ;; The resolution carries the approver stamped by the operation
          ;; graph, so the stored record names the human who decided.
          (let [r' (cond-> r
                     (and (= :resolve-return op) (:approved-by payload))
                     (assoc-in [:rma/resolution :resolution/decided-by]
                               (:approved-by payload)))]
            (swap! a assoc-in [:rmas (:rma/id r')] r')
            (when (= :resolve-return op)
              (when-let [i (ret/refund-instruction r')]
                (swap! a update :refunds conj i)))))

        nil))
    record)
  (append-ledger! [_ fact] (swap! a update :ledger conj fact) fact)
  (with-policies [s policies] (when (seq policies) (swap! a assoc :policies policies)) s))

(defn seed-db []
  (->MemStore (atom (assoc (demo-data) :ledger [] :returns-log []))))

(defn mem-store [m]
  (->MemStore (atom (merge {:orders {} :policies {} :rmas {} :refunds []
                            :delivered-at {} :ledger [] :returns-log []}
                           m))))

;; ----------------------------- derived views -----------------------------

(defn delivered-at
  "When this seller's parcel was delivered, or nil.

  Read from the order actor's own delivery facts. A `nil` here is what
  makes `marketplace.returns/eligibility` answer `:not-yet-delivered`
  rather than guessing a window from the order date."
  [s order-id seller]
  (get-in @(:a s) [:delivered-at (str order-id "/" seller)]))

(defn sub-order-delivered?
  "Whether the ORDER actor says this seller's parcel landed. Cross-checked
  against `delivered-at` so a stale delivery timestamp cannot open a
  return window for a parcel still in transit."
  [s order-id seller]
  (boolean (some-> (order-record s order-id) (order/seller-delivered? seller))))

(defn eligibility-for
  "Run `marketplace.returns/eligibility` for an RMA using THIS store's
  seller policy and the order actor's delivery fact."
  [s r now]
  (let [pol (policy-for s (:rma/seller r))
        delivered (when (sub-order-delivered? s (:rma/order r) (:rma/seller r))
                    (delivered-at s (:rma/order r) (:rma/seller r)))]
    (ret/eligibility {:policy pol
                      :category (:rma/category r)
                      :delivered-at delivered
                      :now now})))

(defn order-amount-minor
  "What this seller's part of the order actually cost — the ceiling on
  any refund."
  [s order-id seller]
  (or (some-> (order-record s order-id) (order/seller-subtotal-minor seller)) 0))

(ns returnops.phase
  "Phase 0->3 staged rollout for the marketplace returns actor.

    Phase 0  read-only          -- no writes, still governor-gated.
    Phase 1  assisted-intake    -- return requests may be opened, every
                                   write needs human approval.
    Phase 2  assisted-policy    -- adds the mechanical policy steps
                                   (authorize / decline) and receipt,
                                   still approval-gated.
    Phase 3  supervised auto    -- governor-clean intake, policy
                                   application, receipt and inspection
                                   recording may auto-commit.

  `:resolve-return` and `:flag-return-concern` are absent from every
  phase's `:auto` set, INCLUDING phase 3.

  ## Why the policy steps may run unattended

  This is the one governor in the stack that decides something, and the
  phase table has to agree with it or the point is lost. Authorizing a
  return within the seller's own published window is not a judgement —
  it is a calendar and a category lookup. Making a buyer wait days for a
  person to read a date protects nobody and is the single most common
  reason returns feel hostile.

  Declining is auto-eligible for the same reason AND a further one: a
  decline records the policy reasons that produced it, and a buyer who
  disagrees can escalate it to a dispute
  (`marketplace.returns/->dispute-reason`). An automatic decline with a
  stated reason and an appeal route is more accountable than a slow
  human one with neither.

  What stays human is `:resolve-return`: whether the goods came back as
  described, and whether money moves. `returnops.governor`'s
  `always-escalate-ops` enforces the same independently."
  (:require [returnops.governor :as governor]))

(def read-ops #{})
(def write-ops governor/allowed-ops)

;; NOTE the invariant: `:resolve-return` and `:flag-return-concern` are
;; members of `write-ops` but are NEVER members of any phase's `:auto`
;; set below. Do not add them there.
(def phases
  {0 {:label "read-only"        :writes #{}             :auto #{}}
   1 {:label "assisted-intake"  :writes #{:open-rma}     :auto #{}}
   2 {:label "assisted-policy"  :writes #{:open-rma :authorize-return
                                          :decline-return :record-return-shipment
                                          :receive-return}
      :auto #{}}
   3 {:label "supervised-auto"  :writes write-ops
      :auto #{:open-rma :authorize-return :decline-return
              :record-return-shipment :receive-return :record-inspection}}})

(def default-phase 3)

(defn gate
  [phase {:keys [op]} governor-disposition]
  (let [{:keys [writes auto]} (get phases phase (get phases default-phase))]
    (cond
      (= :hold governor-disposition)       {:disposition :hold :reason nil}
      (contains? read-ops op)              {:disposition governor-disposition :reason nil}
      (not (contains? writes op))          {:disposition :hold :reason :phase-disabled}
      (and (= :commit governor-disposition)
           (not (contains? auto op)))      {:disposition :escalate :reason :phase-approval}
      :else                                {:disposition governor-disposition :reason nil})))

(defn verdict->disposition
  [verdict]
  (cond (:hard? verdict) :hold
        (:escalate? verdict) :escalate
        :else :commit))

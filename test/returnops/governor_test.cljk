(ns returnops.governor-test
  (:require [clojure.test :refer [deftest is testing]]
            [marketplace.returns :as ret]
            [returnops.advisor :as advisor]
            [returnops.governor :as governor]
            [returnops.store :as store]))

(def now "2026-06-05T09:00:00Z")
(def late "2026-07-01T09:00:00Z")
(defn- ctx [& [t]] {:actor-id "returns-actor" :phase 3 :now (or t now)})

(defn- db [] (store/seed-db))

(defn- advise [st op patch]
  (advisor/-advise (advisor/mock-advisor) st {:op op :patch patch}))

(defn- check [st op patch & [t]]
  (governor/check {:op op} (ctx t) (advise st op patch) st))

(defn- open-patch [& {:as over}]
  (merge {:id "rma-1" :order "ord-1" :seller "merchant.alpha" :buyer "buyer-1"
          :reason :not-as-described :category :beverages
          :lines [{:sku "A1" :qty 1}] :currency "JPY"}
         over))

(defn- with-rma [st & {:as over}]
  (let [p (advise st :open-rma (apply open-patch (mapcat identity (or over {}))))]
    (store/commit-record! st {:op :open-rma :value (:value p)})
    st))

;; ───────────────── intake ─────────────────

(deftest a-clean-return-request-is-accepted
  (let [v (check (db) :open-rma (open-patch))]
    (is (false? (:hard? v)) (pr-str (:violations v)))
    (is (true? (:ok? v)))))

(deftest the-amount-is-taken-from-the-order-not-the-request
  (let [st (db)
        p (advise st :open-rma (open-patch :amount-minor nil))]
    (is (= 1200 (:rma/amount-minor (get-in p [:value :rma])))
        "merchant.alpha's part of ord-1")))

(deftest a-request-against-an-unknown-order-or-seller-is-refused
  (is (some #{:order-unknown} (mapv :rule (:violations (check (db) :open-rma (open-patch :order "nope"))))))
  (is (some #{:seller-not-on-order}
            (mapv :rule (:violations (check (db) :open-rma (open-patch :seller "merchant.nobody")))))))

(deftest a-seller-with-no-published-policy-gets-no-mechanical-answer
  (testing "inventing a default window would be inventing the seller's
            contract terms for them"
    (let [st (store/mem-store {:orders (:orders (store/demo-data))
                               :policies {}
                               :delivered-at (:delivered-at (store/demo-data))})
          v (governor/check {:op :open-rma} (ctx) (advise st :open-rma (open-patch)) st)]
      (is (true? (:hard? v)))
      (is (some #{:no-published-policy} (mapv :rule (:violations v)))))))

(deftest an-unknown-return-reason-is-refused
  (is (some #{:unknown-return-reason}
            (mapv :rule (:violations (check (db) :open-rma (open-patch :reason :vibes)))))))

(deftest a-record-claiming-the-actor-adjudicated-is-refused
  (let [st (db)
        p (advise st :open-rma (open-patch))
        tampered (assoc-in p [:value :rma :rma/adjudicated-by-actor?] true)
        v (governor/check {:op :open-rma} (ctx) tampered st)]
    (is (true? (:hard? v)))
    (is (some #{:actor-adjudicated-return} (mapv :rule (:violations v))))))

;; ───────────────── the mechanical policy step ─────────────────

(deftest an-eligible-return-authorizes-unattended
  (testing "authorizing within the seller's own published window is a
            calendar and a category lookup, not a judgement"
    (let [st (with-rma (db))
          v (check st :authorize-return {:rma-id "rma-1" :now now})]
      (is (false? (:hard? v)) (pr-str (:violations v)))
      (is (true? (:ok? v))))))

(deftest authorizing-an-ineligible-return-is-a-HARD-block
  (testing "an ineligible return must go through :decline-return, which
            records a reason the buyer can act on and can escalate to a
            dispute — quietly authorizing it destroys that path"
    (let [st (with-rma (db))
          v (check st :authorize-return {:rma-id "rma-1" :now late} late)]
      (is (true? (:hard? v)))
      (is (some #{:authorized-while-ineligible} (mapv :rule (:violations v))))
      (is (some #{:outside-window}
                (:reasons (first (filter #(= :authorized-while-ineligible (:rule %))
                                         (:violations v)))))))))

(deftest a-non-returnable-category-cannot-be-authorized
  (let [st (with-rma (db) :category :perishable)
        v (check st :authorize-return {:rma-id "rma-1" :now now})]
    (is (true? (:hard? v)))
    (is (some #{:authorized-while-ineligible} (mapv :rule (:violations v))))))

(deftest a-seller-who-accepts-no-returns-declines-traceably
  (testing "merchant.beta publishes a 0-day window — a legitimate policy,
            and the one that proves a decline points at a rule"
    (let [st (db)
          p (advise st :open-rma (open-patch :seller "merchant.beta" :order "ord-1"))]
      ;; beta is on ord-1 but its parcel is not delivered, so the reason
      ;; is :not-yet-delivered rather than :outside-window — either way it
      ;; is a stated rule, not a mood.
      (store/commit-record! st {:op :open-rma :value (:value p)})
      (let [d (advise st :decline-return {:rma-id "rma-1" :now now})]
        (is (seq (:reasons (get-in d [:value :eligibility]))))
        (is (false? (:hard? (governor/check {:op :decline-return} (ctx) d st))))))))

(deftest declining-is-auto-eligible
  (testing "an automatic decline with a stated reason and an appeal route
            is more accountable than a slow human one with neither"
    (let [st (with-rma (db))
          v (check st :decline-return {:rma-id "rma-1" :now late} late)]
      (is (false? (:hard? v)) (pr-str (:violations v)))
      (is (true? (:ok? v))))))

;; ───────────────── transitions ─────────────────

(deftest nothing-reaches-resolved-without-passing-inspected
  (let [st (with-rma (db))]
    (is (some #{:illegal-transition}
              (mapv :rule (:violations (check st :resolve-return
                                              {:rma-id "rma-1" :outcome :refund-full})))))
    (is (some #{:illegal-transition}
              (mapv :rule (:violations (check st :record-inspection
                                              {:rma-id "rma-1" :condition :as-described
                                               :inspected-by "wh"})))))))

(deftest the-happy-path-walks-the-table
  (let [st (with-rma (db))]
    (doseq [[op patch] [[:authorize-return {:rma-id "rma-1" :now now}]
                        [:record-return-shipment {:rma-id "rma-1" :tracking "RT-1"}]
                        [:receive-return {:rma-id "rma-1"}]
                        [:record-inspection {:rma-id "rma-1" :condition :as-described
                                             :inspected-by "wh-01"}]]]
      (let [p (advise st op patch)
            v (governor/check {:op op} (ctx) p st)]
        (is (false? (:hard? v)) (str op " " (pr-str (:violations v))))
        (store/commit-record! st {:op op :value (:value p)})))
    (is (= :inspected (:rma/state (store/rma-record st "rma-1"))))
    (testing "and now resolution is reachable — but escalates"
      (let [v (check st :resolve-return {:rma-id "rma-1" :outcome :refund-full})]
        (is (false? (:hard? v)) (pr-str (:violations v)))
        (is (true? (:high-stakes? v)))
        (is (false? (:ok? v)))))))

;; ───────────────── money ─────────────────

(deftest a-refund-larger-than-the-order-is-refused
  (testing "a refund is the one number nobody re-reads until it is on a
            bank statement"
    (let [st (with-rma (db))]
      (doseq [op [:authorize-return :record-return-shipment :receive-return]]
        (store/commit-record! st {:op op :value (:value (advise st op {:rma-id "rma-1" :now now}))}))
      (store/commit-record! st {:op :record-inspection
                                :value (:value (advise st :record-inspection
                                                       {:rma-id "rma-1" :condition :used
                                                        :inspected-by "wh-01"}))})
      (let [v (check st :resolve-return {:rma-id "rma-1" :outcome :refund-partial
                                         :refund-minor 99999})]
        (is (true? (:hard? v)))
        (is (some #{:refund-exceeds-order} (mapv :rule (:violations v))))))))

;; ───────────────── structural ─────────────────

(deftest effect-must-be-propose
  (let [st (db)
        v (governor/check {:op :open-rma} (ctx)
                          (assoc (advise st :open-rma (open-patch)) :effect :commit) st)]
    (is (true? (:hard? v)))
    (is (some #{:effect-not-propose} (mapv :rule (:violations v))))))

(deftest no-op-in-the-allowlist-executes-a-refund
  (doseq [op [:execute-refund :pay-buyer :reverse-charge]]
    (let [v (governor/check {:op op} (ctx) {:op op :effect :propose :confidence 0.99} (db))]
      (is (true? (:hard? v)) (str op))
      (is (some #{:op-not-allowed} (mapv :rule (:violations v))) (str op)))))

(deftest scope-exclusion-blocks-claims-money-already-moved
  (let [st (db)
        p (advisor/infer st {:op :open-rma :patch (open-patch) :out-of-scope? true})
        v (governor/check {:op :open-rma} (ctx) p st)]
    (is (true? (:hard? v)))
    (is (some #{:scope-excluded} (mapv :rule (:violations v))))))

(deftest default-mock-advisor-proposals-never-self-trip-scope-exclusion
  (testing "legitimate proposals necessarily talk about refunds"
    (let [st (with-rma (db))]
      (doseq [[op patch] [[:open-rma (open-patch :id "rma-2")]
                          [:authorize-return {:rma-id "rma-1" :now now}]
                          [:decline-return {:rma-id "rma-1" :now late}]
                          [:receive-return {:rma-id "rma-1"}]
                          [:flag-return-concern {:rma-id "rma-1" :concern "返品頻度が高い"}]]]
        (let [v (check st op patch)]
          (is (not-any? #{:scope-excluded} (mapv :rule (:violations v))) (str op)))))))

(deftest resolution-and-concern-always-escalate
  (is (= #{:resolve-return :flag-return-concern} governor/always-escalate-ops)))

(deftest low-confidence-escalates
  (let [st (db)
        v (governor/check {:op :open-rma} (ctx)
                          (assoc (advise st :open-rma (open-patch)) :confidence 0.2) st)]
    (is (false? (:hard? v)))
    (is (true? (:escalate? v)))))

;; ───────────────── the bridge back ─────────────────

(deftest a-declined-return-carries-an-escalation-path
  (let [st (with-rma (db))
        d (advise st :decline-return {:rma-id "rma-1" :now late})
        declined (get-in d [:value :rma])]
    (is (= :declined (:rma/state declined)))
    (is (true? (ret/escalatable? declined)))
    (is (= :not-as-described (ret/->dispute-reason declined)))))

(ns returnops.edge.worker
  "The returns actor's Worker.

  The distinction this whole repo exists to hold: an RMA is not a
  dispute. A return is the seller's own published policy being applied
  mechanically — eligible or not, within the window or not. A dispute is
  a disagreement, and nothing here adjudicates one; `->dispute-reason`
  deliberately answers nil for `:changed-mind`, because changing your
  mind is not a grievance.

  No op executes a refund. `:resolve-return` records a HUMAN's decision
  and produces a refund INSTRUCTION on an append-only stream; moving the
  money is the settlement actor's rail adapter, which refuses without a
  named human of its own. So a refund needs two named people and this
  actor is neither of them.

  Orders are read from the shared ref, written by `-marketplace-order`."
  (:require [marketplace.edge :as edge]
            [returnops.advisor :as advisor]
            [returnops.governor :as governor]
            [returnops.phase :as phase]
            [returnops.store :as store]))

(def ^:private ops
  {:advise      (fn [st req] (advisor/-advise (advisor/mock-advisor) st req))
   :check       governor/check
   :disposition phase/verdict->disposition
   :gate        phase/gate
   :commit!     (fn [st proposal req]
                  (store/commit-record! st {:op (:op proposal)
                                            :rma-id (:rma-id req)
                                            :value (:value proposal)
                                            ;; :payload carries the approver the
                                            ;; resolution is attributed to.
                                            :payload (assoc (:value proposal)
                                                            :approved-by (:approved-by req))}))
   :ledger!     store/append-ledger!
   :hold-fact   governor/hold-fact})

(defn- ctx [body]
  {:actor-id "returnops-edge"
   :phase (get body "phase" 3)
   :now (get body "now" "2026-06-01T00:00:00Z")})

(defn- run [client wants body op patch ref]
  (edge/with-store
    {:client client :wants wants :store-fn store/kotobase-store}
    (fn [st]
      (edge/outcome ref (edge/run ops st (ctx body)
                                  {:op op :rma-id (get body "rma-id") :ref ref
                                   :approved-by (get body "decided-by")
                                   :patch patch})))))

(defn- set-policy
  "A seller's PUBLISHED return policy. Published is the load-bearing
  word: eligibility is decided by applying the policy the buyer could
  read at purchase time, not one written afterwards to suit the case."
  [client body]
  (let [sel (get body "seller")]
    (edge/with-store
      {:client client :wants {:return-policy [sel]} :store-fn store/kotobase-store}
      (fn [st]
        (store/with-policies st {sel {:policy/window-days (get body "window-days")
                                      :policy/restocking-bp (get body "restocking-bp" 0)
                                      :policy/return-shipping (keyword (get body "return-shipping" "buyer"))
                                      :policy/excluded (set (map keyword (get body "excluded" [])))}})
        {:ref sel :disposition "commit" :violations []}))))

;; ───────────────────────── routes ─────────────────────────

(defn- gated [request env f]
  (if-not (edge/authorised? request env)
    (js/Promise.resolve (edge/json {:error "unauthorised"} 401))
    (-> (.json request) (.then #(f (js->clj %))) (.then #(edge/json % 200)))))

(defn- routes [client request env method path _url]
  (cond
    (and (= method "POST") (= path "/policies")) (gated request env #(set-policy client %))

    (and (= method "POST") (= path "/rma"))
    (gated request env
           (fn [b] (run client {:order [(get b "order")] :return-policy :all :rma :all}
                        b :open-rma
                        {:id (get b "rma-id")
                         :order (get b "order") :seller (get b "seller")
                         :reason (keyword (get b "reason" "other"))
                         :opened-at (get b "now" "2026-06-01T00:00:00Z")
                         :amount-minor (get b "amount-minor")}
                        (get b "rma-id"))))

    (and (= method "POST") (= path "/authorize"))
    (gated request env
           (fn [b] (run client {:rma [(get b "rma-id")] :return-policy :all :order :all}
                        b :authorize-return
                        {:rma-id (get b "rma-id")
                         :now (get b "now" "2026-06-01T00:00:00Z")
                         :authorized-at (get b "now" "2026-06-01T00:00:00Z")
                         :carrier (get b "carrier")
                         :return-label (get b "return-label")}
                        (get b "rma-id"))))

    (and (= method "POST") (= path "/receive"))
    (gated request env
           (fn [b] (run client {:rma [(get b "rma-id")] :return-policy :all :order :all}
                        b :receive-return
                        {:rma-id (get b "rma-id")
                         :received-at (get b "now" "2026-06-01T00:00:00Z")}
                        (get b "rma-id"))))

    ;; Needs `decided-by`. A resolution attributed to nobody is how a
    ;; refund becomes untraceable.
    (and (= method "POST") (= path "/resolve"))
    (gated request env
           (fn [b] (if (or (nil? (get b "decided-by")) (= "" (str (get b "decided-by"))))
                     (js/Promise.resolve {:ref (get b "rma-id") :disposition "hold"
                                          :violations ["no-named-decider"]})
                     (run client {:rma [(get b "rma-id")] :return-policy :all :order :all}
                          b :resolve-return
                          {:rma-id (get b "rma-id")
                           :outcome (keyword (get b "outcome" "refund"))
                           :refund-minor (get b "refund-minor")
                           :decided-by (get b "decided-by")
                           :resolved-at (get b "now" "2026-06-01T00:00:00Z")}
                          (get b "rma-id")))))

    (and (= method "GET") (= path "/rma"))
    (if-not (edge/authorised? request env)
      (js/Promise.resolve (edge/json {:error "unauthorised"} 401))
      (-> (edge/read-all client :rma)
          (.then (fn [rs]
                   (edge/json {:rmas (mapv (fn [r] {:rma-id (:rma/id r)
                                                    :order (:rma/order r)
                                                    :seller (:rma/seller r)
                                                    :status (str (:rma/status r))
                                                    :reason (str (:rma/reason r))})
                                           rs)}
                              200)))))

    :else nil))

(def app
  (clj->js
   {:fetch (fn [request env _ctx]
             (edge/serve "cloud-itonami-marketplace-returns" request env routes))}))

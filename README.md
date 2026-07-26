# cloud-itonami-marketplace-returns

Open Business Blueprint (implemented actor): **returns and RMAs — the
path to the `:refunded` escrow state that had no way to reach it.**

**ReturnsAdvisor ⊣ ReturnsGovernor** on
[`langgraph`](https://github.com/kotoba-lang/langgraph); the contract is
`marketplace.returns` in
[`kotoba-lang/marketplace`](https://github.com/kotoba-lang/marketplace).
Design record:
[ADR-2607264000](https://github.com/com-junkawasaki/root/blob/main/90-docs/adr/2607264000-marketplace-federated-commerce-layer.edn).

## An RMA is not a dispute

This distinction is the whole design, and it is why this is the one
governor in the stack that decides something.

A **dispute** is a contested claim — the buyer says one thing, the
seller another — so nobody automated may rule on it, which is why
`marketplace.crossborder` refuses to and why no actor in this fleet
adjudicates.

A **return** is the seller's own published policy applied to facts
nobody disagrees about: delivered on the 1st, window 14 days, today is
the 5th, category returnable. Refusing to answer that mechanically, on
the grounds that all decisions need humans, would make returns unusable
while protecting nobody — the buyer waits days for a person to read a
calendar.

What stays human is the **resolution**: whether the goods came back as
described, and whether money moves.

## The window counts from delivery

A parcel that took three weeks to arrive has not used up the buyer's
window while it was in transit. Day counting is calendar-correct across
month and year boundaries — a naive day-of-month subtraction says 31 Jan
to 2 Feb is −29 days — and compares dates, not clock times, so nobody
loses a day to a 23:00 delivery.

## Seven HARD checks

| Check | What it catches |
|---|---|
| **Order/seller unknown** | an RMA against nothing real |
| **No published policy** | a seller with no policy on file — inventing a default window would be inventing their contract terms |
| **Malformed RMA** | including any record claiming an *actor* adjudicated |
| **Authorized while ineligible** | see below |
| **Illegal transition** | nothing reaches `:resolved` without passing `:inspected` |
| **Refund over the order** | a refund larger than the seller's part of the order |
| **Effect / scope** | any claim to have refunded or paid; any op outside the allowlist |

**Authorizing an ineligible return is a hard block, not a shortcut.** It
must go through `:decline-return`, which records the policy reasons that
produced it — and a buyer who disagrees can escalate that to a dispute
via `marketplace.returns/->dispute-reason`. Quietly authorizing it
instead would destroy that path. An automatic decline with a stated
reason and an appeal route is more accountable than a slow human one
with neither.

`:changed-mind` deliberately has **no** dispute counterpart: a buyer who
changed their mind outside the window has no contested claim, and
manufacturing one would let the dispute path reopen every policy
decision.

## Money

A seller may not charge a restocking fee for **their own mistake** —
wrong item, damaged, defective, not-as-described all force the fee to
zero. That is not a jurisdiction question; it is arithmetic about who
caused the return.

A resolved refund produces a refund **instruction**, never an execution.
Moving money is
[`-marketplace-settlement`](https://github.com/cloud-itonami/cloud-itonami-marketplace-settlement)'s
rail adapter, which refuses without a named human of its own. The amount
is capped at what was actually paid in the library *and* refused by the
governor — a refund is the one number nobody re-reads until it is on a
bank statement.

## The state the transition table always required

`:authorized → :in-transit → :received`. The buyer actually posting the
goods back is its own state, so `:record-return-shipment` exists: a
seller chasing a return needs to know whether it was never sent or is in
the post, and those are different conversations.

```bash
clojure -M:dev:run   # eligible authorize, out-of-window decline, human-gated resolution
clojure -M:test      # 21 tests, 54 assertions
clojure -M:lint
```

## Rollout phases

| Phase | Writes | Auto-commits |
|---|---|---|
| 0 read-only | — | — |
| 1 assisted-intake | `:open-rma` | — |
| 2 assisted-policy | + authorize / decline / shipment / receive | — |
| 3 supervised-auto | all | all except `:resolve-return` and `:flag-return-concern` |

# Trade policy in a vertical bargaining model

`as_vertical_trade_fit(fit)` enables policy counterfactuals on a calibrated
`VerticalFit`. Promotion copies the complete model, fitted primitives and
observations without calibration or an equilibrium solve. It accepts only zero
`tariffPre` and unconstrained (`Inf`) `quotaPre`; nonzero or finite baseline
policies must be introduced through `simulate()`.

```r
policy_fit <- as_vertical_trade_fit(fit)
result <- simulate(
    policy_fit,
    counterfactual(tariff = list(up = .10, down = .05))
)
```

Tier fields are named lists with optional `up` and `down` numeric vectors.
Scalars expand to all products. Omitted tiers retain the preceding policy;
initial tariffs are zero and initial quotas are `Inf`. Supplying `0` tariffs or
`Inf` quotas explicitly removes the indicated restriction. Policies may accompany
ownership and cost changes. Product exit remains unsupported.

## Physical prices, costs and profits

Prices are gross purchaser payments. An upstream tariff `tu` leaves seller
receipts `(1-tu)*w`; a downstream tariff `td` leaves receipts `(1-td)*p`.
Physical unit operating profits are

```text
mu_up   = (1-tu)*w - cu
mu_down = (1-td)*p - w - cd.
```

Fitted physical marginal-cost primitives `cu` and `cd` do not change when a
tariff changes. Separate cost counterfactuals retain the inner model's existing
cost convention and compound from the originating fit. The tariff applies to
integrated links as well. The source model's internal-transfer convention sets
upstream net operating profit to zero, before any upstream capacity rent.

Consumers choose using gross retail prices in Bertrand models. Flat second-score
auctions use their source model's transfer-cost winner probabilities and expected
payment rule. Gross revenue is `p*q`; producer surplus uses the physical net
profits above. Duties are excluded from producer surplus, so the accounting
identity is

```text
PS_up + PS_down + (tu*w + td*p)*q = (p-cu-cd)*q.
```

Price changes report gross purchaser prices. They are not reconstructed from
changes in net operating profits when tariffs vary.

## Bargaining and downstream pricing

The policy solver retains the source Nash-in-Nash convention: bilateral transfer
negotiations hold downstream prices and product-removal disagreement allocations
fixed. Each party's gain includes its whole owned upstream and downstream
portfolio, including ownership across tiers. If `S` and `B` are the seller's and
buyer's gains over product removal and `b` is the fitted bargaining parameter,
the bilateral objective is `S^(1-b) * B^b`. A transfer change has seller derivative
`(1-tu)*q` and buyer derivative `-q`. The resulting condition is

```text
b*S = (1-b)*(1-tu)*B.
```

This seller receipt derivative changes the bargaining condition as well as the
level of seller profits. Downstream Bertrand prices maximize each retailer's
entire owned portfolio profit, including its upstream holdings and the derivative
of downstream net receipts. Nested Logit uses the exact nested product-removal
allocation for disagreement payoffs, retaining the outside option.

## Capacity restrictions

Quotas are multiples of the originating fitted physical quantity. For product
`j` at either tier,

```text
capacity[k,j] = quota[k,j] * originating_quantity[j].
```

The reference quantity is captured on promotion and remains fixed through every
step and resume. One physical flow passes through both tiers. `Inf` is
unconstrained, including when the originating quantity is zero.

```r
result <- simulate(
    policy_fit,
    counterfactual(quota = list(down = c(.80, Inf, Inf, Inf)))
)
```

The multiplier at the constrained tier represents a unit capacity opportunity
cost. The solver uses `mu_up-lambda_up` and `mu_down-lambda_down` in that tier's
portfolio pricing and bargaining incentives; reported producer surplus includes
the scarcity rent. For every finite capacity it verifies stationarity and

```text
q <= capacity
lambda >= 0
lambda * (capacity-q) = 0.
```

A primal-dual active-set solver first solves the unconstrained system, activates
violated capacities, and drops constraints with negative multipliers. It rejects
cycling, nonconvergence and candidates failing its numerical KKT checks.
Independent tests reconstruct demand, whole-portfolio profit derivatives and
removal payoffs rather than reusing the solver residual.

The implementation deliberately rejects:

- Any tariff and finite quota on the same tier, even for different products.
- Finite quotas on both tiers of the same product; allocating the common
  scarcity rent would require an additional mechanism.
- Finite quotas outside full-chain Bertrand models, including second-score
  auctions; constrained auction allocation has not been derived.
- A zero quota on positive fitted output; these Logit products have no finite
  price that attains zero output.
- Downstream tariffs in second-score auctions. Merely scaling expected margins
  while retaining the source winner allocation does not derive the allocation
  implied by net-of-tax payoffs. Upstream auction tariffs are supported.

## Sequences and diagnostics

```r
path <- simulate(
    policy_fit,
    add_step(
        counterfactual(tariff = list(up = .10)),
        tariff = list(down = .05),
        costs = list(up = rep(-.01, 4), down = rep(0, 4))
    )
)
continued <- simulate(path, counterfactual(tariff = list(up = .08)))
vertical_policy_diagnostics(final_result(continued))
```

The source fit stays unchanged. Policies and compounded costs persist across
steps, path resume and direct `simulate_steps()` resume. `simulate_steps()` uses
the cumulative costs saved on its `last_result` when none are supplied explicitly.

`vertical_policy_diagnostics()` accepts either a promoted fit or a simulated
vertical model. Each tier reports tariff, seller retention, quota, physical
capacity and quantity, shadow value, slack, complementarity residual, FOC
residual and solver status. `calcDiagnostics()` also dispatches to these results.
Pass `preMerger=TRUE` to inspect the promoted pre-state. Simulation accepts
`policy_control=list(tol=1e-8, maxit=500, max_active_sets=100)`.

## Source behavior retained

Zero policy reproduces ordinary vertical simulation, including merger results,
fitted demand and bargaining parameters. Promotion preserves every model slot.
The source flat-auction model has a legacy pre/post outside-price normalization:
with a nonzero outside price, its no-change post equilibrium can differ from its
pre equilibrium. The policy layer preserves that behavior and tests identity
against ordinary simulation. It does not relabel that legacy difference as a
policy effect. Physical auction quantities consistently use the model's vertical
winner probabilities, rather than evaluating the downstream auction in isolation.

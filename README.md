# vertical

`vertical` provides vertical supply-chain bargaining and merger simulation on
top of the structural state and demand systems in `antitrust`. It owns the
upstream/downstream classes, Nash-in-Nash bargaining, vertical ownership, and
vertical counterfactual lifecycle methods; reusable demand and structural
machinery remain in `antitrust`.

## Installation

The coordinated dependency minimums are `antitrust >= 0.99.43` and, for the
optional tariff-policy integration, `trade >= 0.9.0`. During development,
CI follows antitrust and trade `master` and coordination `main`, and records
their resolved SHAs.
The release-certification workflow also installs the matching coordination
and trade candidates; it requires a full 40-character SHA for each source
dependency and checks the multi-entry `Remotes` field against the selected
antitrust and trade commits.

```r
fit <- vertical::calibrate(
  demand = "logit", conduct = "vertical_bargaining",
  prices = prices_down, shares = shares_down, margins = margins_down,
  ownerPre = owner_down, pricesUp = prices_up,
  marginsUp = margins_up, ownerPreUp = owner_up
)

result <- vertical::simulate(
  fit,
  ownerPost = list(up = owner_up_post, down = owner_down_post)
)
```

Hybrid transactions use the same interface. A transaction is classified as
hybrid when it creates both new same-tier co-control and new common control
across the upstream and downstream tiers:

```r
hybrid <- vertical::simulate(
  fit,
  ownerPost = list(
    up = rep("U1", length(owner_up)),
    down = c("U1", "U1", "D2", "D2")
  )
)

vertical::merger_type(hybrid)
#> [1] "hybrid"

# Hybrid concentration is reported separately for the two relevant markets.
vertical::hhi(hybrid, preMerger = FALSE)
```

The pre-extraction numerical fixtures under `baseline/` and
`tests/testthat/fixtures/` are reviewed parity references. They contain
economic outputs only and do not serialize the old implementation.

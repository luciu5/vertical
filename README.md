# vertical

`vertical` provides vertical supply-chain bargaining and merger simulation on
top of the structural state and demand systems in `antitrust`. It owns the
upstream/downstream classes, Nash-in-Nash bargaining, vertical ownership, and
vertical counterfactual lifecycle methods; reusable demand and structural
machinery remain in `antitrust`.

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

The pre-extraction numerical fixtures under `baseline/` and
`tests/testthat/fixtures/` are reviewed parity references. They contain
economic outputs only and do not serialize the old implementation.

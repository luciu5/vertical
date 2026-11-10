# vertical

`vertical` calibrates linked upstream/downstream bargaining markets and simulates
horizontal, vertical, and hybrid mergers. It reuses antitrust's structural
lifecycle and demand machinery, and owns the bargaining and policy equations.

## Installation

Run this in a fresh R session. The shared iovrse installer installs `vertical`
and its required compatible dependencies.

```r
source(url("https://raw.githubusercontent.com/luciu5/competitiontoolbox/master/tools/install_iovrse.R"))
install_iovrse(packages = "vertical")
```

Use `install_iovrse()` for the complete cohort. The examples below use vertical's
own model and policy APIs.

## Task guides

1. [A linked supply-chain merger](vignettes/linked-supply-chain-merger.Rmd): calibrate this market and interpret a hybrid merger.
2. [Tariffs and quotas in a fitted supply chain](vignettes/vertical-tariffs-quotas.Rmd): preserve the fit, apply policy and read capacity/FOC diagnostics.

The policy guide includes its own small setup. The ordinary installer skips
vignette building; these source guides remain readable without Pandoc. Source
bundles built with vignettes also provide `browseVignettes("vertical")`.

## A four-product market

Start with four independent supplier/retailer pairs and an outside share of 35%.
For this teaching example, Logit demand with coefficient -1 implies the markup
`1 / (1 - shares)` for each independent retailer. Equal upstream/downstream
dollar markups imply bargaining power 0.5. The resulting observations have
positive upstream production costs `(1, 1.2, .9, 1.1)` and downstream costs
`(.5, .6, .4, .55)`.

Downstream margins are retailer operating margins after wholesale purchases;
upstream margins are supplier operating margins. Each margin is divided by its
own tier's price.

```r
shares <- c(.15, .20, .12, .18)
markup <- 1 / (1 - shares)
prices_up <- c(1, 1.2, .9, 1.1) + markup
prices_down <- prices_up + c(.5, .6, .4, .55) + markup
owners_up <- c("U1", "U2", "U3", "U4")
owners_down <- c("D1", "D2", "D3", "D4")

fit <- vertical::calibrate(
  "logit", conduct = "vertical_bargaining",
  prices = prices_down, shares = shares, margins = markup / prices_down,
  ownerPre = owners_down, pricesUp = prices_up,
  marginsUp = markup / prices_up, ownerPreUp = owners_up,
  insideSize = 65, labels = c("one", "two", "three", "four")
)
vertical::calcMC(fit@model, preMerger = TRUE)
```

`fit@model` holds the linked structural model; `calcMC()` reports the recovered
upstream and downstream costs.

## Simulate a hybrid merger

The first two suppliers consolidate, and the same owner takes over their two
retailers. This creates both same-tier consolidation and cross-tier integration.
The fitted market remains available for other scenarios.

```r
result <- vertical::simulate(
  fit,
  ownerPost = list(
    up = c("U1", "U1", "U3", "U4"),
    down = c("U1", "U1", "D3", "D4")
  )
)
vertical::merger_type(result)
vertical::calcPrices(result, preMerger = FALSE)
# merger_type(result) is "hybrid".
vertical::hhi(result, preMerger = FALSE)
```

`ownerPost` always supplies the upstream and downstream ownership vectors.
Use `counterfactual()` and `add_step()` for supported sequential scenarios;
`update(fit, ...)` recalibrates revised observations under the same model.

## Implemented models

```r
vertical::supportedModels()
```

| Demand | Variant | Downstream game |
| --- | --- | --- |
| `"logit"` | `"standard"` | Bertrand pricing and upstream bargaining |
| `"logit_nests"` | `"standard"` | Nested Logit pricing and upstream bargaining; supply `nests` |
| `"logit"` | `"auction2nd"` | Second-score auction and upstream bargaining |

Select these with `calibrate(demand = ..., variant = ...)`. The registry marks
calibration and simulation support explicitly. Structural `specify()` and model
`respecify()` routes are not implemented for these vertical models.

## Tariffs and quotas

Promote a calibrated fit to its policy-aware form, then apply a scenario using
vertical's policy solver. Promotion preserves the fitted state.

```r
policy_fit <- vertical::as_vertical_trade_fit(fit)
tariff_result <- vertical::simulate(
  policy_fit, vertical::counterfactual(tariff = list(up = .02))
)
vertical::calcPrices(tariff_result, preMerger = FALSE)
```

Finite quotas use the full-chain Bertrand model. Flat second-score auctions
support upstream tariffs; their downstream tariffs and finite quotas require
an allocation model that is not implemented. See `?as_vertical_trade_fit` for
the policy units, supported combinations, and equilibrium diagnostics. Optional
workflows that also call `trade` can install it with
`install_iovrse(packages = c("vertical", "trade"))`.

## Contributor and release checks

The runtime minimum is `antitrust >= 0.99.43`; optional trade interoperability
uses `trade >= 0.9.0`. Release certification verifies exact antitrust/trade
revisions and the coordination test candidate in CI. Ordinary users use the
shared bootstrap above and do not supply release SHAs.

The pre-extraction files under `baseline/` and `tests/testthat/fixtures/` are
historical parity evidence. They are maintained separately from the self-contained
example above.

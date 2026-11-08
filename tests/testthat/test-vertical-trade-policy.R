# Migration parity A: permanent economic/API oracles. Each test identifies a
# distinct material failure; none uses the private policy residual as its oracle.

test_that("A: neutral promotion cannot relabel unmodelled baseline tax receipts or rationing", {
  fit <- suppressWarnings(vertical_calibrate())
  before <- serialize(fit, NULL)
  promoted <- as_vertical_trade_fit(fit)
  expect_identical(serialize(fit, NULL), before)
  expect_identical(promoted@parameters, fit@parameters)
  expect_identical(promoted@observed, fit@observed)
  for (slot in slotNames(fit@model)) {
    expect_identical(slot(promoted@model, slot), slot(fit@model, slot))
  }
  expect_error(as_vertical_trade_fit(fit, tariffPre = list(up = .1)),
    "policy neutral.*simulate")
  expect_error(as_vertical_trade_fit(fit, quotaPre = list(down = 2)),
    "policy neutral.*simulate")
  expect_error(as_vertical_trade_fit(promoted), "already")
})

test_that("A: zero policy preserves ordinary simulation equilibria across all registered demand variants", {
  for (case in c("flat_vertical", "nested_bertrand", "second_score")) {
    if (case == "second_score") {
      x <- vertical_inputs()
      fit <- suppressWarnings(vertical::calibrate("logit", prices = x$prices_down,
        shares = x$shares_down, margins = x$margins_down,
        ownerPre = x$owner_pre_down, pricesUp = x$prices_up,
        marginsUp = x$margins_up, ownerPreUp = x$owner_pre_up,
        insideSize = x$inside_size, priceOutside = x$price_outside,
        variant = "auction2nd"))
    } else fit <- suppressWarnings(vertical_calibrate(case))
    promoted <- as_vertical_trade_fit(fit)
    zero <- simulate(promoted, counterfactual(tariff = list(up = 0, down = 0)))
    # Second-score legacy pre/post outside-price normalization can differ.
    # Identity means the policy layer introduces no additional change.
    expect_equal(zero@up@pricePost, fit@model@up@pricePost, tolerance = 1e-6)
    expect_equal(zero@down@pricePost, fit@model@down@pricePost, tolerance = 1e-6)
    owners <- list(up = rep("U1", 4), down = fit@model@down@ownerPre)
    plain <- suppressWarnings(simulate(fit, counterfactual(ownership = owners)))
    policy <- simulate(promoted, counterfactual(ownership = owners))
    expect_equal(policy@up@pricePost, plain@up@pricePost, tolerance = 1e-6)
    expect_equal(policy@down@pricePost, plain@down@pricePost, tolerance = 1e-6)
    expect_equal(calcProducerSurplus(policy, FALSE), calcProducerSurplus(plain, FALSE),
      tolerance = 1e-6)
    expect_equal(CV(policy), CV(plain), tolerance = 1e-6)
    expect_identical(policy@down@slopes, fit@model@down@slopes)
  }
})

test_that("A: upstream, downstream and both-tier tariffs satisfy physical net profits and portfolio Nash FOCs", {
  fit <- suppressWarnings(vertical_calibrate())
  promoted <- as_vertical_trade_fit(fit)
  policies <- list(list(up = c(.1, .03, 0, .04), down = 0),
    list(up = 0, down = c(.02, .05, 0, .03)),
    list(up = c(.1, .03, 0, .04), down = c(.02, .05, 0, .03)))
  for (tariff in policies) {
    model <- simulate(promoted, counterfactual(tariff = tariff))
    q <- policy_expect_stationarity(model, tariff)
    mu <- (1 - tariff$up) * model@up@pricePost - model@up@mcPost
    md <- (1 - tariff$down) * model@down@pricePost -
      model@up@pricePost - model@down@mcPost
    profit <- calcProducerSurplus(model, FALSE)
    expect_equal(unname(profit$up), unname(mu * q), tolerance = 1e-6)
    expect_equal(unname(profit$down), unname(md * q), tolerance = 1e-6)
    gross <- model@down@pricePost * q
    duty <- (tariff$up * model@up@pricePost + tariff$down * model@down@pricePost) * q
    expect_equal(unname(profit$up + profit$down + duty),
      unname(gross - (model@up@mcPost + model@down@mcPost) * q), tolerance = 1e-6)
    expect_equal(unname(calcRevenues(model, FALSE)), unname(gross), tolerance = 1e-6)
    expect_identical(model@up@mcPost, fit@model@up@mcPre)
    expect_identical(model@down@mcPost, fit@model@down@mcPre)
    expect_equal(calcPriceDelta(model, levels = TRUE),
      list(up = model@up@pricePost - model@up@pricePre,
        down = model@down@pricePost - model@down@pricePre))
  }
})

test_that("A: tariffs retain cross-tier portfolio gains and the nested removal outside option", {
  for (case in c("flat_vertical", "nested_bertrand")) {
    fit <- suppressWarnings(vertical_calibrate(case))
    tariff <- list(up = c(.08, .03, .06, .01), down = c(.03, .02, .04, 0))
    ownership_cases <- list(
      list(up = rep("U1", 4), down = c("U1", "U1", "D2", "D2")),
      # Here the negotiating buyer owns other upstream products, so the
      # seller-retention derivative also changes the buyer's upstream gain.
      list(up = fit@model@up@ownerPre, down = c("U2", "D1", "D2", "D2")))
    for (ownership in ownership_cases) {
      model <- simulate(as_vertical_trade_fit(fit),
        counterfactual(tariff = tariff, ownership = ownership))
      policy_expect_stationarity(model, tariff)
      expect_equal(unname(calcQuantities(model, FALSE)), policy_quantities(model),
        tolerance = 1e-7)
      integrated <- ownership$up == ownership$down
      expect_equal((1 - tariff$up[integrated]) * model@up@pricePost[integrated],
        unname(model@up@mcPost[integrated]), tolerance = 1e-6)
    }
  }
})

test_that("A: quota scarcity rents meet independent stationarity, feasibility and complementarity by tier", {
  fit <- suppressWarnings(vertical_calibrate())
  promoted <- as_vertical_trade_fit(fit)
  original <- policy_quantities(fit@model, retail = fit@model@down@pricePre,
    wholesale = fit@model@up@pricePre, preMerger = TRUE)
  models <- list()
  for (tier in c("up", "down")) {
    quota <- list(up = rep(Inf, 4), down = rep(Inf, 4))
    quota[[tier]] <- c(.8, 2, Inf, Inf)
    model <- simulate(promoted, counterfactual(quota = quota))
    diagnostics <- policy_expect_kkt(model, quota, original)
    expect_gt(diagnostics[[tier]]$shadow_value[1], 0)
    expect_equal(diagnostics[[tier]]$shadow_value[2], 0)
    models[[tier]] <- model
  }
  # The cap fixes the same physical flow, while quota ownership determines
  # which tier receives the scarcity rent and hence the negotiated transfer.
  expect_equal(models$up@down@pricePost, models$down@down@pricePost, tolerance = 1e-6)
  expect_gt(models$up@up@pricePost[1], models$down@up@pricePost[1])
  mixed <- list(up = c(.8, Inf, Inf, Inf), down = c(Inf, Inf, .9, Inf))
  model <- simulate(promoted, counterfactual(quota = mixed))
  policy_expect_kkt(model, mixed, original)
})

test_that("A: nested and integrated multiproduct quotas retain opposite-tier tariff opportunity costs", {
  for (case in c("flat_vertical", "nested_bertrand")) {
    fit <- suppressWarnings(vertical_calibrate(case))
    promoted <- as_vertical_trade_fit(fit)
    original <- policy_quantities(fit@model, retail = fit@model@down@pricePre, preMerger = TRUE)
    quota <- list(up = rep(Inf, 4), down = c(.8, Inf, .9, Inf))
    tariff <- list(up = .05, down = 0)
    owner <- list(up = fit@model@up@ownerPre, down = c("U1", "U1", "D2", "D2"))
    model <- simulate(promoted, counterfactual(ownership = owner,
      tariff = tariff, quota = quota))
    policy_expect_kkt(model, quota, original, tariff)
    quota <- list(up = c(.7, Inf, Inf, Inf), down = rep(Inf, 4))
    tariff <- list(up = 0, down = .03)
    model <- simulate(promoted, counterfactual(ownership = owner,
      tariff = tariff, quota = quota))
    policy_expect_kkt(model, quota, original, tariff)
  }
})

test_that("A: policy sequences and direct/path resume preserve original capacities and compounded costs", {
  fit <- suppressWarnings(vertical_calibrate())
  promoted <- as_vertical_trade_fit(fit)
  first <- counterfactual(costs = list(up = rep(-.03, 4), down = rep(-.02, 4)),
    quota = list(up = c(.85, Inf, Inf, Inf)))
  second <- counterfactual(costs = list(up = rep(-.01, 4), down = rep(-.04, 4)))
  combined <- add_step(first, costs = second@steps[[1]]@changes$costs)
  path <- simulate(promoted, combined)
  single <- simulate(promoted, first)
  resumed <- simulate_steps(promoted, single, second@steps)
  expect_equal(resumed$results[[1]]@up@pricePost, final_result(path)@up@pricePost,
    tolerance = 1e-7)
  expect_equal(resumed$results[[1]]@down@pricePost, final_result(path)@down@pricePost,
    tolerance = 1e-7)
  expect_equal(calcMC(final_result(path), TRUE),
    calcMC(path@results[[1]], FALSE), tolerance = 1e-8)
  expect_equal(unname(final_result(path)@up@mcPost),
    unname(fit@model@up@mcPre * .97 * .99), tolerance = 1e-8)
  expect_equal(unname(final_result(path)@down@mcPost),
    unname(fit@model@down@mcPre * .98 * .96), tolerance = 1e-8)
  original <- policy_quantities(fit@model, retail = fit@model@down@pricePre, preMerger = TRUE)
  quota <- list(up = c(.85, Inf, Inf, Inf), down = rep(Inf, 4))
  policy_expect_kkt(final_result(path), quota, original)
  continued <- simulate(path, counterfactual(quota = list(up = Inf),
    tariff = list(up = .08, down = .03)))
  final <- final_result(continued)
  expect_equal(final@up@mcPost, final_result(path)@up@mcPost)
  expect_equal(final@down@mcPost, final_result(path)@down@mcPost)
  policy_expect_stationarity(final, list(up = .08, down = .03))
  expect_identical(final@down@slopes, fit@model@down@slopes)
  tariff_continued <- simulate(continued, counterfactual(costs = list(
    up = rep(-.01, 4), down = rep(0, 4))))
  policy_expect_stationarity(final_result(tariff_continued), list(up = .08, down = .03))
  expect_equal(unname(final_result(tariff_continued)@up@mcPost),
    unname(fit@model@up@mcPre * .97 * .99 * .99), tolerance = 1e-8)
})

test_that("A: second-score upstream tariffs retain winner probabilities and auction payment identities", {
  x <- vertical_inputs()
  fit <- suppressWarnings(vertical::calibrate("logit", prices = x$prices_down,
    shares = x$shares_down, margins = x$margins_down,
    ownerPre = x$owner_pre_down, pricesUp = x$prices_up,
    marginsUp = x$margins_up, ownerPreUp = x$owner_pre_up,
    insideSize = x$inside_size, priceOutside = x$price_outside,
    variant = "auction2nd"))
  promoted <- as_vertical_trade_fit(fit)
  tariff <- list(up = c(.05, .1, 0, .02), down = 0)
  model <- simulate(promoted, counterfactual(tariff = tariff))
  q <- policy_expect_stationarity(model, tariff)
  shares <- q / model@down@mktSize
  firm <- vapply(seq_along(shares), function(j)
    sum(shares[model@down@ownerPost == model@down@ownerPost[j]]), numeric(1))
  payment_margin <- log(1 - firm) / (model@down@slopes$alpha * firm)
  expect_equal(unname(model@down@pricePost),
    unname(model@up@pricePost + model@down@mcPost + payment_margin), tolerance = 1e-6)
  expect_equal(unname(calcQuantities(model, FALSE)), q, tolerance = 1e-8)
  expect_equal(unname(calcRevenues(model, FALSE)), unname(model@down@pricePost * q),
    tolerance = 1e-6)
  expect_error(simulate(promoted, counterfactual(tariff = list(down = .05))),
    "downstream tariffs.*second-score.*winner allocation")
  expect_error(simulate(promoted, counterfactual(quota = list(up = .8))),
    "second-score.*not supported")
})

test_that("A: unsupported rationing and exit fail before solving an economically different model", {
  fit <- as_vertical_trade_fit(suppressWarnings(vertical_calibrate()))
  expect_error(simulate(fit, counterfactual(tariff = list(up = .1),
    quota = list(up = c(Inf, .8, Inf, Inf)))), "same tier")
  expect_error(simulate(fit, counterfactual(quota = list(up = .8, down = .9))),
    "one finite quota tier")
  expect_error(simulate(fit, counterfactual(quota = list(down = 0))),
    "zero quota.*no finite-price")
  expect_error(simulate(fit, counterfactual(exit = seq_len(4L))), "retain at least one")
  expect_error(simulate(fit, counterfactual(tariff = list(up = 1))), "less than 1")
  expect_error(simulate(fit, counterfactual(quota = list(up = -1))), "non-negative")
  partial <- fit
  partial@model@chain_level <- "retailer"
  expect_error(simulate(partial, counterfactual(quota = list(up = .8))),
    "chain_level = 'full'")
})

# Category A: inner auction demand omits the negotiated-transfer adjustment;
# an HHI report can otherwise stay fixed while actual winner shares change.
test_that("A: policy auction concentration uses physical winner shares", {
  x <- vertical_inputs()
  fit <- suppressWarnings(vertical::calibrate("logit", prices = x$prices_down,
    shares = x$shares_down, margins = x$margins_down,
    ownerPre = x$owner_pre_down, pricesUp = x$prices_up,
    marginsUp = x$margins_up, ownerPreUp = x$owner_pre_up,
    insideSize = x$inside_size, priceOutside = x$price_outside,
    variant = "auction2nd"))
  result <- simulate(as_vertical_trade_fit(fit),
    counterfactual(tariff = list(up = c(.2, .1, 0, .04))))
  quantity <- policy_quantities(result)
  for (tier in c("upstream", "downstream")) {
    owner <- if (tier == "upstream") result@up@ownerPost else result@down@ownerPost
    for (revenue in c(FALSE, TRUE)) {
      mass <- if (revenue) quantity * result@down@pricePost else quantity
      firm_share <- tapply(mass / sum(mass), owner, sum)
      expected <- sum((100 * firm_share)^2)
      expect_equal(hhi(result, FALSE, revenue = revenue, tier = tier),
        unname(expected), tolerance = 1e-8)
    }
  }
})

# A (permanent): API exit must remove the linked physical flow and preserve
# survivor incentives, policies and cumulative masks through path promotion.
# Existing subset tests do not cover Counterfactual routing or resumed policy.
exit_foc <- function(model) {
  active <- model@down@subset
  margin <- calcMargins(model, FALSE, level = TRUE)
  c((model@up@pricePost - model@up@mcPost - margin$up)[active],
    (model@down@pricePost - model@up@pricePost - model@down@mcPost - margin$down)[active])
}

test_that("A: linked exits preserve survivor FOCs without demand recalibration", {
  for (kind in c("flat_vertical", "nested_bertrand")) {
    fit <- suppressWarnings(vertical_calibrate(kind))
    original <- serialize(fit, NULL)
    removed <- if (kind == "nested_bertrand") c(3L, 4L) else 3L
    model <- simulate(fit, counterfactual(exit = removed))
    expect_identical(model@down@slopes, fit@model@down@slopes)
    expect_identical(serialize(fit, NULL), original)
    expect_equal(unname(calcShares(model, FALSE)[removed]), rep(0, length(removed)))
    expect_true(all(is.na(model@up@pricePost[removed])))
    expect_true(all(is.na(model@down@pricePost[removed])))
    expect_lt(max(abs(exit_foc(model))), 1e-7)
    expect_equal(unname(calcQuantities(model, FALSE)),
      unname(policy_quantities(model)), tolerance = 1e-7)
    expect_equal(calcProducerSurplus(model, FALSE)$up[removed],
      setNames(rep(0, length(removed)), model@down@labels[removed]))
    expect_true(is.finite(CV(model)))
    promoted <- final_result(simulate(fit, add_step(counterfactual(exit = removed),
      exit = rep(TRUE, length(model@down@prices)))))
    survivor <- promoted@down@subset
    expect_true(all(is.finite(calcMargins(promoted, TRUE, level = TRUE)$down[survivor])))
    expect_true(is.finite(elast(promoted, TRUE, market = TRUE)))
    expect_true(all(is.finite(diversion(promoted, TRUE)[survivor, survivor])))
    expect_true(is.finite(CV(promoted)))
  }
})

test_that("A: policy exits retain capacity anchors and resume without reentry", {
  fit <- suppressWarnings(as_vertical_trade_fit(vertical_calibrate()))
  anchor <- calcQuantities(fit@model, TRUE)
  quota <- list(up = c(.8, Inf, Inf, Inf), down = rep(Inf, 4))
  path <- simulate(fit, add_step(counterfactual(quota = quota), exit = 3L))
  current <- final_result(path)
  policy_expect_kkt(current, quota, anchor)
  expect_equal(vertical_policy_diagnostics(current)$up$shadow_value[3], 0)
  expect_true(is.finite(CV(current)))
  resumed <- simulate(path, counterfactual(exit = c(TRUE, TRUE, TRUE, FALSE),
    costs = list(up = rep(-.01, 4), down = rep(0, 4))))
  last <- final_result(resumed)
  expect_identical(last@down@subset, c(TRUE, TRUE, FALSE, FALSE))
  policy_expect_kkt(last, quota, anchor)
  expect_true(all(is.finite(calcShares(last, TRUE)[c(1, 2, 4)])))
  expect_equal(unname(calcShares(last, TRUE)[3]), 0)
  expect_equal(vertical_policy_diagnostics(last)$up$capacity,
    vertical_policy_diagnostics(current)$up$capacity)
  expect_true(is.finite(CV(last)))
  expect_error(simulate(resumed, counterfactual(exit = c(1L, 2L))),
    "retain at least one")
})

test_that("A: ordinary resumed exits preserve old masks and reject malformed requests", {
  fit <- suppressWarnings(vertical_calibrate())
  path <- simulate(fit, add_step(counterfactual(exit = fit@model@down@labels[3]),
    exit = 4L))
  last <- final_result(path)
  expect_identical(last@down@subset, c(TRUE, TRUE, FALSE, FALSE))
  expect_lt(max(abs(exit_foc(last))), 1e-7)
  expect_true(is.finite(CV(last)))
  expect_equal(unname(calcQuantities(last, TRUE)[3]), 0)
  expect_true(is.finite(elast(last, TRUE, market = TRUE)))
  expect_lt(elast(last, TRUE, market = TRUE), 0)
  expect_true(all(is.finite(diversion(last, TRUE)[c(1, 2, 4), c(1, 2, 4)])))
  expect_true(all(is.finite(hhi(last, TRUE))))
  expect_true(all(is.finite(calcMargins(last, TRUE, level = TRUE)$down[c(1, 2, 4)])))
  resumed <- simulate(path, counterfactual(exit = rep(TRUE, 4)))
  expect_identical(final_result(resumed)@down@subset, last@down@subset)
  expect_lt(max(abs(exit_foc(final_result(resumed)))), 1e-7)
  expect_error(simulate(fit, counterfactual(exit = c(TRUE, NA, TRUE, TRUE))), "without NAs")
  expect_error(simulate(fit, counterfactual(exit = "missing")), "labels")
  expect_error(simulate(fit, counterfactual(exit = 1.5)), "valid product")
})

test_that("A: second-score linked exits preserve survivor auction equations", {
  x <- vertical_inputs()
  fit <- suppressWarnings(vertical::calibrate("logit", "vertical_bargaining",
    prices = x$prices_down, shares = x$shares_down, margins = x$margins_down,
    ownerPre = x$owner_pre_down, pricesUp = x$prices_up,
    marginsUp = x$margins_up, ownerPreUp = x$owner_pre_up,
    insideSize = x$inside_size, priceOutside = x$price_outside,
    labels = x$labels, variant = "auction2nd"))
  model <- simulate(fit, counterfactual(exit = 3L))
  expect_true(is.na(calcShares(model, FALSE)[3]))
  expect_true(all(is.finite(calcShares(model, FALSE)[c(1, 2, 4)])))
  # Physical winner probabilities independently follow wholesale costs.
  expect_equal(unname(calcShares(model, FALSE)[c(1, 2, 4)]),
    unname(policy_quantities(model)[c(1, 2, 4)]) / model@down@mktSize,
    tolerance = 1e-7)
  resumed <- simulate(simulate(fit, add_step(counterfactual(exit = 3L),
    exit = 4L)), counterfactual(exit = rep(TRUE, 4)))
  expect_identical(final_result(resumed)@down@subset, c(TRUE, TRUE, FALSE, FALSE))
})

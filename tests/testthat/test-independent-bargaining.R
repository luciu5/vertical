test_that("two-tier bargaining prices satisfy independent Logit best responses", {
  shares <- c(.15, .20)
  outside_share <- 1 - sum(shares)
  alpha <- -1
  bargaining_power <- .5
  upstream_cost <- c(1, 1.2)
  downstream_cost <- c(.5, .6)
  downstream_markup <- -1 / (alpha * (1 - shares))
  upstream_markup <- (1 - bargaining_power) /
    bargaining_power * downstream_markup
  wholesale_pre <- upstream_cost + upstream_markup
  retail_pre <- wholesale_pre + downstream_cost + downstream_markup

  fit <- suppressWarnings(vertical::vertical.barg(
    sharesDown = shares, pricesDown = retail_pre,
    marginsDown = downstream_markup / retail_pre,
    ownerPreDown = c("D1", "D2"), ownerPostDown = c("D1", "D2"),
    pricesUp = wholesale_pre, marginsUp = upstream_markup / wholesale_pre,
    ownerPreUp = c("U1", "U2"), ownerPostUp = c("U1", "U2"),
    mcDeltaUp = c(.1, 0), priceOutside = 0, insideSize = 35
  ))

  # The observed market was constructed from known demand, costs, and
  # bargaining power. Calibration must recover those primitives.
  expect_equal(fit@down@slopes$alpha, alpha, tolerance = 1e-5)
  expect_equal(unname(fit@up@bargpowerPre), rep(bargaining_power, 2),
               tolerance = 1e-5)
  expect_equal(unname(fit@up@mcPre), upstream_cost, tolerance = 1e-5)
  expect_equal(unname(fit@down@mcPre), downstream_cost, tolerance = 1e-5)
  expect_equal(unname(fit@up@mcPost), upstream_cost * c(1.1, 1),
               tolerance = 1e-5)

  mean_utility <- log(shares / outside_share) - alpha * retail_pre
  post_share <- function(retail_price) {
    exp_utility <- exp(mean_utility + alpha * retail_price)
    exp_utility / (1 + sum(exp_utility))
  }
  # For each independent upstream/downstream pair, the retailer's Logit
  # markup is -1/[alpha(1-share)]. Nash bargaining gives the upstream
  # partner (1-b)/b times that markup. Iterate those explicit best responses.
  retail_expected <- retail_pre
  for (iteration in seq_len(200L)) {
    share <- post_share(retail_expected)
    retail_markup <- -1 / (alpha * (1 - share))
    wholesale_expected <- upstream_cost * c(1.1, 1) +
      (1 - bargaining_power) / bargaining_power * retail_markup
    next_retail <- wholesale_expected + downstream_cost + retail_markup
    if (max(abs(next_retail - retail_expected)) < 1e-12) {
      retail_expected <- next_retail
      break
    }
    retail_expected <- next_retail
  }
  expect_lt(iteration, 200L)
  expect_equal(unname(fit@up@pricePost), unname(wholesale_expected),
               tolerance = 1e-7)
  expect_equal(unname(fit@down@pricePost), unname(retail_expected),
               tolerance = 1e-7)
  expect_equal(unname(calcShares(fit, FALSE)), unname(post_share(retail_expected)),
               tolerance = 1e-8)
})

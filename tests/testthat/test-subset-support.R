## Focused acceptance tests for inactive-product masks.  These tests exercise
## the public vertical lifecycle and the antitrust output conventions without
## reproducing either package's equilibrium engine.

subset_mask <- c(TRUE, TRUE, FALSE, TRUE)
subset_nest_mask <- c(TRUE, TRUE, FALSE, FALSE)

subset_fit <- function(demand = "logit", variant = "standard",
                       chain_level = "full") {
  x <- vertical_inputs()
  args <- list(
    demand = demand,
    conduct = "vertical_bargaining",
    prices = x$prices_down,
    shares = x$shares_down,
    margins = x$margins_down,
    ownerPre = x$owner_pre_down,
    pricesUp = x$prices_up,
    marginsUp = x$margins_up,
    ownerPreUp = x$owner_pre_up,
    insideSize = x$inside_size,
    priceOutside = x$price_outside,
    labels = x$labels,
    chain_level = chain_level
  )
  if (identical(demand, "logit_nests")) {
    args$nests <- c("n1", "n1", "n2", "n2")
  }
  if (identical(variant, "auction2nd")) args$variant <- variant
  do.call(vertical::calibrate, args)
}

subset_owner <- function() {
  x <- vertical_inputs()
  list(up = x$owner_pre_up,
       down = c("U1", "U1", "D2", "D2"))
}

subset_direct_args <- function(demand = "logit", variant = "standard",
                               chain_level = "full", subset = subset_mask) {
  args <- vertical_case_args("flat_vertical")
  args$chain_level <- chain_level
  args$subset <- subset
  if (identical(demand, "logit_nests")) {
    args$nests <- c("n1", "n1", "n2", "n2")
  }
  if (identical(variant, "auction2nd")) args$supplyDown <- "2nd"
  args
}

subset_foc <- function(object, chain_level = object@chain_level) {
  active <- object@down@subset
  prices <- list(up = object@up@pricePost,
                 down = object@down@pricePost)
  costs <- calcMC(object, preMerger = FALSE)
  margins <- calcMargins(object, preMerger = FALSE, level = TRUE)
  up <- prices$up - costs$up - margins$up
  down <- prices$down - prices$up - costs$down - margins$down
  if (identical(chain_level, "wholesaler")) {
    return(up[active])
  }
  if (identical(chain_level, "retailer")) {
    return(down[active])
  }
  c(up[active], down[active])
}

subset_second_score_foc <- function(object) {
  up <- object@up
  down <- object@down
  active <- down@subset
  price_up <- up@pricePost[active]
  price_down <- down@pricePost[active]
  costs <- calcMC(object, preMerger = FALSE)
  down@ownerPost <- ownerToMatrix(down, preMerger = FALSE)
  meanval <- down@slopes$meanval
  meanval[active] <- meanval[active] +
    down@slopes$alpha * (price_up - down@priceOutside)
  down@slopes$meanval <- meanval
  shares <- calcShares(down, preMerger = FALSE, revenue = FALSE)
  shares <- shares[active]
  margins_down <- calcMargins(down, preMerger = FALSE, level = TRUE)
  margins_down <- margins_down[active]
  diversion <- tcrossprod(1 / (1 - shares), shares) * shares
  diag(diversion) <- -shares
  diversion <- as.vector(diversion)
  upstream <- (object@ownerUpLambdaPost[active, active] * diversion) %*%
    (price_up - costs$up[active]) -
    (object@ownerDownLambdaPost[active, active] * diversion) %*%
    margins_down
  downstream <- price_down - price_up - costs$down[active] - margins_down
  c(downstream, as.vector(upstream))
}

assert_subset_outputs <- function(object, inactive = !object@down@subset,
                                  second_score = FALSE) {
  active <- object@down@subset
  expect_length(object@up@pricePre, length(active))
  expect_length(object@down@pricePre, length(active))
  expect_length(object@up@pricePost, length(active))
  expect_length(object@down@pricePost, length(active))
  expect_true(all(is.finite(object@up@pricePre)))
  expect_true(all(is.finite(object@down@pricePre)))
  expect_true(all(is.finite(object@up@pricePost[active])))
  expect_true(all(is.finite(object@down@pricePost[active])))
  expect_true(all(is.na(object@up@pricePost[inactive])))
  expect_true(all(is.na(object@down@pricePost[inactive])))

  pre_shares <- calcShares(object, preMerger = TRUE)
  post_shares <- calcShares(object, preMerger = FALSE)
  expect_true(all(is.finite(pre_shares)))
  if (second_score) {
    expect_true(all(is.finite(post_shares[active])))
    expect_true(all(is.na(post_shares[inactive])))
  } else {
    expect_true(all(is.finite(post_shares[active])))
    expect_equal(unname(post_shares[inactive]), rep(0, sum(inactive)))
  }

  pre_margins <- calcMargins(object, preMerger = TRUE, level = TRUE)
  post_margins <- calcMargins(object, preMerger = FALSE, level = TRUE)
  expect_true(all(is.finite(pre_margins$up)))
  expect_true(all(is.finite(pre_margins$down)))
  expect_true(all(is.finite(post_margins$up[active])))
  expect_true(all(is.finite(post_margins$down[active])))
  expect_true(all(is.na(post_margins$up[inactive])))
  expect_true(all(is.na(post_margins$down[inactive])))

  costs <- calcMC(object, preMerger = FALSE)
  expect_length(costs$up, length(active))
  expect_length(costs$down, length(active))
  expect_true(all(is.finite(costs$up)))
  expect_true(all(is.finite(costs$down)))
  expect_equal(dim(object@ownerUpLambdaPost), rep(length(active), 2L))
  expect_equal(dim(object@ownerDownLambdaPost), rep(length(active), 2L))

  quantities <- calcQuantities(object, preMerger = FALSE)
  revenues <- calcRevenues(object, preMerger = FALSE)
  expect_length(quantities, length(active))
  expect_length(revenues, length(active))
  expect_true(all(is.finite(quantities[active])))
  expect_true(all(is.finite(revenues[active])))
  if (second_score) {
    expect_true(all(is.na(quantities[inactive])))
    expect_true(all(is.na(revenues[inactive])))
  } else {
    expect_equal(unname(quantities[inactive]), rep(0, sum(inactive)))
    expect_true(all(is.na(revenues[inactive])))
  }
  invisible(NULL)
}

test_that("partial Bertrand masks work in direct and fitted lifecycles", {
  for (chain_level in c("full", "wholesaler", "retailer")) {
    args <- subset_direct_args(chain_level = chain_level)
    direct <- suppressWarnings(do.call(vertical::vertical.barg, args))
    fit <- suppressWarnings(subset_fit(chain_level = chain_level))
    simulated <- suppressWarnings(simulate(
      fit, ownerPost = subset_owner(), subset = subset_mask
    ))

    expect_s4_class(direct, "VertBargBertLogit")
    expect_s4_class(simulated, "VertBargBertLogit")
    expect_identical(direct@down@subset, subset_mask)
    expect_identical(simulated@down@subset, subset_mask)
    expect_equal(direct@up@pricePost[subset_mask],
                 simulated@up@pricePost[subset_mask], tolerance = 1e-7)
    expect_equal(direct@down@pricePost[subset_mask],
                 simulated@down@pricePost[subset_mask], tolerance = 1e-7)
    assert_subset_outputs(direct)
    assert_subset_outputs(simulated)
    expect_lt(max(abs(subset_foc(simulated, chain_level))), 1e-7)
  }
})

test_that("nested Bertrand masks work at each chain level", {
  for (chain_level in c("full", "wholesaler", "retailer")) {
    args <- subset_direct_args(
      demand = "logit_nests", chain_level = chain_level
    )
    direct <- suppressWarnings(do.call(vertical::vertical.barg, args))
    fit <- suppressWarnings(subset_fit(
      demand = "logit_nests", chain_level = chain_level
    ))
    simulated <- suppressWarnings(simulate(
      fit, ownerPost = subset_owner(), subset = subset_mask
    ))

    expect_s4_class(direct, "VertBargBertLogitNests")
    expect_s4_class(simulated, "VertBargBertLogitNests")
    expect_equal(direct@down@pricePost[subset_mask],
                 simulated@down@pricePost[subset_mask], tolerance = 1e-7)
    assert_subset_outputs(direct)
    assert_subset_outputs(simulated)
    expect_lt(max(abs(subset_foc(simulated, chain_level))), 1e-7)
  }
})

test_that("nested Bertrand masks handle a completely inactive nest", {
  fit <- suppressWarnings(subset_fit(demand = "logit_nests"))
  simulated <- suppressWarnings(simulate(
    fit, ownerPost = subset_owner(), subset = subset_nest_mask
  ))

  expect_true(all(is.finite(calcShares(simulated, TRUE))))
  expect_equal(unname(calcShares(simulated, FALSE)[!subset_nest_mask]),
               rep(0, sum(!subset_nest_mask)))
  assert_subset_outputs(simulated, inactive = !subset_nest_mask)
  expect_lt(max(abs(subset_foc(simulated))), 1e-7)
})

test_that("second-score masks preserve active FOCs and NA inactive outputs", {
  for (chain_level in c("full", "wholesaler", "retailer")) {
    args <- subset_direct_args(variant = "auction2nd", chain_level = chain_level)
    direct <- suppressWarnings(do.call(vertical::vertical.barg, args))
    fit <- suppressWarnings(subset_fit(
      variant = "auction2nd", chain_level = chain_level
    ))
    simulated <- suppressWarnings(simulate(
      fit, ownerPost = subset_owner(), subset = subset_mask
    ))

    expect_s4_class(direct, "VertBarg2ndLogit")
    expect_s4_class(simulated, "VertBarg2ndLogit")
    expect_equal(direct@up@pricePost[subset_mask],
                 simulated@up@pricePost[subset_mask], tolerance = 1e-7)
    expect_equal(direct@down@pricePost[subset_mask],
                 simulated@down@pricePost[subset_mask], tolerance = 1e-7)
    assert_subset_outputs(direct, second_score = TRUE)
    assert_subset_outputs(simulated, second_score = TRUE)
    if (identical(chain_level, "full")) {
      expect_lt(max(abs(subset_second_score_foc(simulated))), 1e-7)
    }
  }
})

test_that("subset masks are strict and survive serialization", {
  fit <- suppressWarnings(subset_fit())
  owner <- subset_owner()
  expect_error(
    simulate(fit, ownerPost = owner, subset = as.numeric(subset_mask)),
    "subset.*logical"
  )
  expect_error(
    simulate(fit, ownerPost = owner, subset = rep(FALSE, 4L)),
    "at least one TRUE"
  )
  expect_error(
    simulate(fit, ownerPost = owner,
             subset = c(TRUE, NA, FALSE, TRUE)),
    "at least one TRUE"
  )
  expect_error(
    simulate(fit, ownerPost = owner, subset = c(TRUE, FALSE)),
    "same length"
  )

  result <- suppressWarnings(simulate(
    fit, ownerPost = owner, subset = subset_mask
  ))
  restored <- unserialize(serialize(result, NULL))
  expect_s4_class(restored, "VertBargBertLogit")
  expect_identical(restored@down@subset, subset_mask)
  expect_equal(restored@up@pricePost, result@up@pricePost)
  expect_equal(restored@down@pricePost, result@down@pricePost)
})

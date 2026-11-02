vertical_manual_hhi <- function(object, preMerger, tier,
                                revenue = FALSE) {
  shares <- calcShares(object, preMerger = preMerger, revenue = revenue)
  shares <- shares / sum(shares, na.rm = TRUE)
  owners <- switch(
    tier,
    upstream = if (preMerger) object@up@ownerPre else object@up@ownerPost,
    downstream = if (preMerger) object@down@ownerPre else object@down@ownerPost
  )
  as.vector(HHI(shares, owner = owners))
}

vertical_price_residual <- function(object, preMerger = FALSE) {
  costs <- calcMC(object, preMerger = preMerger)
  margins <- calcMargins(object, preMerger = preMerger, level = TRUE)
  price_up <- if (preMerger) object@up@pricePre else object@up@pricePost
  price_down <- if (preMerger) object@down@pricePre else object@down@pricePost
  c(
    upstream = price_up - costs$up - margins$up,
    downstream = price_down - price_up - costs$down - margins$down
  )
}

vertical_second_score_residual <- function(object) {
  up <- object@up
  down <- object@down
  subset <- down@subset
  price_up <- up@pricePost[subset]
  price_down <- down@pricePost[subset]
  costs <- calcMC(object, preMerger = FALSE)

  down@ownerPost <- ownerToMatrix(down, preMerger = FALSE)[subset, subset]
  down@slopes$meanval <- down@slopes$meanval[subset] +
    down@slopes$alpha * (price_up - down@priceOutside)
  shares <- calcShares(down, preMerger = FALSE, revenue = FALSE)
  margins_down <- calcMargins(down, preMerger = FALSE, level = TRUE)
  diversion <- tcrossprod(1 / (1 - shares), shares) * shares
  diag(diversion) <- -shares
  diversion <- as.vector(diversion)

  owner_up_lambda <- object@ownerUpLambdaPost[subset, subset]
  owner_down_lambda <- object@ownerDownLambdaPost[subset, subset]
  upstream <- (owner_up_lambda * diversion) %*%
    (price_up - costs$up[subset]) -
    (owner_down_lambda * diversion) %*% margins_down
  downstream <- price_down - price_up - costs$down[subset] - margins_down
  c(downstream, as.vector(upstream))
}

test_that("hybrid classification uses ownership relationships", {
  upstream <- vertical_make_case("hybrid_upstream")
  expect_identical(merger_type(upstream), "hybrid")
  expect_true(upstream@isHorizontal)
  expect_true(upstream@isUpstream)

  downstream_args <- vertical_case_args("flat_vertical")
  downstream_args$ownerPostDown <- rep("U1", 4L)
  downstream <- do.call(vertical::vertical.barg, downstream_args)
  expect_identical(merger_type(downstream), "hybrid")
  expect_true(downstream@isHorizontal)
  expect_false(downstream@isUpstream)

  both_args <- vertical_case_args("hybrid_upstream")
  both_args$ownerPostDown <- rep("U1", 4L)
  both <- do.call(vertical::vertical.barg, both_args)
  expect_identical(merger_type(both), "hybrid")
  expect_true(both@isHorizontal)
  expect_true(both@isUpstream)

  both_horizontal_args <- vertical_case_args("hybrid_upstream")
  both_horizontal_args$ownerPostDown <- rep("D1", 4L)
  both_horizontal <- do.call(vertical::vertical.barg, both_horizontal_args)
  expect_identical(merger_type(both_horizontal), "horizontal")
  expect_length(hhi(both_horizontal, preMerger = FALSE), 2L)

  expect_identical(merger_type(vertical_make_case("flat_upstream_horizontal")),
                   "horizontal")
  expect_identical(merger_type(vertical_make_case("flat_downstream_horizontal")),
                   "horizontal")
  expect_identical(merger_type(vertical_make_case("flat_vertical")), "vertical")
  expect_identical(merger_type(vertical_calibrate()), "none")
})

test_that("classification handles restructurings and non-aligned integration", {
  model <- vertical_calibrate()@model
  model@up@ownerPre <- model@up@ownerPost <- c("U1", "U2", "U3", "U4")
  model@down@ownerPre <- c("D1", "D2", "D3", "D4")
  model@down@ownerPost <- c("D1", "U1", "D3", "D4")
  expect_identical(merger_type(model), "vertical")

  model@down@ownerPost <- c("U1", "D2", "D3", "D4")
  expect_identical(merger_type(model), "vertical")

  model@down@ownerPost <- c("X1", "X2", "X3", "X4")
  expect_identical(merger_type(model), "none")

  model@down@ownerPre <- c("U1", "D2", "D3", "D4")
  model@down@ownerPost <- c("D1", "D2", "D3", "D4")
  expect_identical(merger_type(model), "restructuring")

  model@up@ownerPre <- model@up@ownerPost <- c("U1", "U2", "U3", "U3")
  model@down@ownerPre <- c("U1", "D1", "D2", "D2")
  model@down@ownerPost <- c("U1", "D1", "D1", "D1")
  expect_identical(merger_type(model), "horizontal")
})

test_that("hybrid Bertrand economics match the pre-extraction implementation", {
  expect_no_error(vertical_expect_fixture(
    "hybrid_upstream", compare_flags = FALSE, compare_hhi = FALSE,
    compare_summary = FALSE
  ))
  fit <- vertical_make_case("hybrid_upstream")
  expect_lt(max(abs(vertical_price_residual(fit))), 1e-7)
})

test_that("hybrid concentration is reported separately by tier", {
  fit <- vertical_make_case("hybrid_upstream")
  for (pre in c(TRUE, FALSE)) {
    expected <- c(
      upstream = vertical_manual_hhi(fit, pre, "upstream"),
      downstream = vertical_manual_hhi(fit, pre, "downstream")
    )
    expect_equal(hhi(fit, preMerger = pre), expected, tolerance = 1e-8)
    expect_equal(hhi(fit, preMerger = pre, tier = "upstream"),
                 unname(expected[["upstream"]]), tolerance = 1e-8)
    expect_equal(hhi(fit, preMerger = pre, tier = "downstream"),
                 unname(expected[["downstream"]]), tolerance = 1e-8)
  }

  printed <- capture.output(market <- summary(fit, market = TRUE))
  expect_true(length(printed) > 0L)
  expect_true(all(c("Upstream HHI Change", "Downstream HHI Change") %in%
                    names(market)))
  expect_false("HHI Change" %in% names(market))
})

test_that("hybrid simulation and sequential promotion retain transition types", {
  fit <- vertical_calibrate()
  x <- vertical_inputs()
  owner_post <- list(
    up = rep("U1", 4L),
    down = c("U1", "U1", "D2", "D2")
  )
  direct <- vertical_make_case("hybrid_upstream")
  simulated <- simulate(fit, ownerPost = owner_post)
  expect_identical(merger_type(simulated), "hybrid")
  vertical_expect_equal(calcPrices(simulated, FALSE), calcPrices(direct, FALSE))

  cf <- add_step(
    counterfactual(ownership = owner_post),
    costs = list(up = rep(-0.01, 4L), down = rep(-0.01, 4L))
  )
  path <- simulate(fit, cf)
  expect_identical(unname(merger_type(path)), c("hybrid", "none"))

  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file), add = TRUE)
  saveRDS(simulated, file)
  expect_identical(merger_type(readRDS(file)), "hybrid")
})

test_that("all registered vertical variants solve hybrid ownership", {
  nested_args <- vertical_case_args("hybrid_upstream")
  nested_args$nests <- c("n1", "n1", "n2", "n2")
  nested <- do.call(vertical::vertical.barg, nested_args)
  expect_identical(merger_type(nested), "hybrid")
  expect_true(all(is.finite(c(nested@up@pricePost,
                              nested@down@pricePost))))
  expect_lt(max(abs(vertical_price_residual(nested))), 1e-7)

  second_args <- vertical_case_args("hybrid_upstream")
  second_args$supplyDown <- "2nd"
  warnings <- character()
  second <- withCallingHandlers(
    do.call(vertical::vertical.barg, second_args),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_false(any(grepl("solver|converge", warnings, ignore.case = TRUE)))
  expect_identical(merger_type(second), "hybrid")
  expect_equal(
    unname(second@up@pricePost),
    c(30.51241463, 33.88537865, 99.34251962, 102.71548363),
    tolerance = 1e-6
  )
  expect_equal(
    unname(second@down@pricePost),
    c(66.67912191, 62.47280592, 127.23382012, 123.02750413),
    tolerance = 1e-6
  )
  expect_lt(max(abs(vertical_second_score_residual(second))), 1e-7)

  for (chain_level in c("full", "wholesaler", "retailer")) {
    args <- vertical_case_args("hybrid_upstream")
    args$chain_level <- chain_level
    result <- do.call(vertical::vertical.barg, args)
    expect_identical(merger_type(result), "hybrid")
    expect_true(all(is.finite(c(result@up@pricePost,
                                result@down@pricePost))))
  }
})

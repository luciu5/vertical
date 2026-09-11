vertical_fixture <- local({
  path <- testthat::test_path("fixtures", "pre-extraction-vertical.rds")
  readRDS(path)
})

vertical_inputs <- function() vertical_fixture$metadata$input

vertical_case_args <- function(case) {
  x <- vertical_inputs()
  args <- list(
    sharesDown = x$shares_down,
    pricesDown = x$prices_down,
    marginsDown = x$margins_down,
    ownerPreDown = x$owner_pre_down,
    ownerPostDown = x$owner_pre_down,
    pricesUp = x$prices_up,
    marginsUp = x$margins_up,
    ownerPreUp = x$owner_pre_up,
    ownerPostUp = x$owner_pre_up,
    insideSize = x$inside_size,
    priceOutside = x$price_outside,
    labels = x$labels
  )
  if (identical(case, "flat_upstream_horizontal")) {
    args$ownerPostUp <- rep("U1", 4L)
    args$mcDeltaUp <- rep(-.01, 4L)
    args$mcDeltaDown <- rep(-.01, 4L)
  } else if (identical(case, "flat_vertical")) {
    args$ownerPostDown <- c("U1", "U1", "D2", "D2")
  } else if (identical(case, "flat_downstream_horizontal")) {
    args$ownerPostDown <- rep("D1", 4L)
  } else if (identical(case, "second_score")) {
    args$supplyDown <- "2nd"
    args$ownerPostUp <- rep("U1", 4L)
  } else if (identical(case, "nested_bertrand")) {
    args$ownerPostUp <- rep("U1", 4L)
    args$nests <- c("n1", "n1", "n2", "n2")
  } else if (identical(case, "wholesaler")) {
    args$ownerPostUp <- rep("U1", 4L)
    args$chain_level <- "wholesaler"
  } else if (identical(case, "retailer")) {
    args$ownerPostUp <- rep("U1", 4L)
    args$chain_level <- "retailer"
  } else {
    stop("unknown vertical fixture case: ", case)
  }
  args
}

vertical_make_case <- function(case) {
  do.call(vertical::vertical.barg, vertical_case_args(case))
}

vertical_calibrate <- function(case = "flat_vertical") {
  x <- vertical_inputs()
  args <- list(
    demand = if (identical(case, "nested_bertrand")) "logit_nests" else "logit",
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
    labels = x$labels
  )
  if (identical(case, "nested_bertrand")) {
    args$nests <- c("n1", "n1", "n2", "n2")
  }
  do.call(vertical::calibrate, args)
}

vertical_expect_equal <- function(actual, expected, tolerance = 1e-7,
                                  info = NULL) {
  strip_names <- function(x) {
    if (is.list(x)) return(lapply(x, strip_names))
    if (is.numeric(x)) return(unname(x))
    x
  }
  testthat::expect_equal(strip_names(actual), strip_names(expected),
                         tolerance = tolerance, info = info)
}

vertical_expect_fixture <- function(case) {
  expected <- vertical_fixture[[case]]
  fit <- vertical_make_case(case)

  testthat::expect_equal(class(fit)[[1L]], expected$class)
  testthat::expect_equal(fit@supplyDown, expected$flags$supplyDown)
  testthat::expect_equal(fit@isHorizontal, expected$flags$isHorizontal)
  testthat::expect_equal(fit@isUpstream, expected$flags$isUpstream)
  testthat::expect_equal(fit@constrain, expected$flags$constrain)
  testthat::expect_equal(fit@chain_level, expected$flags$chain_level)

  vertical_expect_equal(fit@down@slopes, expected$parameters$down,
                        info = paste(case, "demand parameters"))
  vertical_expect_equal(fit@up@bargpowerPre,
                        expected$parameters$bargpowerPre,
                        info = paste(case, "pre bargaining power"))
  vertical_expect_equal(fit@up@bargpowerPost,
                        expected$parameters$bargpowerPost,
                        info = paste(case, "post bargaining power"))
  for (pre in c(TRUE, FALSE)) {
    suffix <- if (pre) "pre" else "post"
    vertical_expect_equal(calcMC(fit, pre), expected$costs[[suffix]],
                          info = paste(case, suffix, "costs"))
    vertical_expect_equal(calcPrices(fit, pre), expected$prices[[suffix]],
                          info = paste(case, suffix, "prices"))
    vertical_expect_equal(calcMargins(fit, pre, level = TRUE),
                          expected$margins[[suffix]],
                          info = paste(case, suffix, "margins"))
    vertical_expect_equal(calcShares(fit, pre), expected$shares[[suffix]],
                          info = paste(case, suffix, "shares"))
    vertical_expect_equal(calcQuantities(fit, pre),
                          expected$quantities[[suffix]],
                          info = paste(case, suffix, "quantities"))
    vertical_expect_equal(calcRevenues(fit, pre),
                          expected$revenues[[suffix]],
                          info = paste(case, suffix, "revenues"))
  }
  vertical_expect_equal(hhi(fit, TRUE), expected$hhi$pre,
                        info = paste(case, "pre HHI"))
  vertical_expect_equal(hhi(fit, FALSE), expected$hhi$post,
                        info = paste(case, "post HHI"))
  vertical_expect_equal(CV(fit), expected$cv, info = paste(case, "CV"))
  vertical_expect_equal(calcPriceDelta(fit, levels = TRUE),
                        expected$price_delta,
                        info = paste(case, "price delta"))
  vertical_expect_equal(calcProducerSurplus(fit, TRUE),
                        expected$producer_surplus$pre,
                        info = paste(case, "pre producer surplus"))
  vertical_expect_equal(calcProducerSurplus(fit, FALSE),
                        expected$producer_surplus$post,
                        info = paste(case, "post producer surplus"))
  invisible(fit)
}

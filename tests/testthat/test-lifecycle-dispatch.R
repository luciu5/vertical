test_that("vertical lifecycle dispatch is explicit", {
  fit <- vertical_calibrate()
  x <- vertical_inputs()
  owner_post <- list(up = rep("U1", 4L), down = x$owner_pre_down)

  testthat::expect_s4_class(fit, "VerticalFit")
  testthat::expect_s4_class(fit@model, "VertBargBertLogit")
  testthat::expect_identical(
    validate_counterfactual(fit, counterfactual(ownership = owner_post)),
    counterfactual(ownership = owner_post)
  )
  testthat::expect_error(
    validate_counterfactual(
      fit,
      counterfactual(quality = stats::setNames(rep(1, 4L), x$labels))
    ),
    "does not support"
  )
  testthat::expect_error(
    validate_counterfactual(fit, counterfactual(capacity = rep(1, 4L))),
    "does not support"
  )
  testthat::expect_error(
    validate_counterfactual(fit, counterfactual(bargaining = rep(.5, 4L))),
    "does not support"
  )

  result <- simulate(fit, ownerPost = owner_post)
  testthat::expect_s4_class(result, "VertBargBertLogit")
  testthat::expect_true(all(is.finite(result@down@pricePost)))

  updated <- stats::update(fit, prices = x$prices_down)
  testthat::expect_s4_class(updated, "VerticalFit")
  testthat::expect_error(stats::update(fit, demand = "logit_nests"),
                         "same demand, conduct, and variant")
  testthat::expect_error(stats::update(fit, x$prices_down), "must be named")
  testthat::expect_error(
    specify("logit", "vertical_bargaining", prices = x$prices_down,
            parameters = list(), ownerPre = x$owner_pre_down),
    "not supported"
  )
  testthat::expect_error(respecify(fit), "no respecify\\(\\) method")
  testthat::expect_error(
    simulate(fit, ownerPost = owner_post, capacitiesPost = rep(1, 4L)),
    "not supported"
  )
  testthat::expect_error(
    simulate(fit, ownerPost = owner_post, bargpowerPost = rep(.5, 4L)),
    "not supported"
  )
})

test_that("vertical sequential simulation and resume preserve baseline fixtures", {
  fit <- vertical_calibrate()
  x <- vertical_inputs()
  cf <- counterfactual(ownership = list(
    up = rep("U1", 4L), down = x$owner_pre_down
  ))
  cf <- add_step(cf, costs = list(up = rep(-.01, 4L),
                                  down = rep(-.01, 4L)))
  path <- simulate(fit, cf)
  expected <- vertical_fixture$sequential

  testthat::expect_s4_class(path, "CounterfactualPath")
  testthat::expect_length(path@results, 2L)
  testthat::expect_equal(
    vapply(path@results, function(x) class(x)[[1L]], character(1)),
    expected$classes
  )
  for (i in seq_len(2L)) {
    actual <- path@results[[i]]
    baseline <- expected[[paste0("step", i)]]
    vertical_expect_equal(calcPrices(actual, TRUE), baseline$prices$pre,
                          info = paste("sequential step", i, "pre prices"))
    vertical_expect_equal(calcPrices(actual, FALSE), baseline$prices$post,
                          info = paste("sequential step", i, "post prices"))
    vertical_expect_equal(calcMC(actual, FALSE), baseline$costs$post,
                          info = paste("sequential step", i, "post costs"))
  }
  vertical_expect_equal(path@diagnostics$cumulative_costs,
                        expected$cumulative_costs,
                        info = "sequential cumulative costs")

  resumed <- simulate(path, counterfactual(costs = list(
    up = rep(0, 4L), down = rep(0, 4L)
  )))
  testthat::expect_s4_class(resumed, "CounterfactualPath")
  testthat::expect_length(resumed@results, 3L)
})

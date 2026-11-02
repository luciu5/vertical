test_that("vertical Logit welfare obeys demand and transfer accounting", {
  fit <- suppressWarnings(vertical_make_case("flat_vertical"))
  down <- fit@down
  alpha <- down@slopes$alpha
  mean_utility <- down@slopes$meanval
  market_size <- down@mktSize

  # One outside option and the estimated utility index give quantities
  # independently of vertical's share and producer-surplus methods.
  quantity_at <- function(price) {
    utility <- mean_utility + alpha * (price - down@priceOutside)
    market_size * exp(utility) / (1 + sum(exp(utility)))
  }
  pre_quantity <- quantity_at(down@pricePre)
  post_quantity <- quantity_at(down@pricePost)
  expect_equal(unname(calcQuantities(fit, TRUE)), unname(pre_quantity),
               tolerance = 1e-8)
  expect_equal(unname(calcQuantities(fit, FALSE)), unname(post_quantity),
               tolerance = 1e-8)

  for (pre in c(TRUE, FALSE)) {
    q <- if (pre) pre_quantity else post_quantity
    retail_price <- if (pre) down@pricePre else down@pricePost
    wholesale_price <- if (pre) fit@up@pricePre else fit@up@pricePost
    upstream_cost <- if (pre) fit@up@mcPre else fit@up@mcPost
    downstream_cost <- if (pre) down@mcPre else down@mcPost
    profit <- calcProducerSurplus(fit, preMerger = pre)

    expect_equal(unname(profit$up),
                 unname((wholesale_price - upstream_cost) * q),
                 tolerance = 1e-6)
    expect_equal(unname(profit$down),
                 unname((retail_price - wholesale_price - downstream_cost) * q),
                 tolerance = 1e-6)
    # Wholesale payments cancel from aggregate producer surplus, including
    # the product whose upstream and downstream owners become integrated.
    expect_equal(unname(profit$up + profit$down),
                 unname((retail_price - upstream_cost - downstream_cost) * q),
                 tolerance = 1e-6)
  }

  inclusive_value <- function(price) {
    log1p(sum(exp(mean_utility + alpha *
                  (price - down@priceOutside))))
  }
  expected_cv <- market_size *
    (inclusive_value(down@pricePost) - inclusive_value(down@pricePre)) /
    alpha
  expect_equal(unname(CV(fit)), unname(expected_cv), tolerance = 1e-8)
})

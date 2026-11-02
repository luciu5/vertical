# Independent utility and profit calculations for the policy economic oracles.
# These deliberately avoid vertical's policy demand, margins and FOC helpers.
policy_quantities <- function(model, retail = model@down@pricePost,
                              wholesale = model@up@pricePost,
                              removed = integer(), preMerger = FALSE) {
  down <- model@down
  price <- if (model@supplyDown == "2nd") wholesale else retail
  utility <- down@slopes$meanval + down@slopes$alpha *
    (price - down@priceOutside)
  if (model@supplyDown == "2nd" && !preMerger) {
    # Legacy auction post-state normalization applies its additive cost change
    # and outside-price normalization after the vertical transfer adjustment.
    utility <- utility + down@slopes$alpha * (down@mcDelta - down@priceOutside)
  }
  keep <- setdiff(which(is.finite(price)), removed)
  quantity <- numeric(length(price))
  if (methods::is(down, "LogitNests")) {
    nest <- as.character(down@nests[keep])
    scale <- down@slopes$sigma[nest]
    value <- exp(utility[keep] / scale)
    mass <- tapply(value, nest, sum)
    inclusive <- mass^down@slopes$sigma[names(mass)]
    quantity[keep] <- value * mass[nest]^(scale - 1) /
      (1 + sum(inclusive))
  } else {
    value <- exp(utility[keep])
    quantity[keep] <- value / (1 + sum(value))
  }
  size <- if (is.na(down@mktSize)) 1 else down@mktSize
  quantity * size
}

policy_expect_stationarity <- function(model, tariff = list(up = 0, down = 0),
                                       shadow = NULL, tolerance = 2e-6) {
  n <- length(model@down@prices)
  expand <- function(x) rep(x, length.out = n)
  ru <- 1 - expand(tariff$up)
  rd <- 1 - expand(tariff$down)
  w <- model@up@pricePost
  p <- model@down@pricePost
  cu <- model@up@mcPost
  cd <- model@down@mcPost
  uowner <- model@up@ownerPost
  downer <- model@down@ownerPost
  if (is.null(shadow)) shadow <- list(up = numeric(n), down = numeric(n))
  q <- policy_quantities(model)
  mu <- ru * w - cu - shadow$up
  md <- rd * p - w - cd - shadow$down
  derivative <- function(f, x) {
    h <- max(abs(x), 1) * 1e-5
    unname((f(x + h) - f(x - h)) / (2 * h))
  }
  for (j in seq_len(n)) {
    if (uowner[j] == downer[j]) {
      # Source convention: internal transfers have zero net operating margin,
      # with scarce upstream capacity valued at its opportunity cost.
      expect_equal(unname(mu[j]), 0, tolerance = tolerance)
    } else {
      lost <- q - policy_quantities(model, removed = j)
      # The source Nash-in-Nash problem holds downstream prices fixed during
      # a bilateral transfer negotiation. Compute each party's entire owned
      # portfolio gain over product removal, including cross-tier ownership.
      log_nash <- function(transfer) {
        this_mu <- mu
        this_md <- md
        this_mu[j] <- ru[j] * transfer - cu[j] - shadow$up[j]
        this_md[j] <- rd[j] * p[j] - transfer - cd[j] - shadow$down[j]
        seller <- sum(this_mu[uowner == uowner[j]] * lost[uowner == uowner[j]]) +
          sum(this_md[downer == uowner[j]] * lost[downer == uowner[j]])
        buyer <- sum(this_mu[uowner == downer[j]] * lost[uowner == downer[j]]) +
          sum(this_md[downer == downer[j]] * lost[downer == downer[j]])
        if (seller <= 0 || buyer <= 0) return(NA_real_)
        power <- model@up@bargpowerPost[j]
        (1 - power) * log(seller) + power * log(buyer)
      }
      expect_true(is.finite(log_nash(w[j])), info = paste("positive Nash gains", j))
      expect_equal(derivative(log_nash, w[j]), 0, tolerance = tolerance,
        info = paste("bilateral net-profit Nash derivative", j))
    }
    if (model@supplyDown != "2nd") {
      retailer_profit <- function(price) {
        candidate <- p
        candidate[j] <- price
        quantity <- policy_quantities(model, retail = candidate)
        margins <- rd * candidate - w - cd - shadow$down
        sum(margins[downer == downer[j]] * quantity[downer == downer[j]]) +
          sum(mu[uowner == downer[j]] * quantity[uowner == downer[j]])
      }
      expect_equal(derivative(retailer_profit, p[j]) / sum(q), 0,
        tolerance = tolerance, info = paste("retailer portfolio profit derivative", j))
    }
  }
  invisible(q)
}

policy_expect_kkt <- function(model, quota, original_quantity,
                              tariff = list(up = 0, down = 0)) {
  diagnostics <- vertical_policy_diagnostics(model)
  q <- policy_quantities(model)
  shadow <- lapply(diagnostics[c("up", "down")], `[[`, "shadow_value")
  policy_expect_stationarity(model, tariff, shadow)
  for (tier in c("up", "down")) {
    cap <- ifelse(is.infinite(quota[[tier]]), Inf,
      quota[[tier]] * original_quantity)
    finite <- is.finite(cap)
    expect_equal(unname(diagnostics[[tier]]$capacity), unname(cap), tolerance = 1e-8)
    expect_equal(unname(diagnostics[[tier]]$quantity), unname(q), tolerance = 1e-8)
    expect_true(all(q[finite] <= cap[finite] + 1e-7))
    expect_true(all(shadow[[tier]] >= -1e-9))
    expect_equal(shadow[[tier]][!finite], rep(0, sum(!finite)))
    expect_equal(shadow[[tier]][finite] * (cap[finite] - q[finite]),
      rep(0, sum(finite)), tolerance = 1e-6)
  }
  invisible(diagnostics)
}

# Vertical output methods.

# Demand-only view of a promoted pre-market; local copies retain the fitted
# utility index and use existing antitrust derivatives on survivor products.
.vertical_reduced_down <- function(down, active) {
  n <- length(active)
  normalizer <- down@normIndex
  for (field in methods::slotNames(down)) {
    value <- methods::slot(down, field)
    if (is.matrix(value) && identical(dim(value), c(n, n))) {
      methods::slot(down, field) <- value[active, active, drop = FALSE]
    } else if (is.atomic(value) && length(value) == n) {
      value <- value[active]
      if (is.factor(value)) value <- droplevels(value)
      methods::slot(down, field) <- value
    }
  }
  if (length(normalizer) == 1L && !is.na(normalizer)) {
    position <- match(normalizer, which(active))
    down@normIndex <- if (is.na(position)) 1 else position
  }
  down@slopes$meanval <- down@slopes$meanval[active]
  if (methods::is(down, "LogitNests")) {
    down@slopes$sigma <- down@slopes$sigma[levels(down@nests)]
  }
  down
}

.vertical_pre_derivative <- function(object, method, market = FALSE, revenue = TRUE) {
  active <- .vertical_active_subset(object, TRUE)
  down <- .vertical_reduced_down(object@down, active)
  if (methods::is(down, "LogitNests") && !market) {
    # The inner implementation's diag(scalar) is ambiguous after a whole
    # nest exits. Write the same nested derivative with explicit dimensions.
    share <- calcShares(down, TRUE)
    nest <- as.character(down@nests)
    scale <- as.numeric(down@slopes$sigma[nest])
    nest_share <- as.numeric(tapply(share, nest, sum)[nest])
    same <- outer(nest, nest, "==")
    elasticity <- -down@slopes$alpha *
      matrix(share * down@pricePre, length(share), length(share), byrow = TRUE) *
      (1 + same * ((1 - scale) / scale / nest_share))
    diag(elasticity) <- diag(elasticity) + down@slopes$alpha * down@pricePre / scale
    if (method == "elast") {
      value <- elasticity
    } else {
      if (revenue) {
        value <- t(elasticity) / (diag(elasticity) - 1)
        value <- -value / diag(value)
      } else value <- -t(elasticity) / diag(elasticity)
      output <- calcShares(down, TRUE, revenue = revenue)
      value <- value * tcrossprod(1 / output, output)
    }
  } else {
    value <- if (method == "elast") elast(down, TRUE, market = market) else
      diversion(down, TRUE, revenue = revenue)
  }
  if (market) return(value)
  result <- matrix(NA_real_, length(active), length(active),
    dimnames = list(object@down@labels, object@down@labels))
  result[active, active] <- value
  result
}


#' @rdname vertical-methods
#' @export
setMethod("calcRevenues", "VertBargBertLogit", function(object, preMerger = TRUE, market = FALSE) {
  if (!is.null(.vertical_policy_state(object)) ||
      any(!.vertical_active_subset(object, preMerger))) {
    prices <- if (preMerger) object@down@pricePre else object@down@pricePost
    revenue <- prices * calcQuantities(object, preMerger)
    if (market) return(sum(revenue, na.rm = TRUE))
    return(revenue)
  }
  calcRevenues(object@down, preMerger = preMerger, market = market)
})

#' @rdname vertical-methods
#' @export
setMethod("calcQuantities", "VertBargBertLogit", function(object, preMerger = TRUE, market = FALSE) {
  if (!is.null(.vertical_policy_state(object)) ||
      any(!.vertical_active_subset(object, preMerger))) {
    size <- object@down@mktSize
    if (is.na(size)) size <- 1
    quantity <- calcShares(object, preMerger) * size
    if (market) return(sum(quantity, na.rm = TRUE))
    return(quantity)
  }
  calcQuantities(object@down, preMerger = preMerger, market = market)
})

#' @rdname vertical-methods
#' @export
setMethod("calcShares", "VertBargBertLogit", function(object, preMerger = TRUE, revenue = FALSE) {
  down <- object@down
  active <- .vertical_active_subset(object, preMerger)
  if (preMerger && any(!active)) {
    down@slopes$meanval[!active] <- -Inf
    down@pricePre[!active] <- 0
  }
  shares <- calcShares(down, preMerger = preMerger, revenue = revenue)
  shares[!active] <- 0
  shares
})

#' @rdname vertical-methods
#' @export
setMethod("calcShares", "VertBarg2ndLogit", function(object, preMerger = TRUE, revenue = FALSE) {
  down <- object@down
  up <- object@up
  priceOutside <- down@priceOutside
  upPrice <- if (preMerger) up@pricePre else up@pricePost
  active <- .vertical_active_subset(object, preMerger)
  if (preMerger && any(!active)) {
    down@slopes$meanval[!active] <- -Inf
    down@pricePre[!active] <- 0
    upPrice[!active] <- 0
  }
  meanval <- down@slopes$meanval
  alpha <- down@slopes$alpha
  down@slopes$meanval <- meanval + alpha * (upPrice - priceOutside)
  shares <- calcShares(down, preMerger = preMerger, revenue = revenue)
  shares[!active] <- NA_real_
  shares
})

#' @rdname vertical-methods
#' @export
setMethod("elast", "VertBargBertLogit", function(object, preMerger = TRUE, market = FALSE) {
  if (preMerger && any(!.vertical_active_subset(object, TRUE)))
    return(.vertical_pre_derivative(object, "elast", market = market))
  elast(object@down, preMerger = preMerger, market = market)
})

#' @rdname vertical-methods
#' @export
setMethod("diversion", "VertBargBertLogit", function(object, preMerger = TRUE, revenue = TRUE) {
  if (preMerger && any(!.vertical_active_subset(object, TRUE)))
    return(.vertical_pre_derivative(object, "diversion", revenue = revenue))
  diversion(object@down, preMerger = preMerger, revenue = revenue)
})

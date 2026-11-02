# Vertical output methods.

#' @rdname vertical-methods
#' @export
setMethod("calcRevenues", "VertBargBertLogit", function(object, preMerger = TRUE, market = FALSE) {
  if (!is.null(.vertical_policy_state(object))) {
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
  if (!is.null(.vertical_policy_state(object))) {
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
  calcShares(object@down, preMerger = preMerger, revenue = revenue)
})

#' @rdname vertical-methods
#' @export
setMethod("calcShares", "VertBarg2ndLogit", function(object, preMerger = TRUE, revenue = FALSE) {
  down <- object@down
  up <- object@up
  priceOutside <- down@priceOutside
  upPrice <- if (preMerger) up@pricePre else up@pricePost
  meanval <- down@slopes$meanval
  alpha <- down@slopes$alpha
  down@slopes$meanval <- meanval + alpha * (upPrice - priceOutside)
  calcShares(down, preMerger = preMerger, revenue = revenue)
})

#' @rdname vertical-methods
#' @export
setMethod("elast", "VertBargBertLogit", function(object, preMerger = TRUE, market = FALSE) {
  elast(object@down, preMerger = preMerger, market = market)
})

#' @rdname vertical-methods
#' @export
setMethod("diversion", "VertBargBertLogit", function(object, preMerger = TRUE, revenue = TRUE) {
  diversion(object@down, preMerger = preMerger, revenue = revenue)
})

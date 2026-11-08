# Vertical welfare and merger-output methods.

#' @rdname vertical-methods
#' @export
setMethod("hhi", "VertBargBertLogit", function(object, preMerger = TRUE,
  revenue = FALSE, insideonly = TRUE,
  tier = c("auto", "upstream", "downstream"), ...) {
  tier <- match.arg(tier)

  up <- object@up
  down <- object@down
  ownerUp <- ownerToMatrix(up, preMerger = preMerger)
  ownerDown <- ownerToMatrix(down, preMerger = preMerger)

  # Policy auctions allocate physical output using gross wholesale prices.
  # Match public vertical shares before applying the established HHI weights.
  if (!is.null(.vertical_policy_state(object)) &&
      methods::is(object, "VertBarg2ndLogit")) {
    wholesale <- if (preMerger) up@pricePre else up@pricePost
    down@slopes$meanval <- down@slopes$meanval +
      down@slopes$alpha * (wholesale - down@priceOutside)
  }

  # Keep the downstream demand shares and HHI weighting used by the
  # antitrust method while substituting the ownership matrix for a tier.
  hhiWithOwner <- function(owner) {
    thisDown <- down
    if (preMerger) thisDown@ownerPre <- owner else thisDown@ownerPost <- owner
    if (preMerger && any(!.vertical_active_subset(object, TRUE))) {
      thisDown <- .vertical_reduced_down(thisDown, .vertical_active_subset(object, TRUE))
    }
    hhi(thisDown, preMerger = preMerger, revenue = revenue,
      insideonly = insideonly)
  }

  components <- .vertical_merger_components(object)
  reportByTier <- isTRUE(components$hybrid) ||
    (isTRUE(components$upstream_horizontal) &&
      isTRUE(components$downstream_horizontal))

  # A hybrid has no single combined HHI.  Report the two relevant tier
  # values, each evaluated with the same vertical output shares.
  if (tier == "auto" && reportByTier) {
    return(c(upstream = hhiWithOwner(ownerUp),
      downstream = hhiWithOwner(ownerDown)))
  }

  # Explicit tier requests always use that tier's ownership.  This is also
  # useful for callers that need one side of a hybrid merger only.
  if (tier == "upstream") return(hhiWithOwner(ownerUp))
  if (tier == "downstream") return(hhiWithOwner(ownerDown))

  if (object@isHorizontal) {
    owner <- if (object@isUpstream) ownerUp else ownerDown
  } else {
    # Preserve the legacy vertical auto HHI: products tied to a common
    # upstream/downstream owner are treated as one downstream firm.
    if (preMerger) {
      thisDownOwner <- down@ownerPre
      thisUpOwner <- up@ownerPre
    } else {
      thisDownOwner <- down@ownerPost
      thisUpOwner <- up@ownerPost
    }
    vertFirms <- intersect(thisUpOwner, thisDownOwner)
    for (v in vertFirms) {
      isParty <- thisUpOwner == v | thisDownOwner == v
      ownerDown[isParty, isParty] <- 1
    }
    owner <- ownerDown
  }
  hhiWithOwner(owner)
})

#' @rdname vertical-methods
#' @export
setMethod("CV", "VertBargBertLogit", function(object) {
  down <- object@down
  previous <- .vertical_active_subset(object, TRUE)
  down@slopes$meanval[!previous] <- -Inf
  down@pricePre[!previous] <- 0
  logitCV <- selectMethod("CV", class(down))
  logitCV(down)
})

#' @rdname vertical-methods
#' @export
setMethod("CV", "VertBarg2ndLogit", function(object) {
  down <- object@down
  mktSize <- down@mktSize
  alpha <- down@slopes$alpha
  meanvalPre <- down@slopes$meanval
  priceDelta <- calcPriceDelta(object, levels = TRUE)
  marginsPre <- calcMargins(object, preMerger = TRUE, level = TRUE)
  marginsPost <- calcMargins(object, preMerger = FALSE, level = TRUE)
  sharesPre <- calcShares(object, preMerger = TRUE, revenue = FALSE)
  sharesPost <- calcShares(object, preMerger = FALSE, revenue = FALSE)
  result <- sum(marginsPost$down * sharesPost, na.rm = TRUE) -
    sum(marginsPre$down * sharesPre, na.rm = TRUE)
  idx <- down@normIndex
  subset <- down@subset
  if (is.na(idx)) {
    outVal <- 1
    mcDeltaOut <- down@priceOutside
  } else {
    outVal <- 0
    mcDeltaOut <- down@mcDelta[idx]
  }
  meanvalPost <- meanvalPre + alpha * (priceDelta$up + down@mcDelta - mcDeltaOut)
  previous <- .vertical_active_subset(object, TRUE)
  VPre <- sum(exp(meanvalPre[previous]), na.rm = TRUE) + outVal
  VPost <- sum(exp(meanvalPost[subset]), na.rm = TRUE) + outVal
  result <- result + log(VPost / VPre) / alpha
  if (!is.na(mktSize)) result <- mktSize * result
  result
})

#' @rdname vertical-methods
#' @export
setMethod("calcDiagnostics", "VertBargBertLogit", function(object,
  labels = object@down@labels) {
  if (!is.null(.vertical_policy_state(object))) {
    return(vertical_policy_diagnostics(object))
  }
  down <- object@down
  up <- object@up
  obsUpPrices <- up@prices
  obsUpMargins <- up@margins
  obsDownPrices <- down@prices
  obsDownMargins <- down@margins
  obsShares <- down@shares
  obsShares <- obsShares / sum(obsShares, na.rm = TRUE)
  obsElast <- down@mktElast
  preUpPrices <- unname(drop(up@pricePre))
  preDownPrices <- unname(drop(down@pricePre))
  preMargins <- calcMargins(object, preMerger = TRUE)
  preShares <- drop(calcShares(object, preMerger = TRUE))
  preShares <- drop(preShares / sum(preShares, na.rm = TRUE))
  preElast <- elast(object, preMerger = TRUE, market = TRUE)
  res <- data.frame(
    upPrices = 1 - obsUpPrices / preUpPrices,
    downPrices = 1 - obsDownPrices / preDownPrices,
    Shares = 1 - obsShares / preShares,
    upMargins = 1 - obsUpMargins / preMargins$up,
    downMargins = 1 - obsDownMargins / preMargins$down,
    `Market Elasticity` = 1 - obsElast / preElast,
    check.names = FALSE
  ) * 100
  rownames(res) <- labels
  res
})

#' @rdname vertical-methods
#' @export
setMethod("calcProducerSurplus", "VertBargBertLogit", function(object,
  preMerger = TRUE) {
  mktSize <- object@down@mktSize
  margins <- calcMargins(object, preMerger = preMerger, level = TRUE)
  output <- calcShares(object, preMerger)
  if (is.na(mktSize)) {
    warning("'insideSize' is missing; using normalized shares instead of quantities. Producer-surplus results are not in market units.")
    mktSize <- 1
  }
  psup <- margins$up * output * mktSize
  psdown <- margins$down * output * mktSize
  inactive <- !.vertical_active_subset(object, preMerger)
  psup[inactive] <- psdown[inactive] <- 0
  names(psup) <- names(psdown) <- object@down@labels
  list(up = psup, down = psdown)
})

#' @rdname vertical-methods
#' @export
setMethod("HypoMonTest", "VertBargBertLogit", function(object, prodIndex,
  ssnip, hmg = c("2023", "2010"), ...) {
  if (missing(ssnip)) ssnip <- ifelse(object@down@output, .05, -.05)
  hmg <- match.arg(hmg)
  down <- object@down
  down@ownerPre <- ownerToMatrix(down, preMerger = TRUE)
  down@ownerPost <- ownerToMatrix(down, preMerger = FALSE)
  down@pricePre <- calcPrices(down, preMerger = TRUE)
  HypoMonTest(object = down, prodIndex = prodIndex, ssnip = ssnip,
    hmg = hmg, ...)
})

#' @rdname vertical-methods
#' @export
setMethod("calcPriceDelta", "VertBargBertLogit", function(object,
  levels = FALSE, market = FALSE, ...) {
  up <- object@up
  down <- object@down
  if (!is.null(.vertical_policy_state(object))) {
    delta <- list(up = up@pricePost - up@pricePre,
      down = down@pricePost - down@pricePre)
    base <- list(up = up@pricePre, down = down@pricePre)
    if (market) {
      s0 <- calcShares(object, TRUE)
      s1 <- calcShares(object, FALSE)
      delta <- list(up = sum(up@pricePost * s1, na.rm = TRUE) -
          sum(up@pricePre * s0, na.rm = TRUE),
        down = sum(down@pricePost * s1, na.rm = TRUE) -
          sum(down@pricePre * s0, na.rm = TRUE))
      base <- lapply(base, function(p) sum(p * s0, na.rm = TRUE))
    }
    if (!levels) delta <- Map(`/`, delta, base)
    return(delta)
  }
  chain_level <- object@chain_level
  marginsPre <- calcMargins(object, preMerger = TRUE, level = TRUE)
  marginsPost <- calcMargins(object, preMerger = FALSE, level = TRUE)
  sharesPre <- calcShares(object, preMerger = TRUE, revenue = FALSE)
  sharesPost <- calcShares(object, preMerger = FALSE, revenue = FALSE)
  upMCPre <- ifelse(is.na(up@mcPre), 0, up@mcPre)
  downMCPre <- ifelse(is.na(down@mcPre), 0, down@mcPre)
  upPricePre <- up@pricePre
  upMCPost <- ifelse(is.na(up@mcPost), 0, up@mcPost)
  downMCPost <- ifelse(is.na(down@mcPost), 0, down@mcPost)
  upPricePost <- up@pricePost
  if (!market) {
    mcDeltaUp <- upMCPost - upMCPre
    mcDeltaDown <- downMCPost - downMCPre + upPricePost - upPricePre
    upDelta <- marginsPost$up - marginsPre$up + mcDeltaUp
    downDelta <- marginsPost$down - marginsPre$down + mcDeltaDown
    if (chain_level == "retailer") upDelta <- rep(0, length(upDelta))
    else if (chain_level == "wholesaler") downDelta <- rep(0, length(downDelta))
    upPricePre <- up@pricePre
    downPricePre <- down@pricePre
  } else {
    mcDeltaUp <- upMCPost * sharesPost - upMCPre * sharesPre
    mcDeltaDown <- (downMCPost + upPricePost) * sharesPost -
      (downMCPre + upPricePre) * sharesPre
    upDelta <- marginsPost$up * sharesPost - marginsPre$up * sharesPre + mcDeltaUp
    downDelta <- marginsPost$down * sharesPost - marginsPre$down * sharesPre + mcDeltaDown
    upPricePre <- up@pricePre * sharesPre
    downPricePre <- down@pricePre * sharesPre
    upDelta <- sum(upDelta, na.rm = TRUE)
    downDelta <- sum(downDelta, na.rm = TRUE)
    if (chain_level == "retailer") upDelta <- 0
    else if (chain_level == "wholesaler") downDelta <- 0
    upPricePre <- sum(upPricePre, na.rm = TRUE)
    downPricePre <- sum(downPricePre, na.rm = TRUE)
  }
  if (!levels) {
    upDelta <- upDelta / upPricePre
    downDelta <- downDelta / downPricePre
  }
  list(up = upDelta, down = downDelta)
})

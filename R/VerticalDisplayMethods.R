# Vertical summary and display methods.

#' @rdname vertical-methods
#' @export
setMethod("summary", "VertBargBertLogit", function(object, revenue = TRUE,
  levels = FALSE, parameters = FALSE, market = FALSE, insideOnly = FALSE,
  digits = 2, ...) {
  curWidth <- getOption("width")
  up <- object@up
  down <- object@down
  priceUpPre <- up@pricePre
  priceUpPost <- up@pricePost
  priceDownPre <- down@pricePre
  priceDownPost <- down@pricePost
  priceDelta <- calcPriceDelta(object, levels = levels, market = market)
  if (!levels) priceDelta <- lapply(priceDelta, function(x) x * 100)
  outPre <- calcShares(object, preMerger = TRUE, revenue = revenue) * 100
  outPost <- calcShares(object, preMerger = FALSE, revenue = revenue) * 100
  if (insideOnly) {
    outPre <- outPre / sum(outPre) * 100
    outPost <- outPost / sum(outPost, na.rm = TRUE) * 100
  }
  sumlabels <- paste("shares", c("Pre", "Post"), sep = "")
  mcDeltaUp <- up@mcDelta * 100
  mcDeltaDown <- down@mcDelta * 100
  outDelta <- if (levels) outPost - outPre else (outPost / outPre - 1) * 100

  components <- .vertical_merger_components(object)
  reportByTier <- isTRUE(components$hybrid) ||
    (isTRUE(components$upstream_horizontal) &&
      isTRUE(components$downstream_horizontal))
  if (reportByTier) {
    partyIndices <- function(x, n) {
      if (is.null(x)) return(integer())
      if (is.list(x)) x <- unlist(x, use.names = FALSE)
      if (is.logical(x)) idx <- which(x) else {
        idx <- suppressWarnings(as.integer(x))
      }
      idx <- idx[is.finite(idx) & idx >= 1L & idx <= n]
      unique(idx)
    }
    partyUp <- partyIndices(components$party_up, length(up@labels))
    partyDown <- partyIndices(components$party_down, length(down@labels))
    isPartyLogical <- logical(length(down@labels))
    isPartyLogical[partyDown] <- TRUE
    # Upstream and downstream products share the vertical product index.  A
    # party at either tier therefore marks the corresponding display row.
    partyUp <- partyUp[partyUp <= length(isPartyLogical)]
    isPartyLogical[partyUp] <- TRUE
  } else {
    # Preserve legacy party display and single-HHI behavior for pure cases.
    isPartyHorzDown <- down@ownerPost %in% down@ownerPre &
      down@ownerPost != down@ownerPre
    if (any(isPartyHorzDown)) {
      isPartyHorzDown <- down@ownerPost %in%
        down@ownerPost[isPartyHorzDown]
    }
    isPartyHorzUp <- up@ownerPost %in% up@ownerPre &
      up@ownerPost != up@ownerPre
    if (any(isPartyHorzUp)) {
      isPartyHorzUp <- up@ownerPost %in% up@ownerPost[isPartyHorzUp]
    }
    isPartyVert <- unique(down@ownerPost[
      down@ownerPost == up@ownerPost & down@ownerPre != up@ownerPre
    ])
    isPartyVert <- down@ownerPost %in% isPartyVert |
      up@ownerPost %in% isPartyVert
    isPartyLogical <- isPartyHorzDown | isPartyHorzUp | isPartyVert
  }
  isParty <- factor(isPartyLogical,
    levels = c(FALSE, TRUE), labels = c(" ", "*"))

  results <- data.frame(
    priceUpPre = priceUpPre, priceUpPost = priceUpPost,
    priceUpDelta = priceDelta$up, priceDownPre = priceDownPre,
    priceDownPost = priceDownPost, priceDownDelta = priceDelta$down,
    outputPre = outPre, outputPost = outPost, outputDelta = outDelta
  )
  if (sum(abs(mcDeltaUp)) > 0) results <- cbind(results, mcDeltaUp = mcDeltaUp)
  if (sum(abs(mcDeltaDown)) > 0) results <- cbind(results, mcDeltaDown = mcDeltaDown)
  rownames(results) <- paste(isParty, down@labels)
  sharesPost <- calcShares(object, FALSE, revenue)

  if (market) {
    thiscv <- NA_real_
    try(thiscv <- CV(object), silent = TRUE)
    try(thispsPre <- calcProducerSurplus(object, TRUE), silent = TRUE)
    try(thispsPost <- calcProducerSurplus(object, FALSE), silent = TRUE)
    thispsdeltaUp <- sum(thispsPost$up - thispsPre$up, na.rm = TRUE)
    thispsdeltaDown <- sum(thispsPost$down - thispsPre$down, na.rm = TRUE)
    if (object@chain_level == "wholesaler") thispsdeltaDown <- -thispsdeltaUp
    hhiShares <- outPre / sum(outPre)
    hhiUp <- as.integer(HHI(hhiShares, owner = up@ownerPost) -
      HHI(hhiShares, owner = up@ownerPre))
    hhiDown <- as.integer(HHI(hhiShares, owner = down@ownerPost) -
      HHI(hhiShares, owner = down@ownerPre))

    # Preserve the legacy single-HHI treatment for pure vertical cases:
    # party products are assigned to one downstream control group.  Hybrid
    # cases do not use this synthetic grouping; they report each tier.
    downOwnerVertical <- down@ownerPost
    partyLabels <- unique(downOwnerVertical[isParty])
    if (length(partyLabels)) {
      downOwnerVertical[isPartyLogical] <- partyLabels[[1L]]
    }
    isVertical <- isTRUE(components$vertical)
    hhiVert <- as.integer(HHI(hhiShares, owner = downOwnerVertical) -
      HHI(hhiShares, owner = down@ownerPre))
    hhiDelta <- if (isVertical) hhiVert else max(hhiUp, hhiDown)
    hhiResults <- if (reportByTier) {
      data.frame(
        `Upstream HHI Change` = hhiUp,
        `Downstream HHI Change` = hhiDown,
        check.names = FALSE
      )
    } else {
      data.frame(`HHI Change` = hhiDelta, check.names = FALSE)
    }
    results <- cbind(hhiResults, with(results, data.frame(
      `Up Price Change (%)` = priceDelta$up,
      `Down Price Change (%)` = priceDelta$down,
      `Consumer Harm ($)` = thiscv,
      `Up Producer Benefit ($)` = thispsdeltaUp,
      `Down Producer Benefit ($)` = thispsdeltaDown,
      `Difference ($)` = thiscv - thispsdeltaUp - thispsdeltaDown,
      check.names = FALSE
    )))
    if (levels) colnames(results) <- gsub("%", "$/unit", colnames(results))
  }

  colnames(results)[colnames(results) %in% c("outputPre", "outputPost")] <- sumlabels
  cat("\nMerger simulation results under '", class(object), "' demand:\n\n", sep = "")
  options(width = ifelse(market, 25, 100))
  print(round(results, digits), digits = digits, row.names = ifelse(market, FALSE, TRUE))
  options(width = curWidth)
  if (!market) {
    results <- cbind(isParty, results)
    rownames(results) <- down@labels
    cat("\n\tNotes: '*' indicates merging parties' products.\n ")
    if (levels) cat("\tDeltas are level changes.\n") else cat("\tDeltas are percent changes.\n")
    if (revenue) cat("\tOutput is based on revenues.\n") else cat("\tOutput is based on units sold.\n")
  }
  cat("\n\n")
  if (parameters) {
    print(getParms(object), digits = digits)
    if (.hasSlot(object, "constraint") && object@constraint) {
      cat("\nNote: (non-singleton) nesting parameters are constrained to be equal")
    }
    cat("\n\n")
  }
  invisible(results)
})

#' @rdname vertical-methods
#' @export
setMethod("show", "VertBargBertLogit", function(object) {
  summary(object, market = TRUE)
  NULL
})

# Vertical marginal-cost calculations.

#' @rdname vertical-methods
#' @export
setMethod(
  f = "calcMC",
  signature = "VertBargBertLogit",
  definition = function(object, preMerger = TRUE) {
    # Policy states explicitly promote physical pre-state costs between steps.
    # The inner antitrust primitive remains anchored at its originating fit;
    # reading that primitive for a promoted pre-state would lose prior shocks.
    if (preMerger && !is.null(.vertical_policy_state(object))) {
      return(list(up = object@up@mcPre, down = object@down@mcPre))
    }
    # After construction, the common antitrust cost state on each inner model
    # is authoritative.  Delegating to the public generic keeps persistence
    # and cost-mode semantics in antitrust rather than copying them here.
    if (isTRUE(attr(object, "vertical_cost_state_initialized"))) {
      mcUp <- as.vector(calcMC(object@up, preMerger = preMerger))
      mcDown <- as.vector(calcMC(object@down, preMerger = preMerger))
      names(mcUp) <- object@up@labels
      names(mcDown) <- object@down@labels
      return(list(up = mcUp, down = mcDown))
    }

    # Initial calibration path: infer costs from the observed vertical
    # equilibrium.  Downstream marginal cost subtracts the upstream transfer
    # price; upstream marginal cost is its own price less its negotiated
    # margin.  This is the legacy vertical economics and is intentionally
    # needed only before common cost state is initialized.
    up <- object@up
    down <- object@down

    if (length(up@pricePre) == 0) {
      priceUpPre <- up@prices
      object@up@pricePre <- up@prices
    } else {
      priceUpPre <- up@pricePre
    }

    if (length(down@pricePre) == 0) {
      priceDownPre <- down@prices
      object@down@pricePre <- priceDownPre
    } else {
      priceDownPre <- down@pricePre
    }

    marginsPre <- calcMargins(object, preMerger = TRUE, level = TRUE)
    mcDown <- -(marginsPre$down - priceDownPre + priceUpPre)
    mcUp <- -(marginsPre$up - priceUpPre)

    if (!preMerger) {
      mcUp <- mcUp * (1 + up@mcDelta)
      mcDown <- mcDown * (1 + down@mcDelta)
    }

    mcUp <- as.vector(mcUp)
    mcDown <- as.vector(mcDown)
    names(mcUp) <- up@labels
    names(mcDown) <- down@labels

    isNegUpMC <- mcUp < 0
    isNegDownMC <- mcDown < 0
    if (preMerger && any(isNegUpMC, na.rm = TRUE)) {
      warning(paste(
        "Negative upstream marginal costs were calibrated for the following firms:",
        paste(up@labels[isNegUpMC & !is.na(isNegUpMC)], collapse = ",")
      ))
    }
    if (preMerger && any(isNegDownMC, na.rm = TRUE)) {
      warning(paste(
        "Negative downstream marginal costs were calibrated for the following firms:",
        paste(down@labels[isNegDownMC & !is.na(isNegDownMC)], collapse = ",")
      ))
    }

    list(up = mcUp, down = mcDown)
  }
)

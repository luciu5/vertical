#' Construct and simulate a vertical bargaining model
#'
#' Calibrates downstream Logit or Nested Logit demand and upstream bargaining
#' power from observed prices, shares, and margins. It then solves the
#' post-counterfactual Nash-in-Nash supply-chain equilibrium with downstream
#' Bertrand pricing or a second-score auction. The `chain_level` option can
#' hold one tier's observed prices fixed while solving the other tier.
#'
#' @param supplyDown Downstream conduct: `"bertrand"` or `"2nd"`.
#' @param sharesDown Downstream product shares.
#' @param pricesDown Downstream prices.
#' @param marginsDown Downstream margins.
#' @param ownerPreDown,ownerPostDown Downstream ownership vectors. Post-merger
#'   ownership may combine horizontal consolidation with vertical integration.
#' @param nests Optional downstream nests.
#' @param diversions Optional downstream diversion matrix.
#' @param mcDeltaDown Downstream proportional cost changes.
#' @param pricesUp Upstream prices.
#' @param marginsUp Upstream margins.
#' @param ownerPreUp,ownerPostUp Upstream ownership vectors. Simultaneous
#'   upstream/downstream ownership changes are supported.
#' @param mcDeltaUp Upstream proportional cost changes.
#' @param normIndex Normalized downstream product index.
#' @param subset A logical vector selecting products that remain active in the
#'   post-counterfactual state. The pre-counterfactual state always keeps all
#'   products active. Inactive post-counterfactual products have `NA` prices,
#'   margins, and revenues. Standard and nested Logit report zero shares and
#'   quantities for them; second-score reports `NA` shares and quantities.
#' @param insideSize Inside market size.
#' @param priceOutside Outside-good price.
#' @param priceStartDown,priceStartUp Price solver starting values.
#' @param isMax Retained for compatibility.
#' @param constrain Bargaining-power restriction.
#' @param chain_level Equilibrium level: `"full"`, `"wholesaler"`, or
#'   `"retailer"`.
#' @param control.slopes,control.equ Optimizer controls.
#' @param labels Product labels.
#' @param ... Additional solver options.
#' @return A `VertBargBertLogit`, `VertBargBertLogitNests`, or
#'   `VertBarg2ndLogit` object.
#' @author Charles Taragin
#' @references Sheu, G. and Taragin, C. (2021), "Simulating mergers in a
#'   vertical supply chain with bargaining," *RAND Journal of Economics*,
#'   52: 596--632. \doi{10.1111/1756-2171.12385}.
#' @export
vertical.barg <- function(
    supplyDown = c("bertrand", "2nd"),
    sharesDown, pricesDown, marginsDown,
    ownerPreDown, ownerPostDown,
    nests = rep(NA, length(pricesDown)),
    diversions = diversions,
    mcDeltaDown = rep(0, length(pricesDown)),
    pricesUp, marginsUp,
    ownerPreUp, ownerPostUp,
    mcDeltaUp = rep(0, length(pricesUp)),
    normIndex = ifelse(isTRUE(all.equal(sum(sharesDown), 1,
      check.names = FALSE)), 1, NA),
    subset = rep(TRUE, length(pricesDown)),
    insideSize = NA_real_, priceOutside = 0,
    priceStartDown = pricesDown, priceStartUp = pricesUp,
    isMax = FALSE,
    constrain = c("global", "pair", "wholesaler", "retailer"),
    chain_level = c("full", "wholesaler", "retailer"),
    control.slopes, control.equ,
    labels = paste0("Prod", 1:length(pricesUp)), ...
) {
  trapWarning <- function(expr) {
    withCallingHandlers(
      expr,
      warning = function(w) {
        if (grepl("'ownerPost' and 'ownerPre' are the same",
          conditionMessage(w), perl = TRUE)) {
          invokeRestart("muffleWarning")
        }
      }
    )
  }

  supplyDown <- match.arg(supplyDown)
  constrain <- match.arg(constrain)
  chain_level <- match.arg(chain_level)

  classification <- .vertical_classify_ownership(
    owner_pre_up = ownerPreUp, owner_post_up = ownerPostUp,
    owner_pre_down = ownerPreDown, owner_post_down = ownerPostDown
  )
  isHorizontal <- classification$horizontal
  isUpstream <- classification$upstream_horizontal

  if (supplyDown == "2nd") marginsDown <- marginsDown * pricesDown

  if (missing(nests) || any(is.na(nests))) {
    if (supplyDown == "bertrand") {
      downClass <- "Logit"
      resclass <- "VertBargBertLogit"
    } else {
      downClass <- "Auction2ndLogit"
      resclass <- "VertBarg2ndLogit"
    }
  } else {
    nests <- factor(nests, levels = unique(nests))
    if (supplyDown == "bertrand") {
      resclass <- "VertBargBertLogitNests"
      downClass <- "LogitNests"
    } else {
      stop("Vertical Supply Chain with Nested Logit and 2nd score auction has not yet been implemented.")
    }
  }

  if (missing(diversions)) {
    diversions <- tcrossprod(1 / (1 - sharesDown), sharesDown)
    diag(diversions) <- -1
  }

  trapWarning(up <- new(
    "Bargaining", prices = pricesUp, shares = sharesDown,
    subset = subset, margins = marginsUp,
    ownerPre = ownerPreUp, ownerPost = ownerPostUp,
    mcDelta = mcDeltaUp, priceStart = priceStartUp, labels = labels
  ))

  if (any(is.na(nests))) {
    trapWarning(down <- new(
      downClass, prices = pricesDown, shares = sharesDown,
      margins = marginsDown, normIndex = normIndex,
      ownerPre = ownerPreDown, ownerPost = ownerPostDown,
      insideSize = insideSize, mcDelta = mcDeltaDown,
      subset = subset, diversion = diversions,
      weights = rep(1, length(sharesDown)), priceOutside = priceOutside,
      priceStart = priceStartDown,
      shareInside = ifelse(isTRUE(all.equal(sum(sharesDown), 1,
        check.names = FALSE, tolerance = 1e-3)), 1, sum(sharesDown)),
      labels = labels
    ))
  } else {
    trapWarning(down <- new(
      downClass, prices = pricesDown, shares = sharesDown,
      margins = marginsDown, normIndex = normIndex,
      ownerPre = ownerPreDown, ownerPost = ownerPostDown,
      insideSize = insideSize, mcDelta = mcDeltaDown,
      nests = nests, subset = subset,
      weights = rep(1, length(sharesDown)),
      shareInside = ifelse(isTRUE(all.equal(sum(sharesDown), 1,
        check.names = FALSE, tolerance = 1e-3)), 1, sum(sharesDown)),
      priceOutside = priceOutside, priceStart = priceStartDown,
      labels = labels
    ))
  }

  if (!missing(control.slopes)) down@control.slopes <- control.slopes
  if (!missing(control.equ)) down@control.equ <- control.equ

  result <- new(
    resclass, up = up, down = down, constrain = constrain,
    chain_level = chain_level, supplyDown = supplyDown,
    isHorizontal = isHorizontal, isUpstream = isUpstream
  )

  result <- calcSlopes(result)

  # Keep legacy vertical cost timing. Fit-level lifecycle code initializes
  # common antitrust cost state after this constructor completes.
  mcPre <- calcMC(result, TRUE)
  mcPost <- calcMC(result, FALSE)
  result@down@mcPre <- mcPre$down
  result@down@mcPost <- mcPost$down
  result@up@mcPre <- mcPre$up
  result@up@mcPost <- mcPost$up

  resultsPre <- calcPrices(result, preMerger = TRUE)
  result@down@pricePre <- resultsPre$down
  result@up@pricePre <- resultsPre$up

  resultsPost <- calcPrices(result, preMerger = FALSE)
  result@down@pricePost <- resultsPost$down
  result@up@pricePost <- resultsPost$up

  result
}

#' Vertical ownership and bargaining matrices
#'
#' Methods for the legacy vertical equilibrium objects reuse antitrust-owned
#' generics while applying vertical ownership, bargaining, pricing, output,
#' and welfare economics.
#'
#' @name vertical-methods
#' @param object A vertical equilibrium object or fitted vertical model.
#' @param preMerger Whether to evaluate the pre-counterfactual state.
#' @param level Whether to return margin levels rather than shares of price.
#' @param market Whether to aggregate results to the market level.
#' @param revenue Whether shares or output should be revenue weighted.
#' @param insideonly Whether HHI should condition on the inside market.
#' @param labels Product labels used in diagnostics.
#' @param prodIndex Products included in a hypothetical-monopolist test.
#' @param ssnip The candidate SSNIP.
#' @param hmg Horizontal Merger Guidelines vintage.
#' @param levels Whether changes are returned in levels.
#' @param party Whether to return merging-party results only.
#' @param isMax Whether to run a local profit-maximum check when supported.
#' @param index Price-index convention used for market aggregation.
#' @param parameters Whether a summary should print calibrated parameters.
#' @param insideOnly Whether a summary should condition on inside products.
#' @param digits Number of digits to display.
#' @param ... Additional arguments passed to the applicable antitrust generic
#'   or equilibrium solver.
NULL

#' @rdname vertical-methods
#' @export
setMethod(
  f = "ownerToMatrix",
  signature = "VertBargBertLogit",
  definition = function(object, preMerger = TRUE) {
    up <- object@up
    down <- object@down

    if (preMerger) {
      thisUpOwner <- up@ownerPre
      thisDownOwner <- down@ownerPre
      bargParm <- up@bargpowerPre
    } else {
      thisUpOwner <- up@ownerPost
      thisDownOwner <- down@ownerPost
      bargParm <- up@bargpowerPost
    }

    thisUpOwnerMat <- ownerToMatrix(up, preMerger = preMerger)
    thisDownOwnerMat <- ownerToMatrix(down, preMerger = preMerger)
    nprods <- nrow(thisDownOwnerMat)

    if (!is.matrix(thisUpOwner) && !is.matrix(thisDownOwner)) {
      vertFirms <- intersect(thisUpOwner, thisDownOwner)
      thisDownOwnerMatVertical <- matrix(0, nrow = nprods, ncol = nprods)

      for (v in vertFirms) {
        vertrows <- thisUpOwner != v & thisDownOwner == v
        thisUpOwnerMat[vertrows, thisUpOwner == v] <-
          -(1 - bargParm[vertrows]) / bargParm[vertrows]
      }

      # An integrated product has no bargaining disagreement payoff.
      for (n in which(thisUpOwner == thisDownOwner)) {
        thisUpOwnerMat[n, -n] <- thisUpOwnerMat[-n, n] <- 0
      }

      ownerDownLambda <- thisDownOwnerMat * (1 - bargParm) / bargParm
      for (v in vertFirms) {
        vertrows <- thisUpOwner == v & thisDownOwner != v
        thisDownOwnerMatVertical[thisDownOwner == v, vertrows] <- 1
        ownerDownLambda[vertrows, thisDownOwner == v] <- -1
      }

      if (preMerger) {
        object@ownerDownPre <- thisDownOwnerMatVertical
        object@ownerDownLambdaPre <- ownerDownLambda
        object@ownerUpLambdaPre <- thisUpOwnerMat
      } else {
        object@ownerDownPost <- thisDownOwnerMatVertical
        object@ownerDownLambdaPost <- ownerDownLambda
        object@ownerUpLambdaPost <- thisUpOwnerMat
      }
    }

    object
  }
)

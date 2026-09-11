# Vertical equilibrium price methods.

#' @rdname vertical-methods
#' @export
setMethod(
  f = "calcPrices",
  signature = "VertBargBertLogit",
  definition = function(object, preMerger = TRUE, ...) {
    up <- object@up
    down <- object@down
    chain_level <- object@chain_level
    upPricePre <- up@prices
    downPricePre <- down@prices
    subsetDown <- down@subset
    alpha <- down@slopes$alpha
    meanval <- down@slopes$meanval[subsetDown]
    priceStartUp <- up@priceStart
    priceStartDown <- down@priceStart
    if (chain_level == "full") {
      priceStart <- c(priceStartUp[subsetDown], priceStartDown[subsetDown])
    } else if (chain_level == "retailer") {
      priceStart <- priceStartDown[subsetDown]
    } else if (chain_level == "wholesaler") {
      priceStart <- priceStartUp[subsetDown]
    }
    nprods <- length(meanval)
    ownerUp <- ownerToMatrix(up, preMerger = preMerger)[subsetDown, subsetDown]
    ownerDown <- ownerToMatrix(down, preMerger = preMerger)[subsetDown, subsetDown]

    if (preMerger) {
      up@ownerPre <- ownerUp
      down@ownerPre <- ownerDown
      bargparm <- up@bargpowerPre[subsetDown]
      mcUp <- up@mcPre[subsetDown]
      mcDown <- down@mcPre[subsetDown]
    } else {
      up@ownerPost <- ownerUp
      down@ownerPost <- ownerDown
      bargparm <- up@bargpowerPost[subsetDown]
      mcUp <- up@mcPost[subsetDown]
      mcDown <- down@mcPost[subsetDown]
    }

    FOC <- function(priceCand) {
      thisobj <- object
      if (chain_level == "full") {
        priceCandUp <- priceCand[seq_len(length(priceCand) / 2)]
        priceCandDown <- priceCand[-seq_len(length(priceCandUp))]
      } else if (chain_level == "retailer") {
        priceCandUp <- up@prices[subsetDown]
        priceCandDown <- priceCand
      } else {
        priceCandDown <- down@prices[subsetDown]
        priceCandUp <- priceCand
      }
      if (preMerger) {
        up@pricePre <- priceCandUp
        down@pricePre <- priceCandDown
      } else {
        up@pricePost <- priceCandUp
        down@pricePost <- priceCandDown
      }
      thisobj@up <- up
      thisobj@down <- down
      theseMargins <- calcMargins(thisobj, preMerger = preMerger, level = TRUE)
      upFOC <- priceCandUp - mcUp - theseMargins$up
      downFOC <- priceCandDown - priceCandUp - mcDown - theseMargins$down
      if (chain_level == "full") {
        c(upFOC, downFOC)
      } else if (chain_level == "retailer") {
        downFOC
      } else {
        upFOC
      }
    }

    nleqslv_maxit <- as.integer(down@control.equ$maxit)
    if (length(nleqslv_maxit) == 0 || is.na(nleqslv_maxit[1]) ||
        nleqslv_maxit[1] < 1) nleqslv_maxit <- 150L
    minResult <- nleqslv::nleqslv(
      priceStart, FOC, method = "Broyden",
      control = list(ftol = down@control.equ$tol, maxit = nleqslv_maxit)
    )
    if (minResult$termcd > 2) {
      minResult <- BB::BBsolve(priceStart, FOC, quiet = TRUE,
        control = down@control.equ, ...)
      priceResult <- minResult$par
      if (minResult$convergence != 0) {
        warning("'calcPrices' nonlinear solver may not have successfully converged. 'BBsolve' reports: '",
          minResult$message, "'")
      }
    } else {
      priceResult <- minResult$x
      if (minResult$termcd > 1) {
        warning("'calcPrices' may not have fully converged. 'nleqslv' termcd: ",
          minResult$termcd)
      }
    }

    minResultUp <- rep(NA, length(priceStartUp))
    minResultDown <- rep(NA, length(priceStartDown))
    if (chain_level == "full") {
      minResultUp[subsetDown] <- priceResult[seq_along(priceStartUp[subsetDown])]
      minResultDown[subsetDown] <- priceResult[-seq_along(priceStartUp[subsetDown])]
    } else if (chain_level == "retailer") {
      minResultUp[subsetDown] <- upPricePre[subsetDown]
      minResultDown[subsetDown] <- priceResult
    } else {
      minResultUp[subsetDown] <- priceResult
      minResultDown[subsetDown] <- downPricePre[subsetDown]
    }
    list(up = minResultUp, down = minResultDown)
  }
)

#' @rdname vertical-methods
#' @export
setMethod(
  f = "calcPrices",
  signature = "VertBarg2ndLogit",
  definition = function(object, preMerger = TRUE, ...) {
    up <- object@up
    down <- object@down
    alpha <- down@slopes$alpha
    meanval <- down@slopes$meanval
    subsetDown <- down@subset
    priceStartUp <- up@priceStart
    priceStartDown <- down@priceStart
    priceStart <- c(priceStartUp[subsetDown], priceStartDown[subsetDown])
    nprods <- length(priceStartDown)
    ownerDown <- ownerToMatrix(down, preMerger = preMerger)[subsetDown, subsetDown]
    priceOutside <- down@priceOutside

    if (preMerger) {
      bargparm <- up@bargpowerPre[subsetDown]
      down@ownerPre <- ownerDown
      mcUp <- up@mcPre[subsetDown]
      mcDown <- down@mcPre[subsetDown]
      ownerDownLambda <- object@ownerDownLambdaPre[subsetDown, subsetDown]
      ownerUpLambda <- object@ownerUpLambdaPre[subsetDown, subsetDown]
    } else {
      down@ownerPost <- ownerDown
      bargparm <- up@bargpowerPost[subsetDown]
      mcUp <- up@mcPost[subsetDown]
      mcDown <- down@mcPost[subsetDown]
      ownerDownLambda <- object@ownerDownLambdaPost[subsetDown, subsetDown]
      ownerUpLambda <- object@ownerUpLambdaPost[subsetDown, subsetDown]
    }

    FOC <- function(priceCand) {
      priceCandUp <- priceCand[seq_len(length(priceCand) / 2)]
      priceCandDown <- priceCand[-seq_len(length(priceCandUp))]
      down@slopes$meanval <- meanval + alpha * (priceCandUp - priceOutside)
      shareCandDown <- calcShares(down, preMerger = preMerger, revenue = FALSE)
      marginsDownCand <- calcMargins(down, preMerger = preMerger, level = TRUE)
      div <- tcrossprod(1 / (1 - shareCandDown), shareCandDown) * shareCandDown
      diag(div) <- -shareCandDown
      div <- as.vector(div)
      upFOC <- (ownerUpLambda * div) %*% (priceCandUp - mcUp) -
        ((ownerDownLambda * div) %*% marginsDownCand)
      downFOC <- priceCandDown - priceCandUp - mcDown - marginsDownCand
      c(downFOC, as.vector(upFOC))
    }

    nleqslv_maxit <- as.integer(down@control.equ$maxit)
    if (length(nleqslv_maxit) == 0 || is.na(nleqslv_maxit[1]) ||
        nleqslv_maxit[1] < 1) nleqslv_maxit <- 150L
    minResult <- nleqslv::nleqslv(
      priceStart, FOC, method = "Broyden",
      control = list(ftol = down@control.equ$tol, maxit = nleqslv_maxit)
    )
    if (minResult$termcd > 2) {
      minResult <- BB::BBsolve(priceStart, FOC, quiet = TRUE,
        control = down@control.equ, ...)
      priceResult <- minResult$par
      if (minResult$convergence != 0) {
        warning("'calcPrices' nonlinear solver may not have successfully converged. 'BBsolve' reports: '",
          minResult$message, "'")
      }
    } else {
      priceResult <- minResult$x
      if (minResult$termcd > 1) {
        warning("'calcPrices' may not have fully converged. 'nleqslv' termcd: ",
          minResult$termcd)
      }
    }
    minResultUp <- rep(NA, length(priceStartUp))
    minResultDown <- rep(NA, length(priceStartDown))
    minResultUp[subsetDown] <- priceResult[seq_along(priceStartUp[subsetDown])]
    minResultDown[subsetDown] <- priceResult[-seq_along(priceStartUp[subsetDown])]
    down@slopes$meanval <- meanval + alpha * (minResultUp - priceOutside)
    list(up = minResultUp, down = minResultDown)
  }
)

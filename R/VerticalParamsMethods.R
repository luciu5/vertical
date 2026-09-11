# Vertical bargaining and parameter methods.

#' @rdname vertical-methods
#' @export
setMethod(
  f = "calcSlopes",
  signature = "VertBargBertLogit",
  definition = function(object) {
    constrain <- object@constrain
    is2nd <- any(grepl("2nd", class(object)))
    up <- object@up
    down <- object@down
    sigma <- 0.1
    if (grepl("Nest", class(object))) {
      nests <- down@nests
      sigma <- rep(0.1, nlevels(nests))
      names(sigma) <- levels(nests)
    }

    owner.up.pre <- up@ownerPre
    owner.down.pre <- down@ownerPre
    owner.up.post <- up@ownerPost
    owner.down.post <- down@ownerPost
    pricesUp <- up@prices
    marginsUp <- up@margins
    marginsDown <- down@margins
    idx <- down@normIndex
    sharesDown <- down@shares
    pricesDown <- down@prices
    down@pricePre <- pricesDown

    if (is.na(idx)) {
      idxShare <- 1 - down@shareInside
      idxPrice <- down@priceOutside
    } else {
      idxShare <- sharesDown[idx]
      idxPrice <- pricesDown[idx]
    }

    id <- data.frame(up.firm = owner.up.pre, down.firm = owner.down.pre)
    nprods <- nrow(id)
    if (constrain == "pair") {
      id <- with(id, interaction(up.firm, down.firm))
    } else if (constrain == "wholesaler") {
      id <- with(id, up.firm)
    } else if (constrain == "retailer") {
      id <- with(id, down.firm)
    } else {
      id <- rep(1, nprods)
    }
    id <- factor(id)

    marginsUp <- marginsUp * pricesUp
    if (!is2nd) marginsDown <- marginsDown * pricesDown

    # alpha is initialized from a single-product unintegrated firm and each
    # bargaining-power parameter starts at one half.
    bStart <- rep(0.5, nlevels(id))
    firstAvail <- which(!is.na(marginsDown))[1]
    alphaStart <- -1 / (marginsDown[firstAvail] * (1 - sharesDown[firstAvail]))
    parmStart <- c(alphaStart, bStart)

    div <- tcrossprod(1 / (1 - sharesDown), sharesDown) * sharesDown
    diag(div) <- -sharesDown
    div <- as.vector(div)

    vertFirms <- intersect(owner.up.pre, owner.down.pre)
    ownerDownMat <- ownerToMatrix(down, preMerger = TRUE)
    ownerBargUpVert <- ownerToMatrix(up, preMerger = TRUE)
    ownerDownMatVertical <- matrix(0, nrow = nprods, ncol = nprods)

    minD <- function(theta) {
      alpha <- theta[1]
      b <- theta[-1]
      b <- b[as.numeric(id)]
      b[owner.up.pre == owner.down.pre] <- 1

      for (v in vertFirms) {
        vertrows <- owner.up.pre != v & owner.down.pre == v
        ownerBargUpVert[vertrows, owner.up.pre == v] <-
          -(1 - b[vertrows]) / b[vertrows]
      }
      for (n in which(owner.up.pre == owner.down.pre)) {
        ownerBargUpVert[n, -n] <- ownerBargUpVert[-n, n] <- 0
      }

      ownerBargDownVert <- ownerDownMat * (1 - b) / b
      for (v in vertFirms) {
        vertrows <- owner.up.pre == v & owner.down.pre != v
        if (!is2nd) ownerDownMatVertical[owner.down.pre == v, vertrows] <- 1
        ownerBargDownVert[vertrows, owner.down.pre == v] <- -1
      }

      down@ownerPre <- ownerDownMat
      if (is2nd) {
        mval <- log(sharesDown) - log(idxShare) - alpha * (pricesUp - idxPrice)
      } else {
        mval <- log(sharesDown) - log(idxShare) - alpha * (pricesDown - idxPrice)
      }
      down@slopes <- list(alpha = alpha, meanval = mval, sigma = sigma)
      marginsCandDown <- calcMargins(down, preMerger = TRUE, level = TRUE)
      shareCandDown <- calcShares(down, preMerger = TRUE, revenue = FALSE)

      if (!is2nd) {
        elast <- -alpha * tcrossprod(sharesDown)
        diag(elast) <- alpha * sharesDown + diag(elast)
        elast.inv <- try(solve(ownerDownMat * elast), silent = TRUE)
        if (any(class(elast.inv) == "try-error")) {
          elast.inv <- MASS::ginv(ownerDownMat * elast)
        }
        marginsCandDown <- marginsCandDown -
          elast.inv %*% ((ownerDownMatVertical * elast) %*% marginsUp)
      }

      depVar <- as.vector((ownerBargUpVert * div) %*% marginsUp)
      regressor <- as.vector((ownerBargDownVert * div) %*% marginsCandDown)
      err <- c(depVar - regressor, marginsDown - marginsCandDown,
        sharesDown - shareCandDown)
      sum(err^2, na.rm = TRUE)
    }

    lowerB <- rep(0.01, length(parmStart))
    lowerB[1] <- -1e9
    upperB <- rep(0.99, length(parmStart))
    upperB[1] <- -1e-9
    thetaOpt <- BB::BBoptim(
      parmStart, minD, lower = lowerB, upper = upperB,
      control = object@control.slopes, quiet = TRUE
    )
    if (thetaOpt$convergence != 0) {
      warning("Calibration routine may not have converged. Optimizer Reports:\n\t",
        thetaOpt$message)
    }

    alphaOpt <- thetaOpt$par[1]
    if (!is2nd) {
      mvalOpt <- log(sharesDown) - log(idxShare) -
        alphaOpt * (pricesDown - idxPrice)
    } else {
      mvalOpt <- log(sharesDown) - log(idxShare) -
        alphaOpt * (pricesUp - idxPrice)
    }
    bOpt <- thetaOpt$par[-1]
    bargparmPre <- bargparmPost <- bOpt[as.numeric(id)]
    bargparmPre[owner.up.pre == owner.down.pre] <- 1
    names(bargparmPre) <- down@labels
    bargparmPost[owner.up.post == owner.down.post] <- 1
    names(bargparmPost) <- down@labels

    down@slopes <- list(alpha = alphaOpt, meanval = mvalOpt, sigma = sigma)
    down@mktSize <- down@insideSize / down@shareInside
    object@down <- down
    up@bargpowerPre <- bargparmPre
    up@bargpowerPost <- bargparmPost
    object@up <- up
    object <- ownerToMatrix(object, preMerger = TRUE)
    object <- ownerToMatrix(object, preMerger = FALSE)
    object
  }
)

#' @rdname vertical-methods
#' @export
setMethod(
  f = "getParms",
  signature = "VertBargBertLogit",
  definition = function(object, digits = 10) {
    up <- object@up
    down <- object@down
    if (is.list(down@slopes)) {
      result <- lapply(down@slopes, round, digits = digits)
    } else {
      result <- list(
        slopes = round(down@slopes, digits),
        intercepts = round(down@intercepts, digits)
      )
    }
    mcPre <- calcMC(object, preMerger = TRUE)
    result$mcUpPre <- round(mcPre$up, digits)
    result$mcDownPre <- round(mcPre$down, digits)
    result$bargpower <- round(up@bargpowerPre, digits)
    result
  }
)

#' @rdname vertical-methods
#' @export
setMethod(
  f = "getNestsParms",
  signature = "VertBargBertLogitNests",
  definition = function(object) getNestsParms(object@down)
)

#' @rdname vertical-methods
#' @export
setMethod(
  f = "getNestsParms",
  signature = "VertBarg2ndLogitNests",
  definition = function(object) getNestsParms(object@down)
)

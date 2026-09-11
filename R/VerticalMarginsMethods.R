# Vertical equilibrium margin methods.

#' @rdname vertical-methods
#' @export
setMethod(
  f = "calcMargins",
  signature = "VertBargBertLogit",
  definition = function(object, preMerger = TRUE, level = FALSE) {
    is2nd <- any(grepl("2nd", class(object)))
    up <- object@up
    down <- object@down
    alpha <- down@slopes$alpha
    chain_level <- object@chain_level
    preMerger_chain <- preMerger
    if (chain_level == "wholesaler") {
      preMerger_chain <- TRUE
      if (!preMerger) down@ownerPre <- ownerToMatrix(down, preMerger = TRUE)
    }

    ownerUp <- ownerToMatrix(up, preMerger = preMerger)
    ownerDown <- ownerToMatrix(down, preMerger = preMerger)
    nprods <- nrow(ownerDown)
    if (preMerger) {
      bargparm <- up@bargpowerPre
      priceUp <- up@pricePre
      priceDown <- down@pricePre
      ownerDownLambda <- object@ownerDownLambdaPre
      ownerUpLambda <- object@ownerUpLambdaPre
      ownerVDown <- object@ownerDownPre
      down@ownerPre <- ownerDown
    } else {
      bargparm <- up@bargpowerPost
      priceUp <- up@pricePost
      priceDown <- down@pricePost
      ownerDownLambda <- object@ownerDownLambdaPost
      ownerUpLambda <- object@ownerUpLambdaPost
      ownerVDown <- object@ownerDownPost
      down@ownerPost <- ownerDown
    }

    shareDown <- calcShares(object, preMerger = preMerger, revenue = FALSE)
    elast <- -alpha * tcrossprod(shareDown)
    diag(elast) <- alpha * shareDown + diag(elast)
    elast.inv <- try(solve(ownerDown * elast), silent = TRUE)
    if (any(class(elast.inv) == "try-error")) {
      elast.inv <- MASS::ginv(ownerDown * elast)
    }

    if (is2nd) {
      alpha <- down@slopes$alpha
      down@slopes$meanval <- down@slopes$meanval +
        alpha * (priceUp - down@priceOutside)
    }
    marginsDown <- calcMargins(down, preMerger = preMerger_chain, level = TRUE)

    div <- tcrossprod(1 / (1 - shareDown), shareDown) * shareDown
    diag(div) <- -shareDown
    div <- as.vector(div)

    if (chain_level == "retailer" && !preMerger) {
      marginsUp <- up@pricePre - up@mcPost
    } else {
      marginsUpPart <- try(
        solve(ownerUpLambda * div) %*% (ownerDownLambda * div),
        silent = TRUE
      )
      if (any(class(marginsUpPart) == "try-error")) {
        marginsUpPart <- MASS::ginv(ownerUpLambda * div) %*%
          (ownerDownLambda * div)
      }
      marginsUp <- try(
        solve(diag(nprods) +
          (marginsUpPart %*% elast.inv %*% (ownerVDown * elast))),
        silent = TRUE
      )
      if (any(class(marginsUp) == "try-error")) {
        marginsUp <- MASS::ginv(diag(nprods) +
          (marginsUpPart %*% elast.inv %*% (ownerVDown * elast)))
      }
      marginsUp <- drop(marginsUp %*% marginsUpPart %*% marginsDown)
    }

    if (!is2nd) {
      if (chain_level == "wholesaler" && !preMerger) {
        marginsDown <- down@pricePre - down@mcPost - priceUp
      } else {
        marginsDown <- marginsDown -
          elast.inv %*% ((ownerVDown * elast) %*% marginsUp)
      }
    }
    if (!level) {
      marginsUp <- marginsUp / priceUp
      marginsDown <- marginsDown / priceDown
    }
    names(marginsUp) <- up@labels
    names(marginsDown) <- down@labels
    list(up = as.vector(marginsUp), down = as.vector(marginsDown))
  }
)

#' @rdname vertical-methods
#' @export
setMethod(
  f = "calcMargins",
  signature = "VertBargBertLogitNests",
  definition = function(object, preMerger = TRUE, level = FALSE) {
    is2nd <- any(grepl("2nd", class(object)))
    up <- object@up
    down <- object@down
    alpha <- down@slopes$alpha
    nestParm <- 1 - down@slopes$sigma
    nests <- droplevels(down@nests)
    nestMat <- tcrossprod(model.matrix(~ -1 + nests))
    chain_level <- object@chain_level
    preMerger_chain <- preMerger
    if (chain_level == "wholesaler") {
      preMerger_chain <- TRUE
      if (!preMerger) down@ownerPre <- ownerToMatrix(down, preMerger = TRUE)
    }
    ownerUp <- ownerToMatrix(up, preMerger = preMerger)
    ownerDown <- ownerToMatrix(down, preMerger = preMerger)
    nprods <- nrow(ownerDown)
    if (preMerger) {
      bargparm <- up@bargpowerPre
      priceUp <- up@pricePre
      priceDown <- down@pricePre
      ownerDownLambda <- object@ownerDownLambdaPre
      ownerUpLambda <- object@ownerUpLambdaPre
      ownerVDown <- object@ownerDownPre
      down@ownerPre <- ownerDown
    } else {
      bargparm <- up@bargpowerPost
      priceUp <- up@pricePost
      priceDown <- down@pricePost
      ownerDownLambda <- object@ownerDownLambdaPost
      ownerUpLambda <- object@ownerUpLambdaPost
      ownerVDown <- object@ownerDownPost
      down@ownerPost <- ownerDown
    }

    shareDown <- calcShares(object, preMerger = preMerger, revenue = FALSE)
    shareBetweenDown <- as.vector(unname(tapply(shareDown, nests, sum)[nests]))
    shareWithinDown <- shareDown / shareBetweenDown
    elastDiff <- -alpha * tcrossprod(shareDown)
    elastSame <- -alpha * tcrossprod(
      shareDown * (1 + nestParm[nests] / (1 - nestParm[nests]) *
        (1 / shareBetweenDown)), shareDown
    )
    elast <- elastSame * nestMat + elastDiff * (1 - nestMat)
    diag(elast) <- alpha / (1 - nestParm[nests]) * shareDown *
      (1 - nestParm[nests] * shareWithinDown -
        (1 - nestParm[nests]) * shareDown)
    elast.inv <- try(solve(ownerDown * elast), silent = TRUE)
    if (any(class(elast.inv) == "try-error")) {
      elast.inv <- MASS::ginv(ownerDown * elast)
    }
    marginsDown <- -as.vector(elast.inv %*% shareDown)

    divSameNest <- (1 - shareWithinDown)^nestParm[nests] *
      (1 - shareBetweenDown + shareBetweenDown *
        (1 - shareWithinDown)^(1 - nestParm[nests]))
    divSameNest <- tcrossprod((1 / divSameNest - 1), shareDown)
    divDiffNest <- shareBetweenDown *
      (1 - (1 - shareWithinDown)^(1 - nestParm[nests]))
    divDiffNest <- tcrossprod(1 / (1 / divDiffNest - 1), shareDown)
    div <- divSameNest * nestMat + divDiffNest * (1 - nestMat)
    diag(div) <- -shareDown
    div[is.na(div)] <- 0

    if (chain_level == "retailer" && !preMerger) {
      marginsUp <- up@pricePre - up@mcPost
    } else {
      marginsUpPart <- try(
        solve(ownerUpLambda * div) %*% (ownerDownLambda * div),
        silent = TRUE
      )
      if (any(class(marginsUpPart) == "try-error")) {
        marginsUpPart <- MASS::ginv(ownerUpLambda * div) %*%
          (ownerDownLambda * div)
      }
      marginsUp <- try(
        solve(diag(nprods) +
          (marginsUpPart %*% elast.inv %*% (ownerVDown * elast))),
        silent = TRUE
      )
      if (any(class(marginsUp) == "try-error")) {
        marginsUp <- MASS::ginv(diag(nprods) +
          (marginsUpPart %*% elast.inv %*% (ownerVDown * elast)))
      }
      marginsUp <- drop(marginsUp %*% marginsUpPart %*% marginsDown)
    }

    if (!is2nd) {
      if (chain_level == "wholesaler" && !preMerger) {
        marginsDown <- down@pricePre - down@mcPost - priceUp
      } else {
        marginsDown <- marginsDown -
          elast.inv %*% ((ownerVDown * elast) %*% marginsUp)
      }
    }
    if (!level) {
      marginsUp <- marginsUp / priceUp
      marginsDown <- marginsDown / priceDown
    }
    names(marginsUp) <- up@labels
    names(marginsDown) <- down@labels
    list(up = as.vector(marginsUp), down = as.vector(marginsDown))
  }
)

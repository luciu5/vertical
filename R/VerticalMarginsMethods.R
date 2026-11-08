# Vertical equilibrium margin methods.

#' @rdname vertical-methods
#' @export
setMethod(
  f = "calcMargins",
  signature = "VertBargBertLogit",
  definition = function(object, preMerger = TRUE, level = FALSE) {
    if (!is.null(.vertical_policy_state(object))) {
      return(.vertical_policy_margins(object, preMerger, level))
    }
    if (preMerger && any(!.vertical_active_subset(object, TRUE))) {
      # This pre-state is a promoted, already solved market. Evaluate its
      # physical profit identity directly, excluding links absent there,
      # rather than asking an inner all-products pre-state to price NA goods.
      active <- .vertical_active_subset(object, TRUE)
      up <- object@up@pricePre - object@up@mcPre
      down <- object@down@pricePre - object@up@pricePre - object@down@mcPre
      if (!level) {
        up <- up / object@up@pricePre
        down <- down / object@down@pricePre
      }
      up[!active] <- down[!active] <- NA_real_
      return(list(up = as.vector(up), down = as.vector(down)))
    }
    is2nd <- any(grepl("2nd", class(object)))
    up <- object@up
    down <- object@down
    alpha <- down@slopes$alpha
    chain_level <- object@chain_level
    active <- .vertical_active_subset(object, preMerger)
    n_total <- length(active)
    preMerger_chain <- preMerger
    if (chain_level == "wholesaler") {
      preMerger_chain <- TRUE
      if (!preMerger) down@ownerPre <- ownerToMatrix(down, preMerger = TRUE)
    }

    ownerDownFull <- ownerToMatrix(down, preMerger = preMerger)
    ownerDown <- ownerDownFull[active, active, drop = FALSE]
    nprods <- sum(active)
    if (preMerger) {
      priceUp <- up@pricePre[active]
      priceDown <- down@pricePre[active]
      ownerDownLambda <- object@ownerDownLambdaPre[
        active, active, drop = FALSE]
      ownerUpLambda <- object@ownerUpLambdaPre[
        active, active, drop = FALSE]
      ownerVDown <- object@ownerDownPre[active, active, drop = FALSE]
      down@ownerPre <- ownerDownFull
    } else {
      priceUp <- up@pricePost[active]
      priceDown <- down@pricePost[active]
      ownerDownLambda <- object@ownerDownLambdaPost[
        active, active, drop = FALSE]
      ownerUpLambda <- object@ownerUpLambdaPost[
        active, active, drop = FALSE]
      ownerVDown <- object@ownerDownPost[active, active, drop = FALSE]
      down@ownerPost <- ownerDownFull
    }

    shareDown <- calcShares(
      object, preMerger = preMerger, revenue = FALSE
    )[active]
    elast <- -alpha * tcrossprod(shareDown)
    diag(elast) <- alpha * shareDown + diag(elast)
    elast.inv <- try(solve(ownerDown * elast), silent = TRUE)
    if (any(class(elast.inv) == "try-error")) {
      elast.inv <- MASS::ginv(ownerDown * elast)
    }

    if (is2nd) {
      alpha <- down@slopes$alpha
      meanval <- down@slopes$meanval
      meanval[active] <- meanval[active] +
        alpha * (priceUp - down@priceOutside)
      down@slopes$meanval <- meanval
    }
    marginsDown <- calcMargins(
      down, preMerger = preMerger_chain, level = TRUE
    )[active]

    div <- tcrossprod(1 / (1 - shareDown), shareDown) * shareDown
    diag(div) <- -shareDown
    div <- as.vector(div)

    if (chain_level == "retailer" && !preMerger) {
      marginsUp <- (up@pricePre - up@mcPost)[active]
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
        marginsDown <- (down@pricePre - down@mcPost)[active] - priceUp
      } else {
        marginsDown <- marginsDown -
          elast.inv %*% ((ownerVDown * elast) %*% marginsUp)
      }
    }
    if (!level) {
      marginsUp <- marginsUp / priceUp
      marginsDown <- marginsDown / priceDown
    }
    resultUp <- rep(NA_real_, n_total)
    resultDown <- rep(NA_real_, n_total)
    resultUp[active] <- as.vector(marginsUp)
    resultDown[active] <- as.vector(marginsDown)
    names(resultUp) <- up@labels
    names(resultDown) <- down@labels
    list(up = as.vector(resultUp), down = as.vector(resultDown))
  }
)

#' @rdname vertical-methods
#' @export
setMethod(
  f = "calcMargins",
  signature = "VertBargBertLogitNests",
  definition = function(object, preMerger = TRUE, level = FALSE) {
    if (!is.null(.vertical_policy_state(object))) {
      return(.vertical_policy_margins(object, preMerger, level))
    }
    if (preMerger && any(!.vertical_active_subset(object, TRUE))) {
      # This pre-state is a promoted, already solved market. Evaluate its
      # physical profit identity directly, excluding links absent there,
      # rather than asking an inner all-products pre-state to price NA goods.
      active <- .vertical_active_subset(object, TRUE)
      up <- object@up@pricePre - object@up@mcPre
      down <- object@down@pricePre - object@up@pricePre - object@down@mcPre
      if (!level) {
        up <- up / object@up@pricePre
        down <- down / object@down@pricePre
      }
      up[!active] <- down[!active] <- NA_real_
      return(list(up = as.vector(up), down = as.vector(down)))
    }
    is2nd <- any(grepl("2nd", class(object)))
    up <- object@up
    down <- object@down
    alpha <- down@slopes$alpha
    nestParm <- 1 - down@slopes$sigma
    active <- .vertical_active_subset(object, preMerger)
    n_total <- length(active)
    nests <- droplevels(down@nests[active])
    nestParmActive <- unname(nestParm[as.character(nests)])
    nestMat <- 1 * outer(
      as.character(nests), as.character(nests), FUN = "=="
    )
    chain_level <- object@chain_level
    preMerger_chain <- preMerger
    if (chain_level == "wholesaler") {
      preMerger_chain <- TRUE
      if (!preMerger) down@ownerPre <- ownerToMatrix(down, preMerger = TRUE)
    }
    ownerDownFull <- ownerToMatrix(down, preMerger = preMerger)
    ownerDown <- ownerDownFull[active, active, drop = FALSE]
    nprods <- sum(active)
    if (preMerger) {
      priceUp <- up@pricePre[active]
      priceDown <- down@pricePre[active]
      ownerDownLambda <- object@ownerDownLambdaPre[
        active, active, drop = FALSE]
      ownerUpLambda <- object@ownerUpLambdaPre[
        active, active, drop = FALSE]
      ownerVDown <- object@ownerDownPre[active, active, drop = FALSE]
      down@ownerPre <- ownerDownFull
    } else {
      priceUp <- up@pricePost[active]
      priceDown <- down@pricePost[active]
      ownerDownLambda <- object@ownerDownLambdaPost[
        active, active, drop = FALSE]
      ownerUpLambda <- object@ownerUpLambdaPost[
        active, active, drop = FALSE]
      ownerVDown <- object@ownerDownPost[active, active, drop = FALSE]
      down@ownerPost <- ownerDownFull
    }

    shareDown <- calcShares(
      object, preMerger = preMerger, revenue = FALSE
    )[active]
    shareBetweenDown <- as.vector(unname(
      tapply(shareDown, nests, sum)[as.character(nests)]
    ))
    shareWithinDown <- shareDown / shareBetweenDown
    elastDiff <- -alpha * tcrossprod(shareDown)
    elastSame <- -alpha * tcrossprod(
      shareDown * (1 + nestParmActive / (1 - nestParmActive) *
        (1 / shareBetweenDown)), shareDown
    )
    elast <- elastSame * nestMat + elastDiff * (1 - nestMat)
    diag(elast) <- alpha / (1 - nestParmActive) * shareDown *
      (1 - nestParmActive * shareWithinDown -
        (1 - nestParmActive) * shareDown)
    elast.inv <- try(solve(ownerDown * elast), silent = TRUE)
    if (any(class(elast.inv) == "try-error")) {
      elast.inv <- MASS::ginv(ownerDown * elast)
    }
    marginsDown <- -as.vector(elast.inv %*% shareDown)

    divSameNest <- (1 - shareWithinDown)^nestParmActive *
      (1 - shareBetweenDown + shareBetweenDown *
        (1 - shareWithinDown)^(1 - nestParmActive))
    divSameNest <- tcrossprod((1 / divSameNest - 1), shareDown)
    divDiffNest <- shareBetweenDown *
      (1 - (1 - shareWithinDown)^(1 - nestParmActive))
    divDiffNest <- tcrossprod(1 / (1 / divDiffNest - 1), shareDown)
    div <- divSameNest * nestMat + divDiffNest * (1 - nestMat)
    diag(div) <- -shareDown
    div[is.na(div)] <- 0

    if (chain_level == "retailer" && !preMerger) {
      marginsUp <- (up@pricePre - up@mcPost)[active]
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
        marginsDown <- (down@pricePre - down@mcPost)[active] - priceUp
      } else {
        marginsDown <- marginsDown -
          elast.inv %*% ((ownerVDown * elast) %*% marginsUp)
      }
    }
    if (!level) {
      marginsUp <- marginsUp / priceUp
      marginsDown <- marginsDown / priceDown
    }
    resultUp <- rep(NA_real_, n_total)
    resultDown <- rep(NA_real_, n_total)
    resultUp[active] <- as.vector(marginsUp)
    resultDown[active] <- as.vector(marginsDown)
    names(resultUp) <- up@labels
    names(resultDown) <- down@labels
    list(up = as.vector(resultUp), down = as.vector(resultDown))
  }
)

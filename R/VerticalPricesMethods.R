# Vertical equilibrium price methods.

.vertical_second_score_foc <- function(object, priceCand,
                                       preMerger = TRUE) {
  up <- object@up
  down <- object@down
  active <- .vertical_active_subset(object, preMerger)
  n_active <- sum(active)
  if (!is.numeric(priceCand) || length(priceCand) != 2L * n_active) {
    stop("second-score price candidates must contain upstream and downstream prices")
  }

  price_up <- priceCand[seq_len(n_active)]
  price_down <- priceCand[n_active + seq_len(n_active)]
  owner_down <- ownerToMatrix(down, preMerger = preMerger)
  if (preMerger) {
    down@ownerPre <- owner_down
    mc_up <- up@mcPre[active]
    mc_down <- down@mcPre[active]
    owner_down_lambda <- object@ownerDownLambdaPre[active, active,
      drop = FALSE]
    owner_up_lambda <- object@ownerUpLambdaPre[active, active, drop = FALSE]
  } else {
    down@ownerPost <- owner_down
    mc_up <- up@mcPost[active]
    mc_down <- down@mcPost[active]
    owner_down_lambda <- object@ownerDownLambdaPost[active, active,
      drop = FALSE]
    owner_up_lambda <- object@ownerUpLambdaPost[active, active, drop = FALSE]
  }

  candidate_down <- down
  candidate_slopes <- candidate_down@slopes
  candidate_slopes$meanval[active] <- candidate_down@slopes$meanval[active] +
    candidate_down@slopes$alpha * (price_up - candidate_down@priceOutside)
  candidate_down@slopes <- candidate_slopes
  candidate_prices <- rep(NA_real_, length(candidate_down@prices))
  candidate_prices[active] <- price_down
  if (preMerger) {
    candidate_down@pricePre <- candidate_prices
  } else {
    candidate_down@pricePost <- candidate_prices
  }
  shares <- calcShares(
    candidate_down, preMerger = preMerger, revenue = FALSE
  )[active]
  margins_down <- calcMargins(
    candidate_down, preMerger = preMerger, level = TRUE
  )[active]
  diversion <- tcrossprod(1 / (1 - shares), shares) * shares
  diag(diversion) <- -shares
  diversion <- as.vector(diversion)

  upstream <- (owner_up_lambda * diversion) %*% (price_up - mc_up) -
    (owner_down_lambda * diversion) %*% margins_down
  downstream <- price_down - price_up - mc_down - margins_down
  c(downstream, as.vector(upstream))
}

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
    subsetDown <- .vertical_active_subset(object, preMerger)
    priceStartUp <- up@priceStart
    priceStartDown <- down@priceStart
    if (chain_level == "full") {
      priceStart <- c(priceStartUp[subsetDown], priceStartDown[subsetDown])
    } else if (chain_level == "retailer") {
      priceStart <- priceStartDown[subsetDown]
    } else if (chain_level == "wholesaler") {
      priceStart <- priceStartUp[subsetDown]
    }
    if (preMerger) {
      mcUp <- up@mcPre[subsetDown]
      mcDown <- down@mcPre[subsetDown]
    } else {
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
      priceCandUpFull <- rep(NA_real_, length(up@prices))
      priceCandDownFull <- rep(NA_real_, length(down@prices))
      priceCandUpFull[subsetDown] <- priceCandUp
      priceCandDownFull[subsetDown] <- priceCandDown
      if (preMerger) {
        up@pricePre <- priceCandUpFull
        down@pricePre <- priceCandDownFull
      } else {
        up@pricePost <- priceCandUpFull
        down@pricePost <- priceCandDownFull
      }
      thisobj@up <- up
      thisobj@down <- down
      theseMargins <- calcMargins(thisobj, preMerger = preMerger, level = TRUE)
      upFOC <- priceCandUp - mcUp - theseMargins$up[subsetDown]
      downFOC <- priceCandDown - priceCandUp - mcDown -
        theseMargins$down[subsetDown]
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
    subsetDown <- .vertical_active_subset(object, preMerger)
    priceStartUp <- up@priceStart
    priceStartDown <- down@priceStart
    priceStart <- c(priceStartUp[subsetDown], priceStartDown[subsetDown])

    FOC <- function(priceCand) {
      .vertical_second_score_foc(object, priceCand,
        preMerger = preMerger)
    }

    nleqslv_maxit <- as.integer(down@control.equ$maxit)
    if (length(nleqslv_maxit) == 0 || is.na(nleqslv_maxit[1]) ||
        nleqslv_maxit[1] < 1) nleqslv_maxit <- 150L
    nleqslv_ftol <- suppressWarnings(as.numeric(down@control.equ$tol)[1])
    if (length(nleqslv_ftol) == 0 || !is.finite(nleqslv_ftol) ||
        nleqslv_ftol <= 0) nleqslv_ftol <- 1e-10
    residual_tol <- min(1e-7, max(nleqslv_ftol, sqrt(.Machine$double.eps)))

    candidate_diagnostics <- function(candidate) {
      if (!is.numeric(candidate) || length(candidate) != length(priceStart) ||
          any(!is.finite(candidate))) {
        return(list(ok = FALSE, residual = Inf))
      }
      residual <- tryCatch(FOC(candidate), error = function(e) NULL)
      if (!is.numeric(residual) || length(residual) != length(priceStart) ||
          any(!is.finite(residual))) {
        return(list(ok = FALSE, residual = Inf))
      }
      residual <- max(abs(residual))
      list(ok = is.finite(residual) && residual <= residual_tol,
        residual = residual)
    }

    solver_description <- function(result, candidate, solver_name) {
      diagnostics <- candidate_diagnostics(candidate)
      status <- if (is.null(result)) {
        "error"
      } else if (!is.null(result$error)) {
        paste0("error: ", result$error)
      } else if (!is.null(result$termcd)) {
        as.character(result$termcd)
      } else if (!is.null(result$convergence)) {
        as.character(result$convergence)
      } else {
        "unknown"
      }
      paste0(solver_name, " status=", status,
        ", max |FOC|=", format(diagnostics$residual, digits = 6,
          scientific = TRUE))
    }

    nleq_attempt <- function(start, method, global = NULL,
                            allow_singular = FALSE) {
      args <- list(
        x = start, fn = FOC, method = method,
        control = list(
          ftol = min(nleqslv_ftol, 1e-10),
          maxit = nleqslv_maxit,
          allowSingular = allow_singular
        )
      )
      if (!is.null(global)) args$global <- global
      tryCatch(
        do.call(nleqslv::nleqslv, args),
        error = function(e) list(error = conditionMessage(e))
      )
    }

    minResult <- nleq_attempt(priceStart, "Broyden")
    priceResult <- minResult$x
    priceDiagnostics <- candidate_diagnostics(priceResult)
    attempts <- list(list(
      name = "Broyden", result = minResult, candidate = priceResult
    ))

    if (!priceDiagnostics$ok) {
      bbResult <- tryCatch(
        BB::BBsolve(priceStart, FOC, quiet = TRUE,
          control = down@control.equ, ...),
        error = function(e) list(error = conditionMessage(e))
      )
      bbCandidate <- bbResult$par
      bbDiagnostics <- candidate_diagnostics(bbCandidate)
      attempts[[length(attempts) + 1L]] <- list(
        name = "BBsolve", result = bbResult, candidate = bbCandidate
      )
      if (bbDiagnostics$ok) {
        priceResult <- bbCandidate
        priceDiagnostics <- bbDiagnostics
        if (!is.null(bbResult$convergence) && bbResult$convergence != 0) {
          warning("'calcPrices' nonlinear solver may not have successfully converged. 'BBsolve' reports: '",
            bbResult$message, "'")
        }
      }
    }

    if (!priceDiagnostics$ok) {
      fallback_starts <- list(priceStart)
      fallback_names <- "Newton"
      if (isTRUE(.vertical_classify_model(object)$hybrid)) {
        fallback_starts <- c(
          fallback_starts,
          lapply(c(0.75, 0.5, 0.33), function(scale) priceStart * scale)
        )
        fallback_names <- c(
          fallback_names,
          paste0("Broyden(scale=", c(0.75, 0.5, 0.33), ")")
        )
      }
      fallback_results <- vector("list", length(fallback_starts))
      for (i in seq_along(fallback_starts)) {
        fallback_results[[i]] <- if (i == 1L) {
          nleq_attempt(fallback_starts[[i]], "Newton", global = "gline")
        } else {
          nleq_attempt(fallback_starts[[i]], "Broyden",
            global = "dbldog", allow_singular = TRUE)
        }
        candidate <- fallback_results[[i]]$x
        attempts[[length(attempts) + 1L]] <- list(
          name = fallback_names[[i]], result = fallback_results[[i]],
          candidate = candidate
        )
      }
      valid <- vapply(attempts, function(attempt) {
        candidate_diagnostics(attempt$candidate)$ok
      }, logical(1))
      if (!any(valid)) {
        descriptions <- vapply(attempts, function(attempt) {
          solver_description(
            attempt$result, attempt$candidate, attempt$name
          )
        }, character(1))
        stop("'calcPrices' second-score nonlinear solver failed: ",
          paste(descriptions, collapse = "; "),
          "; accepted max |FOC| threshold=",
          format(residual_tol, digits = 6, scientific = TRUE))
      }
      valid_attempts <- attempts[valid]
      distances <- vapply(valid_attempts, function(attempt) {
        sum(((attempt$candidate - priceStart) /
          pmax(abs(priceStart), 1))^2)
      }, numeric(1))
      selected <- valid_attempts[[which.min(distances)]]
      priceResult <- selected$candidate
      priceDiagnostics <- candidate_diagnostics(priceResult)
    } else if (!is.null(minResult$termcd) && minResult$termcd > 1) {
      warning("'calcPrices' may not have fully converged. 'nleqslv' termcd: ",
        minResult$termcd)
    }
    minResultUp <- rep(NA, length(priceStartUp))
    minResultDown <- rep(NA, length(priceStartDown))
    minResultUp[subsetDown] <- priceResult[seq_along(priceStartUp[subsetDown])]
    minResultDown[subsetDown] <- priceResult[-seq_along(priceStartUp[subsetDown])]
    list(up = minResultUp, down = minResultDown)
  }
)

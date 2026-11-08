# Vertical simulation owns only the vertical ownership, cost, subset, and
# sequential-state transitions. Equilibrium calculations dispatch through
# antitrust generics and the vertical economic methods in this package.

.vertical_exit_subset <- function(model, exit = NULL) {
  n <- length(model@down@prices)
  active <- model@down@subset
  if (is.null(exit)) return(active)
  if (is.logical(exit)) {
    if (length(exit) != n || anyNA(exit)) stop("'exit' must be a length-k logical active-product vector without NAs")
    keep <- exit
  } else {
    if (is.character(exit)) {
      if (anyNA(exit) || any(!exit %in% model@down@labels))
        stop("character 'exit' values must match fitted product labels")
      exit <- match(exit, model@down@labels)
    }
    if (!is.numeric(exit) || anyNA(exit) || any(!is.finite(exit)) ||
        any(exit < 1 | exit > n | exit != as.integer(exit)))
      stop("'exit' must contain valid product indices or labels")
    keep <- rep(TRUE, n)
    keep[as.integer(exit)] <- FALSE
  }
  active <- active & keep
  if (!any(active)) stop("'exit' must retain at least one active linked product")
  active
}

.vertical_validate_owner <- function(model, ownerPost) {
  n_up <- length(model@up@prices)
  n_down <- length(model@down@prices)
  if (!is.list(ownerPost) || !all(c("up", "down") %in% names(ownerPost)) ||
      is.matrix(ownerPost$up) || is.matrix(ownerPost$down)) {
    stop("For vertical bargaining, 'ownerPost' must be a list with 'up' and 'down' ownership vectors.")
  }
  if (length(ownerPost$up) != n_up || length(ownerPost$down) != n_down) {
    stop("Vertical 'ownerPost' ownership vectors must match the fitted upstream and downstream markets.")
  }
  ownerPost
}

.vertical_validate_costs <- function(model, mcDelta) {
  if (is.null(mcDelta)) {
    return(list(up = rep(0, length(model@up@prices)),
                down = rep(0, length(model@down@prices))))
  }
  if (!is.list(mcDelta) || !all(c("up", "down") %in% names(mcDelta)) ||
      !is.numeric(mcDelta$up) || !is.numeric(mcDelta$down) ||
      length(mcDelta$up) != length(model@up@prices) ||
      length(mcDelta$down) != length(model@down@prices) ||
      anyNA(mcDelta$up) || anyNA(mcDelta$down)) {
    stop("For vertical bargaining, 'mcDelta' must be a list with numeric 'up' and 'down' vectors matching the fitted markets and no NAs.")
  }
  mcDelta
}

.vertical_refresh_flags <- function(model) {
  classification <- .vertical_classify_model(model)
  model@isHorizontal <- classification$horizontal
  model@isUpstream <- classification$upstream_horizontal
  model
}

.vertical_simulate_model <- function(model, ownerPost = NULL, mcDelta = NULL,
                                     subset = NULL, dots = list()) {
  .vertical_assert_model(model)
  if (is.null(ownerPost)) {
    ownerPost <- list(up = model@up@ownerPre, down = model@down@ownerPre)
  }
  ownerPost <- .vertical_validate_owner(model, ownerPost)
  mcDelta <- .vertical_validate_costs(model, mcDelta)
  if (any(mcDelta$up > 0, na.rm = TRUE) ||
      any(mcDelta$down > 0, na.rm = TRUE)) {
    warning("positive values of 'mcDelta' imply an INCREASE in marginal costs")
  }
  if (!is.null(subset) &&
      (!is.logical(subset) ||
       length(subset) != length(model@down@prices) ||
       anyNA(subset) || !any(subset))) {
    stop("'subset' must be a logical vector the same length as the fitted prices with at least one TRUE value.")
  }
  if (is.null(subset)) subset <- model@down@subset
  subset <- model@down@subset & subset
  if (!any(subset)) {
    stop("'subset' must retain at least one product already active in the fitted model.")
  }

  model@up@ownerPost <- ownerPost$up
  model@down@ownerPost <- ownerPost$down
  model@up@mcDelta <- mcDelta$up
  model@down@mcDelta <- mcDelta$down
  model@down@subset <- subset
  model@up@bargpowerPost <- model@up@bargpowerPre
  integrated <- model@up@ownerPost == model@down@ownerPost
  model@up@bargpowerPost[integrated] <- 1
  model <- .vertical_refresh_flags(model)
  model <- ownerToMatrix(model, preMerger = FALSE)

  costs <- calcMC(model, preMerger = FALSE)
  model@up@mcPost <- costs$up
  model@down@mcPost <- costs$down
  if (is.null(.vertical_policy_state(model))) {
    prices <- do.call(calcPrices, c(list(model, preMerger = FALSE), dots))
  } else {
    prices <- do.call(.vertical_policy_solve,
      c(list(model, preMerger = FALSE), dots))
    policy <- .vertical_policy_state(model)
    policy$shadowPost <- prices$shadow
    policy$solver <- prices$solver
    attr(model, "vertical_trade_policy") <- policy
  }
  model@up@pricePost <- prices$up
  model@down@pricePost <- prices$down
  model
}

.vertical_promote <- function(model) {
  .vertical_assert_model(model)
  attr(model, "vertical_pre_subset") <- model@down@subset
  model@up@ownerPre <- model@up@ownerPost
  model@down@ownerPre <- model@down@ownerPost
  model@up@bargpowerPre <- model@up@bargpowerPost
  model@down@pricePre <- model@down@pricePost
  model@up@pricePre <- model@up@pricePost
  model@down@mcPre <- model@down@mcPost
  model@up@mcPre <- model@up@mcPost
  model <- ownerToMatrix(model, preMerger = TRUE)
  .vertical_policy_promote(model)
}

.vertical_step_costs <- function(model, cumulative, change) {
  if (is.null(change)) return(cumulative)
  change <- .vertical_validate_costs(model, change)
  current_up <- if (is.null(cumulative)) NULL else cumulative$up
  current_down <- if (is.null(cumulative)) NULL else cumulative$down
  list(
    up = antitrust::compound_cost_shocks(model@up, current_up, change$up),
    down = antitrust::compound_cost_shocks(model@down, current_down, change$down)
  )
}

.vertical_solve_steps <- function(object, state, steps, cumulative = NULL,
                                  dots = list()) {
  results <- vector("list", length(steps))
  for (i in seq_along(steps)) {
    changes <- steps[[i]]@changes
    if (!is.null(changes$costs)) {
      if (!is.null(cumulative) && !is.null(changes$costs)) {
        ## Validation occurs in .vertical_step_costs; retain this explicit
        ## branch so malformed first-step costs are not silently ignored.
        invisible(.vertical_validate_costs(state, changes$costs))
      }
      cumulative <- .vertical_step_costs(state, cumulative, changes$costs)
    }
    result <- .vertical_simulate_model(
      state, ownerPost = changes$ownership, mcDelta = cumulative,
      subset = .vertical_exit_subset(state, changes$exit), dots = dots
    )
    result <- .vertical_attach_merger_metadata(result)
    results[[i]] <- result
    state <- .vertical_promote(result)
  }
  list(results = results, cumulative_costs = cumulative)
}

#' Validate a vertical counterfactual
#'
#' Ownership, cost changes and cumulative linked-product exit have defined
#' vertical bargaining mechanics. Other fields fail explicitly.
#' @param object A calibrated `VerticalFit`.
#' @param counterfactual A `Counterfactual` created by [counterfactual()].
#' @return The validated counterfactual, invisibly.
#' @export
setMethod("validate_counterfactual", "VerticalFit",
  function(object, counterfactual) {
    if (!methods::is(counterfactual, "Counterfactual")) {
      stop("'counterfactual' must be a Counterfactual object")
    }
    state <- object@model
    for (step in counterfactual@steps) {
      state@down@subset <- .vertical_exit_subset(state, step@changes$exit)
      unsupported <- setdiff(names(step@changes), c("ownership", "costs", "exit"))
      if (length(unsupported)) {
        stop("model '", object@spec$id,
             "' does not support counterfactual field(s): ",
             paste(unsupported, collapse = ", "))
      }
    }
    invisible(counterfactual)
  }
)

#' Solve sequential vertical counterfactual steps
#'
#' @param object The originating `VerticalFit`.
#' @param last_result The final vertical equilibrium from the existing path.
#' @param steps A non-empty list of `CounterfactualStep` objects.
#' @param cumulative_costs Existing cumulative upstream and downstream cost
#'   shocks, when resuming a path.
#' @param ... Additional arguments passed to the vertical price solver.
#' @return A list containing solved results and cumulative cost shocks.
#' @export
setMethod("simulate_steps", "VerticalFit",
  function(object, last_result, steps, cumulative_costs = NULL, ...) {
    .vertical_assert_model(last_result)
    if (!is.list(steps) || !length(steps) ||
        !all(vapply(steps, methods::is, logical(1), "CounterfactualStep"))) {
      stop("'steps' must be a non-empty list of CounterfactualStep objects")
    }
    current <- object
    current@model <- last_result
    validate_counterfactual(current, new("Counterfactual", steps = steps))
    state <- .vertical_promote(last_result)
    cumulative <- cumulative_costs
    if (!is.null(cumulative)) cumulative <- .vertical_validate_costs(state, cumulative)
    .vertical_solve_steps(object, state, steps, cumulative, list(...))
  }
)

#' Simulate a vertical counterfactual
#'
#' @param object A calibrated `VerticalFit`.
#' @param ownerPost A list containing upstream and downstream ownership, or a
#'   `Counterfactual` object. Simultaneous same-tier consolidation and
#'   cross-tier integration are classified and simulated as a hybrid merger.
#' @param mcDelta A list of upstream and downstream cost changes with `up` and `down`
#'   vectors.
#' @param subset A logical vector selecting products that remain active in the
#'   post-counterfactual state. It is intersected with the model's existing
#'   active-product mask, while the pre-counterfactual state always keeps all
#'   products active. Inactive post-counterfactual products have `NA` prices,
#'   margins, and revenues. Standard and nested Logit report zero shares and
#'   quantities for them; second-score reports `NA` shares and quantities.
#' @param priceStart,solver,isMax Retained for compatibility with the antitrust
#'   lifecycle; the legacy vertical solver determines its own starting values,
#'   algorithm, and checks.
#' @param capacitiesPost,bargpowerPost Unsupported non-vertical scenario
#'   arguments that produce a deliberate error instead of being ignored.
#' @param ... Additional arguments passed to the vertical price solver.
#' @return A vertical equilibrium object for one step, or a
#'   `CounterfactualPath` for multiple steps.
#' @export
setMethod("simulate", "VerticalFit",
  function(object, ownerPost = NULL, mcDelta = NULL, subset = NULL,
           priceStart, capacitiesPost = NULL, bargpowerPost = NULL,
           solver = NULL, isMax = FALSE, ...) {
    dots <- list(...)
    if (methods::is(ownerPost, "Counterfactual")) {
      conflicts <- c(
        if (!is.null(mcDelta)) "mcDelta",
        if (!is.null(subset)) "subset",
        if (!missing(priceStart)) "priceStart",
        if (!is.null(capacitiesPost)) "capacitiesPost",
        if (!is.null(bargpowerPost)) "bargpowerPost"
      )
      if (length(conflicts)) {
        stop("cannot combine a Counterfactual with legacy scenario argument(s): ",
             paste(conflicts, collapse = ", "))
      }
      validate_counterfactual(object, ownerPost)
      if (length(ownerPost@steps) > 1L) {
        solved <- .vertical_solve_steps(
          object, object@model, ownerPost@steps, dots = dots
        )
        return(new(
          "CounterfactualPath", initial = object@model,
          steps = ownerPost@steps, results = solved$results,
          diagnostics = list(fit = object,
                             cumulative_costs = solved$cumulative_costs)
        ))
      }
      step <- ownerPost@steps[[1L]]
      changes <- step@changes
      result <- .vertical_simulate_model(
        object@model, ownerPost = changes$ownership,
        mcDelta = changes$costs,
        subset = .vertical_exit_subset(object@model, changes$exit), dots = dots
      )
      attr(result, "counterfactual") <- list(model_spec = object@spec,
                                               fields = ownerPost)
      result <- .vertical_attach_merger_metadata(result)
      return(result)
    }
    if (!is.null(capacitiesPost)) {
      stop("'capacitiesPost' is not supported for vertical bargaining fits.")
    }
    if (!is.null(bargpowerPost)) {
      stop("'bargpowerPost' is not supported for vertical bargaining fits.")
    }
    if (is.null(ownerPost)) stop("'ownerPost' must be supplied for simulate().")
    result <- .vertical_simulate_model(object@model, ownerPost, mcDelta, subset,
                                       dots = dots)
    attr(result, "counterfactual") <- list(
      model_spec = object@spec,
      fields = counterfactual(ownership = ownerPost, costs = mcDelta)
    )
    result <- .vertical_attach_merger_metadata(result)
    result
  }
)

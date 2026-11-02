# Policy accounting and constrained equilibrium belong to vertical.  The
# antitrust Counterfactual classes remain the neutral scenario interface.

#' A policy-aware fitted vertical market
#' @slot upstream_policy Neutral upstream tariff, quota, capacity and originating
#'   fitted physical quantity.
#' @slot downstream_policy Neutral downstream tariff, quota, capacity and
#'   originating fitted physical quantity.
#' @exportClass VerticalTradeFit
setClass("VerticalTradeFit", contains = "VerticalFit",
  slots = c(upstream_policy = "list", downstream_policy = "list"))

#' Enable trade counterfactuals on a fitted vertical market
#'
#' Promotion copies the fitted model without calibration or an equilibrium
#' solve. Tariffs are fractions of gross purchaser prices, with seller
#' receipts `(1 - tariff) * price`. Quotas are multiples of the originating
#' fitted physical quantity. Missing tiers default to zero tariffs and
#' infinite quotas. Policy changes use the antitrust Counterfactual API.
#' @param object A fitted `VerticalFit`.
#' @param tariffPre,quotaPre Optional named lists with `up` and `down`
#'   numeric vectors (scalars expand). Only zero tariffs and unconstrained
#'   quotas are accepted; apply new policy through `simulate()`.
#' @param counterfactual An antitrust `Counterfactual` containing supported
#'   ownership, costs, tariff and quota changes.
#' @param ownerPost A `Counterfactual` for the simulation method.
#' @param mcDelta,subset,priceStart,capacitiesPost,bargpowerPost,solver,isMax
#'   Legacy scenario arguments; supplying these with a policy counterfactual
#'   is rejected. Product exit is unsupported.
#' @param last_result Last solved vertical policy model when resuming.
#' @param steps Non-empty list of antitrust `CounterfactualStep` objects.
#' @param cumulative_costs Cumulative tier-specific cost shocks. When omitted,
#'   direct resume retains the shocks stored on `last_result`.
#' @param ... Simulation accepts `policy_control`, a list with positive `tol`
#'   (default `1e-8`), `maxit` (`500`) and `max_active_sets` (`100`). Other
#'   converter or solver arguments are rejected.
#' @return Promotion returns a `VerticalTradeFit` retaining the complete
#'   vertical model. Simulation returns a vertical equilibrium, or an antitrust
#'   `CounterfactualPath` for multiple steps. `simulate_steps()` returns results
#'   and cumulative costs; validation returns the counterfactual invisibly.
#' @details Prices are gross purchaser payments; seller receipts are always
#'   `(1 - tariff) * price`, with the fitted physical cost primitives unchanged.
#'   Quota capacities are multiples of the originating fitted quantities and
#'   remain anchored there through sequences and resumes. Finite quotas require
#'   the full-chain Bertrand model. Tariffs and finite quotas on the same tier,
#'   two finite quota tiers on one product, and zero quotas on positive fitted
#'   output are rejected. Flat second-score auctions support upstream tariffs;
#'   downstream tariffs and finite quotas require an unimplemented allocation
#'   derivation and fail explicitly.
#' @examples
#' # For an existing VerticalFit named fit:
#' # policy_fit <- as_vertical_trade_fit(fit)
#' # simulate(policy_fit, counterfactual(tariff = list(up = .1, down = .05)))
#' @export
setGeneric("as_vertical_trade_fit", function(object, ...)
  standardGeneric("as_vertical_trade_fit"))

.vertical_policy_vector <- function(value, n, kind, name) {
  default <- if (kind == "tariff") 0 else Inf
  if (is.null(value)) value <- rep(default, n)
  if (!is.numeric(value) || !is.null(dim(value)) ||
      !length(value) || !length(value) %in% c(1L, n)) {
    stop("'", name, "' must be a numeric scalar or length-k vector")
  }
  if (length(value) == 1L) value <- rep(value, n)
  value[is.na(value)] <- default
  if (kind == "tariff" && (any(!is.finite(value)) || any(value >= 1))) {
    stop("'", name, "' must be finite and less than 1")
  }
  if (kind == "quota" && any(value < 0)) {
    stop("'", name, "' must be non-negative, with Inf unconstrained")
  }
  value
}

.vertical_policy_list <- function(value, n, kind, name, current = NULL) {
  if (is.null(value)) {
    if (!is.null(current)) return(current)
    value <- list()
  }
  if (!is.list(value) || (length(value) &&
      (is.null(names(value)) || any(!names(value) %in% c("up", "down")) ||
       anyDuplicated(names(value))))) {
    stop("'", name, "' must be a named list with optional 'up' and 'down' tiers")
  }
  result <- list()
  for (tier in c("up", "down")) {
    if (is.null(value[[tier]]) && !is.null(current)) {
      result[[tier]] <- current[[tier]]
    } else {
      result[[tier]] <- .vertical_policy_vector(value[[tier]], n, kind,
        paste0(name, "$", tier))
    }
  }
  result
}

.vertical_policy_capacity <- function(quota, quantity) {
  # In particular, Inf * 0 must remain an unconstrained capacity.
  result <- ifelse(is.infinite(quota), Inf, quota * quantity)
  names(result) <- names(quantity)
  result
}

.vertical_policy_check <- function(model, tariff, quota, quantity) {
  for (tier in c("up", "down")) {
    if (any(tariff[[tier]] != 0) && any(is.finite(quota[[tier]]))) {
      stop("tariff and quota simultaneously on the same tier ('", tier,
        "') are not supported")
    }
  }
  if (any(is.finite(quota$up) & is.finite(quota$down))) {
    stop("only one finite quota tier per product is supported; tier rent allocation would be non-unique")
  }
  if (model@supplyDown == "2nd" && any(tariff$down != 0)) {
    stop("downstream tariffs for second-score vertical auctions are not supported: net-of-tax winner allocation is not derived; upstream tariffs remain supported")
  }
  finite <- is.finite(quota$up) | is.finite(quota$down)
  if (any(finite) && model@supplyDown == "2nd") {
    stop("finite quotas for second-score vertical auctions are not supported: constrained auction allocation is not derived")
  }
  if (any(finite) && model@chain_level != "full") {
    stop("finite vertical quotas require chain_level = 'full'")
  }
  if (any(finite & quantity > 0 & (quota$up == 0 | quota$down == 0))) {
    stop("a zero quota on positive Logit output has no finite-price equilibrium")
  }
  invisible(NULL)
}

.vertical_policy_state <- function(object) {
  if (methods::is(object, "VerticalFit")) object <- object@model
  attr(object, "vertical_trade_policy", exact = TRUE)
}

#' @rdname as_vertical_trade_fit
#' @export
setMethod("as_vertical_trade_fit", "VerticalFit",
  function(object, tariffPre = NULL, quotaPre = NULL, ...) {
    if (length(list(...))) stop("unsupported vertical policy promotion argument")
    if (methods::is(object, "VerticalTradeFit")) {
      stop("object is already a VerticalTradeFit; change policy with simulate()")
    }
    model <- object@model
    n <- length(model@down@prices)
    tariff <- .vertical_policy_list(tariffPre, n, "tariff", "tariffPre")
    quota <- .vertical_policy_list(quotaPre, n, "quota", "quotaPre")
    if (any(tariff$up != 0) || any(tariff$down != 0) ||
        any(is.finite(quota$up)) || any(is.finite(quota$down))) {
      stop("promotion is policy neutral: tariffPre must be zero and quotaPre must be Inf; apply new policies through simulate()")
    }
    # Shares, including the second-score upstream-price adjustment, define
    # the single physical flow; calcQuantities(down) alone does not do so.
    size <- model@down@mktSize
    if (is.na(size)) size <- 1
    quantity <- calcShares(model, preMerger = TRUE) * size
    names(quantity) <- model@down@labels
    if (any(!is.finite(quantity)) || any(quantity < 0)) {
      stop("policy promotion requires finite non-negative fitted quantities")
    }
    .vertical_policy_check(model, tariff, quota, quantity)
    capacities <- lapply(quota, .vertical_policy_capacity, quantity = quantity)
    zeros <- lapply(quota, function(x) rep(0, length(x)))
    state <- list(baseline_quantity = quantity,
      cumulative_costs = list(up = rep(0, n), down = rep(0, n)),
      tariffPre = tariff, tariffPost = tariff, quotaPre = quota,
      quotaPost = quota, capacityPre = capacities, capacityPost = capacities,
      shadowPre = zeros, shadowPost = zeros,
      solver = list(status = "source baseline", strategy = "promotion; no solve"))
    attr(model, "vertical_trade_policy") <- state
    diagnostics <- object@diagnostics
    diagnostics$policy_promotion <- list(source = "VerticalFit",
      recalibrated = FALSE, demand_price_level = if (model@supplyDown == "2nd")
        "upstream" else "downstream")
    new("VerticalTradeFit", spec = object@spec, model = model,
      parameters = object@parameters, observed = object@observed,
      diagnostics = diagnostics,
      upstream_policy = list(tariffPre = tariff$up, quotaPre = quota$up,
        capacity = capacities$up, baseline_quantity = quantity),
      downstream_policy = list(tariffPre = tariff$down, quotaPre = quota$down,
        capacity = capacities$down, baseline_quantity = quantity))
  })

.vertical_policy_values <- function(object, preMerger) {
  state <- .vertical_policy_state(object)
  suffix <- if (preMerger) "Pre" else "Post"
  list(tariff = state[[paste0("tariff", suffix)]],
    quota = state[[paste0("quota", suffix)]],
    capacity = state[[paste0("capacity", suffix)]],
    shadow = state[[paste0("shadow", suffix)]],
    retention = lapply(state[[paste0("tariff", suffix)]], function(x) 1 - x))
}

.vertical_policy_margins <- function(object, preMerger = TRUE, level = FALSE) {
  values <- .vertical_policy_values(object, preMerger)
  pu <- if (preMerger) object@up@pricePre else object@up@pricePost
  pd <- if (preMerger) object@down@pricePre else object@down@pricePost
  cu <- if (preMerger) object@up@mcPre else object@up@mcPost
  cd <- if (preMerger) object@down@mcPre else object@down@mcPost
  up <- values$retention$up * pu - cu
  down <- values$retention$down * pd - pu - cd
  if (!level) { up <- up / pu; down <- down / pd }
  list(up = as.vector(up), down = as.vector(down))
}

.vertical_policy_demand <- function(object, preMerger) {
  active <- .vertical_active_subset(object, preMerger)
  s <- calcShares(object, preMerger)[active]
  alpha <- object@down@slopes$alpha
  if (!methods::is(object@down, "LogitNests")) {
    jacobian <- -alpha * tcrossprod(s)
    diag(jacobian) <- alpha * s + diag(jacobian)
    diversion <- tcrossprod(1 / (1 - s), s) * s
    diag(diversion) <- -s
  } else {
    nests <- droplevels(object@down@nests[active])
    sigma <- unname(object@down@slopes$sigma[as.character(nests)])
    between <- unname(tapply(s, nests, sum)[as.character(nests)])
    within <- s / between
    same <- 1 * outer(as.character(nests), as.character(nests), "==")
    jacobian <- (-alpha * tcrossprod(s *
      (1 + (1 - sigma) / sigma / between), s)) * same +
      (-alpha * tcrossprod(s)) * (1 - same)
    diag(jacobian) <- alpha / sigma * s *
      (1 - (1 - sigma) * within - sigma * s)
    # Exact product-removal shares: sigma is the inclusive-value scale,
    # so removing within share z multiplies the nest mass by (1-z)^sigma.
    same_div <- (1 - within)^(1 - sigma) *
      (1 - between + between * (1 - within)^sigma)
    diff_div <- between * (1 - (1 - within)^sigma)
    diversion <- tcrossprod(1 / same_div - 1, s) * same +
      tcrossprod(1 / (1 / diff_div - 1), s) * (1 - same)
    diag(diversion) <- -s
    diversion[is.na(diversion)] <- 0
  }
  list(shares = s, jacobian = jacobian, diversion = diversion,
    active = active)
}

.vertical_policy_foc <- function(object, priceCand, shadow = NULL,
                                 preMerger = FALSE) {
  active <- .vertical_active_subset(object, preMerger)
  n <- sum(active)
  pu <- priceCand[seq_len(n)]
  pd <- priceCand[n + seq_len(n)]
  if (preMerger) {
    object@up@pricePre[active] <- pu
    object@down@pricePre[active] <- pd
    a <- object@ownerUpLambdaPre[active, active, drop = FALSE]
    b <- object@ownerDownLambdaPre[active, active, drop = FALSE]
    v <- object@ownerDownPre[active, active, drop = FALSE]
  } else {
    object@up@pricePost[active] <- pu
    object@down@pricePost[active] <- pd
    a <- object@ownerUpLambdaPost[active, active, drop = FALSE]
    b <- object@ownerDownLambdaPost[active, active, drop = FALSE]
    v <- object@ownerDownPost[active, active, drop = FALSE]
  }
  values <- .vertical_policy_values(object, preMerger)
  ru <- values$retention$up[active]
  rd <- values$retention$down[active]
  margins <- .vertical_policy_margins(object, preMerger, level = TRUE)
  mu <- margins$up[active]
  md <- margins$down[active]
  if (is.null(shadow)) shadow <- values$shadow
  mu <- mu - shadow$up[active]
  md <- md - shadow$down[active]
  demand <- .vertical_policy_demand(object, preMerger)
  ou <- ownerToMatrix(object@up, preMerger)[active, active, drop = FALSE]
  od <- ownerToMatrix(object@down, preMerger)[active, active, drop = FALSE]
  # Buyer-owned upstream disagreement profits acquire the seller's
  # transfer-receipt derivative at this link; own upstream gains keep weight 1.
  a <- ou + ru * (a - ou)
  bargaining_power <- if (preMerger) object@up@bargpowerPre[active] else
    object@up@bargpowerPost[active]
  buyer_weight <- (1 - bargaining_power) / bargaining_power
  # Seller-owned downstream disagreement profits have coefficient -1,
  # independent of the tariff on the negotiated transfer. Only the buyer
  # payoff weight acquires the seller's transfer-revenue derivative.
  b <- b + (ru - 1) * buyer_weight * od
  up_owner <- if (preMerger) object@up@ownerPre else object@up@ownerPost
  down_owner <- if (preMerger) object@down@ownerPre else object@down@ownerPost
  integrated <- (up_owner == down_owner)[active]
  for (j in which(integrated)) {
    # Preserve the source model's zero operating-margin convention for
    # internal transfers, including the quota opportunity cost.
    a[j, ] <- 0
    a[j, j] <- 1
    b[j, ] <- 0
  }
  upstream <- as.vector((a * demand$diversion) %*% mu -
    (b * demand$diversion) %*% md) / pmax(demand$shares, 1e-100)
  if (object@supplyDown == "2nd") {
    down <- object@down
    if (preMerger) down@ownerPre <- ownerToMatrix(down, TRUE) else
      down@ownerPost <- ownerToMatrix(down, FALSE)
    down@slopes$meanval[active] <- down@slopes$meanval[active] +
      down@slopes$alpha * (pu - down@priceOutside)
    auction_margin <- calcMargins(down, preMerger, level = TRUE)[active]
    downstream <- md - rd * auction_margin
  } else {
    downstream <- as.vector(rd * demand$shares +
      (od * demand$jacobian) %*% md +
      (v * demand$jacobian) %*% mu) /
      pmax(abs(diag(demand$jacobian)), 1e-100)
  }
  list(up = upstream, down = downstream, shares = demand$shares)
}

.vertical_policy_solve <- function(object, preMerger = TRUE,
                                   policy_control = list(), ...) {
  if (length(list(...))) stop("unsupported vertical policy solver argument")
  controls <- utils::modifyList(list(tol = 1e-8, maxit = 500L,
    max_active_sets = 100L), policy_control)
  if (!is.numeric(controls$tol) || length(controls$tol) != 1L ||
      !is.finite(controls$tol) || controls$tol <= 0 ||
      !is.numeric(controls$maxit) || length(controls$maxit) != 1L ||
      !is.finite(controls$maxit) || controls$maxit < 1 ||
      !is.numeric(controls$max_active_sets) ||
      length(controls$max_active_sets) != 1L ||
      !is.finite(controls$max_active_sets) || controls$max_active_sets < 1) {
    stop("invalid policy_control: tol, maxit and max_active_sets must be positive finite scalars")
  }
  active <- .vertical_active_subset(object, preMerger)
  n <- sum(active)
  values <- .vertical_policy_values(object, preMerger)
  size <- object@down@mktSize
  if (is.na(size)) size <- 1
  pu <- if (preMerger) object@up@pricePre else object@up@pricePre
  pd <- if (preMerger) object@down@pricePre else object@down@pricePre
  start <- c(pu[active], pd[active])
  fixed <- start
  solve_indices <- switch(object@chain_level, full = seq_len(2L * n),
    retailer = n + seq_len(n), wholesaler = seq_len(n))
  residual_indices <- switch(object@chain_level, full = seq_len(2L * n),
    retailer = n + seq_len(n), wholesaler = seq_len(n))
  capacity <- pmin(values$capacity$up[active], values$capacity$down[active])
  finite <- is.finite(capacity)
  binding <- rep(FALSE, n)
  seen <- character()
  attempts <- list()
  shadow <- list(up = rep(0, length(active)), down = rep(0, length(active)))
  for (iteration in seq_len(as.integer(controls$max_active_sets))) {
    key <- paste(as.integer(binding), collapse = "")
    if (key %in% seen) stop("vertical quota active-set solver cycled; no verified constrained equilibrium")
    seen <- c(seen, key)
    binding_indices <- which(binding)
    equation <- function(x) {
      prices <- fixed
      prices[solve_indices] <- x[seq_along(solve_indices)]
      shadows <- shadow
      if (length(binding_indices)) {
        full_indices <- which(active)[binding_indices]
        lambda <- x[length(solve_indices) + seq_along(binding_indices)]
        upstream_cap <- is.finite(values$capacity$up[full_indices])
        shadows$up[full_indices[upstream_cap]] <- lambda[upstream_cap]
        shadows$down[full_indices[!upstream_cap]] <- lambda[!upstream_cap]
      }
      foc <- .vertical_policy_foc(object, prices, shadows, preMerger)
      quantity <- foc$shares * size
      c(c(foc$up, foc$down)[residual_indices],
        (quantity[binding] - capacity[binding]) /
          pmax(size, abs(capacity[binding]), 1))
    }
    solver_start <- c(start[solve_indices], rep(0, sum(binding)))
    solved <- NULL
    for (method in c("Broyden", "Newton")) {
      candidate <- tryCatch(nleqslv::nleqslv(solver_start, equation,
        method = method, control = list(ftol = min(controls$tol * .01, 1e-10),
          maxit = as.integer(controls$maxit), allowSingular = TRUE)),
        error = function(e) list(error = conditionMessage(e)))
      residual <- if (is.numeric(candidate$x) && all(is.finite(candidate$x)))
        tryCatch(max(abs(equation(candidate$x))), error = function(e) Inf) else Inf
      attempts[[length(attempts) + 1L]] <- list(method = method,
        active_set = binding_indices, status = candidate$termcd,
        error = candidate$error, residual = residual)
      if (is.finite(residual) && residual <= controls$tol) {
        solved <- candidate
        break
      }
    }
    if (is.null(solved)) {
      stop("vertical policy solver failed: no finite candidate with FOC residual <= ",
        controls$tol, "; best residual = ",
        min(vapply(attempts, function(x) x$residual, numeric(1))))
    }
    start[solve_indices] <- solved$x[seq_along(solve_indices)]
    lambda <- solved$x[length(solve_indices) + seq_along(binding_indices)]
    foc <- .vertical_policy_foc(object, start, preMerger = preMerger)
    quantity <- foc$shares * size
    violating <- finite & !binding & quantity > capacity +
      controls$tol * pmax(size, capacity, 1)
    negative <- binding_indices[lambda < -controls$tol]
    if (any(violating) || length(negative)) {
      binding[violating] <- TRUE
      binding[negative] <- FALSE
      next
    }
    if (length(binding_indices)) {
      full_indices <- which(active)[binding_indices]
      upstream_cap <- is.finite(values$capacity$up[full_indices])
      lambda <- pmax(lambda, 0)
      shadow$up[full_indices[upstream_cap]] <- lambda[upstream_cap]
      shadow$down[full_indices[!upstream_cap]] <- lambda[!upstream_cap]
    }
    verified <- .vertical_policy_foc(object, start, shadow, preMerger)
    residual <- max(abs(c(verified$up, verified$down)[residual_indices]))
    slack <- capacity[finite] - verified$shares[finite] * size
    lambda_full <- (shadow$up + shadow$down)[active][finite]
    complementarity <- abs(lambda_full * slack) / pmax(size, 1)
    if (residual > controls$tol || any(slack < -controls$tol * pmax(size, 1)) ||
        any(complementarity > controls$tol * pmax(abs(lambda_full), 1))) {
      stop("vertical policy candidate failed FOC or quota complementarity verification")
    }
    result_up <- result_down <- rep(NA_real_, length(active))
    result_up[active] <- start[seq_len(n)]
    result_down[active] <- start[n + seq_len(n)]
    return(list(up = result_up, down = result_down, shadow = shadow,
      solver = list(status = "converged", strategy = "primal-dual active set",
        iterations = iteration, active_set = which(active)[binding],
        max_foc_residual = residual, max_complementarity_residual =
          if (length(complementarity)) max(complementarity) else 0,
        attempts = attempts, tolerance = controls$tol)))
  }
  stop("vertical quota active-set iteration limit exceeded")
}

.vertical_policy_promote <- function(model) {
  state <- .vertical_policy_state(model)
  if (is.null(state)) return(model)
  for (field in c("tariff", "quota", "capacity", "shadow")) {
    state[[paste0(field, "Pre")]] <- state[[paste0(field, "Post")]]
  }
  attr(model, "vertical_trade_policy") <- state
  model
}

.vertical_policy_simulate_step <- function(state, changes, cumulative, dots) {
  policy <- .vertical_policy_state(state)
  n <- length(state@down@prices)
  tariff <- .vertical_policy_list(changes$tariff, n, "tariff", "tariff",
    policy$tariffPost)
  quota <- .vertical_policy_list(changes$quota, n, "quota", "quota",
    policy$quotaPost)
  .vertical_policy_check(state, tariff, quota, policy$baseline_quantity)
  policy$tariffPost <- tariff
  policy$quotaPost <- quota
  policy$capacityPost <- lapply(quota, .vertical_policy_capacity,
    quantity = policy$baseline_quantity)
  policy$shadowPost <- lapply(quota, function(x) rep(0, length(x)))
  attr(state, "vertical_trade_policy") <- policy
  result <- .vertical_simulate_model(state, ownerPost = changes$ownership,
    mcDelta = cumulative, dots = dots)
  .vertical_attach_merger_metadata(result)
}

.vertical_policy_steps <- function(state, steps, cumulative = NULL, dots = list()) {
  results <- vector("list", length(steps))
  for (i in seq_along(steps)) {
    changes <- steps[[i]]@changes
    cumulative <- .vertical_step_costs(state, cumulative, changes$costs)
    results[[i]] <- .vertical_policy_simulate_step(state, changes, cumulative, dots)
    policy <- .vertical_policy_state(results[[i]])
    policy$cumulative_costs <- cumulative
    attr(results[[i]], "vertical_trade_policy") <- policy
    state <- .vertical_promote(results[[i]])
  }
  list(results = results, cumulative_costs = cumulative)
}

#' @rdname as_vertical_trade_fit
#' @export
setMethod("validate_counterfactual", "VerticalTradeFit",
  function(object, counterfactual) {
    if (!methods::is(counterfactual, "Counterfactual"))
      stop("'counterfactual' must be a Counterfactual object")
    policy <- .vertical_policy_state(object)
    n <- length(object@model@down@prices)
    for (step in counterfactual@steps) {
      changes <- step@changes
      unsupported <- setdiff(names(changes), c("ownership", "costs", "tariff", "quota"))
      if (length(unsupported)) stop("vertical policy does not support counterfactual field(s): ",
        paste(unsupported, collapse = ", "))
      if (!is.null(changes$ownership)) .vertical_validate_owner(object@model, changes$ownership)
      .vertical_validate_costs(object@model, changes$costs)
      policy$tariffPost <- .vertical_policy_list(changes$tariff, n, "tariff", "tariff",
        policy$tariffPost)
      policy$quotaPost <- .vertical_policy_list(changes$quota, n, "quota", "quota",
        policy$quotaPost)
      .vertical_policy_check(object@model, policy$tariffPost, policy$quotaPost,
        policy$baseline_quantity)
    }
    invisible(counterfactual)
  })

#' @rdname as_vertical_trade_fit
#' @export
setMethod("simulate", "VerticalTradeFit",
  function(object, ownerPost = NULL, mcDelta = NULL, subset = NULL,
           priceStart, capacitiesPost = NULL, bargpowerPost = NULL,
           solver = NULL, isMax = FALSE, ...) {
    if (!methods::is(ownerPost, "Counterfactual")) {
      stop("VerticalTradeFit simulation requires an antitrust Counterfactual")
    }
    if (!is.null(mcDelta) || !is.null(subset) || !missing(priceStart) ||
        !is.null(capacitiesPost) || !is.null(bargpowerPost) ||
        !is.null(solver) || isTRUE(isMax)) {
      stop("cannot combine a vertical policy Counterfactual with legacy scenario arguments")
    }
    validate_counterfactual(object, ownerPost)
    solved <- .vertical_policy_steps(object@model, ownerPost@steps, dots = list(...))
    if (length(ownerPost@steps) > 1L) {
      return(new("CounterfactualPath", initial = object@model,
        steps = ownerPost@steps, results = solved$results,
        diagnostics = list(fit = object, cumulative_costs = solved$cumulative_costs)))
    }
    result <- solved$results[[1L]]
    attr(result, "counterfactual") <- list(model_spec = object@spec, fields = ownerPost)
    result
  })

#' @rdname as_vertical_trade_fit
#' @export
setMethod("simulate_steps", "VerticalTradeFit",
  function(object, last_result, steps, cumulative_costs = NULL, ...) {
    .vertical_assert_model(last_result)
    if (is.null(.vertical_policy_state(last_result)))
      stop("last_result does not contain vertical policy state")
    if (!is.list(steps) || !length(steps) ||
        !all(vapply(steps, methods::is, logical(1), "CounterfactualStep")))
      stop("'steps' must be a non-empty list of CounterfactualStep objects")
    # Validate against the last result's current policies, including any
    # change of policy family since the initial promotion.
    current <- object
    current@model <- last_result
    validate_counterfactual(current, new("Counterfactual", steps = steps))
    state <- .vertical_promote(last_result)
    if (is.null(cumulative_costs)) {
      cumulative_costs <- .vertical_policy_state(last_result)$cumulative_costs
    }
    cumulative_costs <- .vertical_validate_costs(state, cumulative_costs)
    .vertical_policy_steps(state, steps, cumulative_costs, list(...))
  })

#' Inspect vertical policy equilibrium diagnostics
#' @param object A `VerticalTradeFit` or a simulated vertical policy model.
#' @param preMerger Whether to inspect the pre-counterfactual state.
#' @return Tier lists with capacity, physical quantity, shadow value, quota
#'   slack, complementarity and FOC residuals, tariffs, and solver status.
#' @export
vertical_policy_diagnostics <- function(object, preMerger = FALSE) {
  if (methods::is(object, "VerticalFit")) object <- object@model
  state <- .vertical_policy_state(object)
  if (is.null(state)) stop("object does not contain vertical trade policy state")
  values <- .vertical_policy_values(object, preMerger)
  active <- .vertical_active_subset(object, preMerger)
  pu <- if (preMerger) object@up@pricePre else object@up@pricePost
  pd <- if (preMerger) object@down@pricePre else object@down@pricePost
  foc <- .vertical_policy_foc(object, c(pu[active], pd[active]),
    preMerger = preMerger)
  size <- object@down@mktSize
  if (is.na(size)) size <- 1
  quantity <- calcShares(object, preMerger) * size
  result <- list()
  for (tier in c("up", "down")) {
    lambda <- values$shadow[[tier]]
    slack <- values$capacity[[tier]] - quantity
    complementarity <- rep(0, length(quantity))
    finite <- is.finite(values$capacity[[tier]])
    complementarity[finite] <- lambda[finite] * slack[finite]
    residual <- rep(NA_real_, length(quantity))
    residual[active] <- foc[[tier]]
    result[[tier]] <- list(tariff = values$tariff[[tier]],
      seller_retention = values$retention[[tier]], quota = values$quota[[tier]],
      capacity = values$capacity[[tier]], quantity = quantity,
      shadow_value = lambda, slack = slack,
      complementarity_residual = complementarity, foc_residual = residual,
      solver_status = state$solver$status)
  }
  result$solver <- state$solver
  result
}

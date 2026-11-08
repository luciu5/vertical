#' A fitted vertical structural model
#'
#' `VerticalFit` stores vertical calibration metadata while retaining the
#' legacy vertical S4 equilibrium object in its `model` slot.  Demand systems,
#' ownership primitives, and generic economic methods remain owned by
#' `antitrust`.
#' @export
#' @exportClass VerticalFit
setClass(
  "VerticalFit",
  contains = "StructuralFit",
  prototype = prototype(
    spec = list(), parameters = list(), observed = list(), diagnostics = list()
  )
)

.vertical_assert_model <- function(model) {
  if (!methods::is(model, "VertBargBertLogit")) {
    stop("vertical model must inherit from 'VertBargBertLogit'")
  }
  invisible(model)
}

.vertical_active_subset <- function(object, preMerger = TRUE) {
  subset <- object@down@subset
  n_products <- length(object@down@shares)
  if (!is.logical(subset) || length(subset) != n_products ||
      anyNA(subset) || !any(subset)) {
    stop("vertical model contains an invalid active-product subset")
  }
  if (preMerger) {
    previous <- attr(object, "vertical_pre_subset", exact = TRUE)
    if (is.null(previous)) rep(TRUE, n_products) else previous
  } else subset
}

.vertical_conditions <- function(expr) {
  warnings <- character()
  messages <- character()
  value <- withCallingHandlers(
    tryCatch(expr, error = function(e) {
      structure(list(message = conditionMessage(e)), class = "vertical_error")
    }),
    warning = function(w) {
      message_text <- conditionMessage(w)
      warnings <<- c(warnings, message_text)
      if (grepl("'ownerPost' and 'ownerPre' are the same", message_text,
                fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    },
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  list(value = value, warnings = unique(warnings), messages = unique(messages))
}

.vertical_parameters <- function(model) {
  .vertical_assert_model(model)
  pre_costs <- calcMC(model, preMerger = TRUE)
  list(
    down = model@down@slopes,
    bargpowerPre = model@up@bargpowerPre,
    bargpowerPost = model@up@bargpowerPost,
    mcUpPre = pre_costs$up,
    mcDownPre = pre_costs$down
  )
}

.vertical_observed <- function(prices, shares, margins, ownerPre, dots) {
  extras <- dots[intersect(names(dots), c(
    "pricesUp", "marginsUp", "ownerPreUp", "nests", "insideSize",
    "priceOutside", "labels", "supplyDown", "constrain", "chain_level",
    "subset", "normIndex", "priceStartDown", "priceStartUp",
    "control.slopes", "control.equ"
  ))]
  c(list(prices = prices, shares = shares, margins = margins,
         ownerPre = ownerPre), extras)
}

.vertical_calibration_args <- function(spec, prices, shares, margins,
                                       ownerPre, dots) {
  nests <- dots$nests
  if (is.null(nests)) nests <- rep(NA_character_, length(prices))
  supply_down <- if (identical(spec$variant, "auction2nd")) "2nd" else "bertrand"
  if (!is.null(dots$supplyDown)) {
    requested_supply <- match.arg(dots$supplyDown, c("bertrand", "2nd"))
    if (!identical(requested_supply, supply_down)) {
      stop("'supplyDown' conflicts with the selected vertical bargaining variant.")
    }
  }
  if (identical(spec$demand, "logit_nests") && all(is.na(nests))) {
    stop("'nests' must be supplied for nested vertical bargaining calibration.")
  }
  if (identical(spec$demand, "logit") && !all(is.na(nests))) {
    stop("'nests' is only supported for nested vertical bargaining calibration.")
  }
  args <- list(
    supplyDown = supply_down,
    sharesDown = shares,
    pricesDown = prices,
    marginsDown = margins,
    ownerPreDown = ownerPre,
    ownerPostDown = ownerPre,
    nests = nests,
    pricesUp = dots$pricesUp,
    marginsUp = dots$marginsUp,
    ownerPreUp = dots$ownerPreUp,
    ownerPostUp = dots$ownerPreUp,
    mcDeltaDown = rep(0, length(prices)),
    mcDeltaUp = rep(0, length(dots$pricesUp)),
    insideSize = if (is.null(dots$insideSize)) NA_real_ else dots$insideSize,
    priceOutside = if (is.null(dots$priceOutside)) 0 else dots$priceOutside
  )
  optional <- intersect(names(dots), c(
    "diversions", "normIndex", "subset", "priceStartDown", "priceStartUp",
    "isMax", "constrain", "chain_level", "control.slopes", "control.equ",
    "labels"
  ))
  c(args, dots[optional])
}

#' Calibrate a vertical bargaining model
#'
#' @param demand A demand-system name or a `vertical_model_spec`.
#' @param conduct The vertical conduct name; omitted when `demand` is a model
#'   specification.
#' @param prices,shares,margins Observed downstream prices, shares, and
#'   margins.
#' @param ownerPre Pre-counterfactual downstream ownership.
#' @param quantities Retained for lifecycle signature compatibility; vertical
#'   bargaining calibration is share based.
#' @param variant The vertical model variant, either `"standard"` or
#'   `"auction2nd"`.
#' @param ... Upstream observations and model options passed to
#'   [vertical.barg()]. `pricesUp`, `marginsUp`, and `ownerPreUp` are required.
#' @return A calibrated `VerticalFit`.
#' @export
calibrate <- function(demand, conduct = NULL, prices, shares = NULL,
                      margins = NULL, ownerPre, quantities = NULL,
                      variant = "standard", ...) {
  dots <- list(...)
  if (inherits(demand, "vertical_model_spec")) {
    if (!is.null(conduct)) {
      stop("'conduct' must be omitted when 'demand' is a model specification")
    }
    spec <- demand
  } else {
    if (is.null(conduct)) conduct <- "vertical_bargaining"
    spec <- model_spec(demand, conduct, variant)
  }
  entry <- .vertical_registry[[spec$id]]
  if (is.null(entry)) stop("unsupported vertical model specification '", spec$id, "'")
  if (!isTRUE(entry$calibrate)) stop("calibration is not supported for '", spec$id, "'")
  if (is.null(shares) || is.null(margins)) {
    stop("'shares' and 'margins' must be supplied for vertical bargaining calibration.")
  }
  required_up <- c("pricesUp", "marginsUp", "ownerPreUp")
  missing_up <- required_up[vapply(required_up, function(x) is.null(dots[[x]]), logical(1))]
  if (length(missing_up)) {
    stop("'", missing_up[[1L]], "' must be supplied for vertical bargaining calibration.")
  }
  forbidden <- intersect(names(dots), c(
    "ownerPost", "mcDelta", "subset", "pricePost", "mcPre", "mcPost",
    "ownerPostUp", "ownerPostDown", "mcDeltaUp", "mcDeltaDown",
    "bargpowerPost", "capacitiesPost"
  ))
  if (length(forbidden)) {
    stop("'", forbidden[[1L]], "' is a vertical simulation scenario; supply it to simulate().")
  }

  args <- .vertical_calibration_args(spec, prices, shares, margins, ownerPre, dots)
  captured <- .vertical_conditions(do.call(vertical.barg, args))
  if (inherits(captured$value, "vertical_error")) stop(captured$value$message)
  model <- captured$value

  ## Cost-state initialization belongs to the fit lifecycle, after the legacy
  ## vertical constructor has completed its calibration and equilibrium solve.
  ## In particular, this preserves the historical second-score calibration.
  model@up <- antitrust::initialize_cost_state(model@up)
  model@down <- antitrust::initialize_cost_state(model@down)
  attr(model, "vertical_cost_state_initialized") <- TRUE

  calibration_args <- c(
    list(
      demand = spec$demand, conduct = spec$conduct, prices = prices,
      shares = shares, margins = margins, ownerPre = ownerPre,
      variant = spec$variant
    ), dots
  )
  new(
    "VerticalFit",
    spec = spec,
    model = model,
    parameters = .vertical_parameters(model),
    observed = .vertical_observed(prices, shares, margins, ownerPre, dots),
    diagnostics = list(
      status = "completed", source = "calibrated", route = "vertical",
      model_class = class(model)[[1L]], calibration_args = calibration_args,
      calibration_call = match.call(), warnings = captured$warnings,
      messages = captured$messages
    )
  )
}

#' Specify a vertical model from supplied structural parameters
#'
#' The registered vertical entries intentionally have `specify = FALSE`.
#' @param ... Arguments are rejected because no vertical specification route
#'   is defined.
#' @export
specify <- function(...) {
  stop("specify() is not supported for registered vertical bargaining models.")
}

#' Update a vertical calibration
#'
#' @param object A `VerticalFit` returned by [calibrate()].
#' @param ... Named observed-data or calibration-option replacements.
#' @param evaluate If `FALSE`, return the unevaluated calibration call.
#' @return A recalibrated `VerticalFit`, or a call when `evaluate = FALSE`.
#' @export
update.VerticalFit <- function(object, ..., evaluate = TRUE) {
  if (!methods::is(object, "VerticalFit")) {
    stop("'object' must be a VerticalFit returned by calibrate().")
  }
  call_args <- object@diagnostics$calibration_args
  if (!is.list(call_args) || is.null(names(call_args))) {
    stop("this fit does not retain a calibration call; update() requires a fit created by calibrate()")
  }
  changes <- list(...)
  if (length(changes)) {
    if (is.null(names(changes)) || any(!nzchar(names(changes)))) {
      stop("update() arguments must be named calibration arguments")
    }
    specification_changes <- intersect(
      names(changes), c("demand", "conduct", "variant")
    )
    if (length(specification_changes)) {
      stop("update() reruns the same demand, conduct, and variant calibration; vertical respecification is not supported")
    }
    call_args[names(changes)] <- changes
  }
  if (!isTRUE(evaluate)) {
    return(as.call(c(list(as.name("calibrate")), call_args)))
  }
  do.call(calibrate, call_args)
}

#' Display a vertical fit through its legacy economic model
#' @param object A `VerticalFit`.
#' @export
setMethod("show", "VerticalFit", function(object) {
  show(object@model)
  invisible(object)
})

#' Summarize a vertical fit without recomputing equilibrium state
#' @param object A `VerticalFit`.
#' @param ... Reserved for summary-method compatibility.
#' @return A list containing the model specification, economic model class,
#'   and diagnostics.
#' @export
setMethod("summary", "VerticalFit", function(object, ...) {
  list(
    spec = object@spec,
    model_class = class(object@model)[[1L]],
    diagnostics = object@diagnostics
  )
})

#' Calculate diagnostics for a vertical fit
#' @param object A `VerticalFit`.
#' @param ... Arguments passed to the vertical model's diagnostics method.
#' @return The diagnostics returned by the fitted vertical model.
#' @export
setMethod("calcDiagnostics", "VerticalFit", function(object, ...) {
  calcDiagnostics(object@model, ...)
})

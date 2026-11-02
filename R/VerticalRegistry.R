# Vertical model registry. These entries are deliberately local to vertical:
# they describe vertical economics, not antitrust's general model registry.

.vertical_registry <- local({
  entries <- list(
    list(
      id = "logit::vertical_bargaining", demand = "logit",
      conduct = "vertical_bargaining", variant = "standard",
      class = "VertBargBertLogit", calibrator = "vertical.barg",
      calibrate = TRUE, specify = FALSE, simulate = TRUE
    ),
    list(
      id = "logit_nests::vertical_bargaining", demand = "logit_nests",
      conduct = "vertical_bargaining", variant = "standard",
      class = "VertBargBertLogitNests", calibrator = "vertical.barg",
      calibrate = TRUE, specify = FALSE, simulate = TRUE
    ),
    list(
      id = "logit::vertical_bargaining::auction2nd", demand = "logit",
      conduct = "vertical_bargaining", variant = "auction2nd",
      class = "VertBarg2ndLogit", calibrator = "vertical.barg",
      calibrate = TRUE, specify = FALSE, simulate = TRUE
    )
  )
  stats::setNames(entries, vapply(entries, `[[`, character(1), "id"))
})

.vertical_normalize_name <- function(x, what) {
  if (length(x) != 1L || is.na(x)) stop("'", what, "' must be a single name.")
  tolower(gsub("[[:space:].-]+", "_", trimws(as.character(x))))
}

#' Describe a vertical model combination
#'
#' @param demand Demand system: `"logit"` or `"logit_nests"`.
#' @param conduct Must be `"vertical_bargaining"` (aliases `"vertical"`
#'   and `"vertical_barg"` are accepted).
#' @param variant `"standard"` or `"auction2nd"`.
#' @return An object of class `vertical_model_spec`.
#' @export
model_spec <- function(demand, conduct = "vertical_bargaining",
                       variant = "standard") {
  demand <- .vertical_normalize_name(demand, "demand")
  conduct <- .vertical_normalize_name(conduct, "conduct")
  variant <- .vertical_normalize_name(variant, "variant")
  demand <- switch(demand,
    logit_nest = "logit_nests",
    logitnests = "logit_nests",
    logit = "logit",
    logit_nests = "logit_nests",
    stop("unsupported vertical demand system '", demand, "'.")
  )
  conduct <- switch(gsub("_", "", conduct, fixed = TRUE),
    vertical = "vertical_bargaining",
    verticalbarg = "vertical_bargaining",
    verticalbargaining = "vertical_bargaining",
    stop("unsupported vertical conduct '", conduct, "'.")
  )
  variant <- switch(gsub("_", "", variant, fixed = TRUE),
    default = "standard",
    standard = "standard",
    auction2nd = "auction2nd",
    auction2 = "auction2nd",
    second = "auction2nd",
    secondscore = "auction2nd",
    stop("unsupported vertical variant '", variant, "'.")
  )
  id <- if (identical(variant, "auction2nd")) {
    paste0(demand, "::vertical_bargaining::auction2nd")
  } else {
    paste0(demand, "::vertical_bargaining")
  }
  entry <- .vertical_registry[[id]]
  if (is.null(entry)) stop(demand, " / vertical_bargaining currently not supported.")
  structure(
    list(demand = demand, conduct = conduct, variant = variant, id = id),
    class = c("vertical_model_spec", "list")
  )
}

#' List vertical model combinations supported by this package
#'
#' @return A data frame describing the three registered vertical models.
#' @export
supportedModels <- function() {
  do.call(rbind, lapply(.vertical_registry, function(entry) {
    data.frame(
      demand = entry$demand, conduct = entry$conduct,
      variant = entry$variant, class = entry$class,
      calibrator = entry$calibrator, calibrate = entry$calibrate,
      specify = entry$specify, simulate = entry$simulate,
      stringsAsFactors = FALSE, row.names = entry$id
    )
  }))
}

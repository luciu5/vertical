#' Vertical supply-chain model classes
#'
#' These classes contain the vertical-specific upstream/downstream state while
#' reusing the structural demand and bargaining classes supplied by antitrust.
#'
#' @slot up An antitrust `Bargaining` object containing upstream state.
#' @slot down An antitrust `Logit`, `LogitNests`, `Auction2ndLogit`, or
#'   `Auction2ndLogitNests` object containing downstream state.
#' @slot supplyDown Downstream conduct, `"bertrand"` or `"2nd"`.
#' @slot isHorizontal Whether the counterfactual is horizontal rather than
#'   newly vertically integrating.
#' @slot isUpstream Whether a horizontal counterfactual occurs upstream.
#' @slot ownerDownPre,ownerDownPost Vertical downstream ownership matrices.
#' @slot ownerDownLambdaPre,ownerDownLambdaPost Downstream bargaining-weight
#'   matrices.
#' @slot ownerUpLambdaPre,ownerUpLambdaPost Upstream bargaining-weight matrices.
#' @slot constrain Bargaining-power calibration restriction.
#' @slot chain_level Supply-chain tier or tiers solved in equilibrium.
#' @slot control.slopes,control.eq Numerical solver controls.
#' @name Vertical-Classes
#' @aliases VertBargBertLogit VertBarg2ndLogit VertBargBertLogitNests VertBarg2ndLogitNests
#' @importFrom methods setClass new
NULL

#' @rdname Vertical-Classes
#' @export
setClass(
  Class = "VertBargBertLogit",
  representation = representation(
    up = "Bargaining",
    down = "Logit",
    supplyDown = "character",
    isHorizontal = "logical",
    isUpstream = "logical",
    ownerDownPre = "matrix",
    ownerDownPost = "matrix",
    ownerDownLambdaPre = "matrix",
    ownerDownLambdaPost = "matrix",
    ownerUpLambdaPre = "matrix",
    ownerUpLambdaPost = "matrix",
    constrain = "character",
    chain_level = "character",
    control.slopes = "list",
    control.eq = "list"
  ),
  prototype = prototype(
    control.slopes = list(trace = FALSE, ftol = 1e-10)
  ),
  validity = function(object) {
    if (length(object@constrain) != 1L ||
        !object@constrain %in% c("global", "pair", "wholesaler", "retailer")) {
      stop("'constrain' must equal 'global','pair','wholesaler','retailer'")
    }
    if (length(object@supplyDown) != 1L ||
        !object@supplyDown %in% c("bertrand", "2nd")) {
      stop("'supplyDown' must equal 'bertrand' or '2nd'")
    }
    TRUE
  }
)

#' @rdname Vertical-Classes
#' @export
setClass(
  Class = "VertBargBertLogitNests",
  contains = "VertBargBertLogit",
  representation = representation(
    up = "Bargaining",
    down = "LogitNests"
  )
)

#' @rdname Vertical-Classes
#' @export
setClass(
  Class = "VertBarg2ndLogit",
  contains = "VertBargBertLogit",
  representation = representation(
    up = "Bargaining",
    down = "Auction2ndLogit"
  )
)

#' @rdname Vertical-Classes
#' @export
setClass(
  Class = "VertBarg2ndLogitNests",
  contains = "VertBarg2ndLogit",
  representation = representation(
    up = "Bargaining",
    down = "Auction2ndLogitNests"
  )
)

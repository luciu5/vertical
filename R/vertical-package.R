#' Supply-Chain Bargaining and Merger Simulation
#'
#' Tools for upstream/downstream bargaining and horizontal, vertical, and hybrid
#' merger simulation, implemented on the structural state and demand machinery
#' owned by antitrust.
#'
#' Start with `vignette("linked-supply-chain-merger", package = "vertical")`,
#' then `vignette("vertical-tariffs-quotas", package = "vertical")` for policy.
#' These are available in bundles built with vignettes; the repository README
#' also links readable source guides when rendered articles are not installed.
#'
#' `calibrate()` recovers the linked state from observations; `simulate()` uses
#' that fitted state; `update()` recalibrates revised observations. `specify()`
#' and `respecify()` are unsupported for the registered vertical models.
#' Policy promotion preserves the fit and uses vertical's solver; optional trade
#' interoperability is not required for vertical tariff/quota calculations.
#'
#' @seealso [calibrate()], [supportedModels()], [as_vertical_trade_fit()]
#' @keywords internal
#' @import methods
"_PACKAGE"

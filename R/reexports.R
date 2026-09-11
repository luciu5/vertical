# Re-export structural classes, lifecycle generics, and economic generics owned
# by antitrust.  Vertical defines its own calibration and registry functions;
# no antitrust function is copied or reached through a private namespace.

#' @importClassesFrom antitrust Auction2ndLogit Auction2ndLogitNests Bargaining Counterfactual CounterfactualPath CounterfactualStep Logit LogitNests StructuralFit
#' @importFrom antitrust add_step compound_cost_shocks counterfactual final_result initialize_cost_state result_at respecify simulate simulate_steps validate_counterfactual
#' @importFrom antitrust getNestsParms getParms
#' @importFrom BB BBoptim BBsolve
#' @importFrom MASS ginv
#' @importFrom nleqslv nleqslv
#' @importFrom stats model.matrix update
#' @export add_step counterfactual final_result result_at respecify simulate simulate_steps validate_counterfactual
#' @export getNestsParms getParms
NULL

#' @importFrom antitrust CV HHI HypoMonTest calcDiagnostics calcMC calcMargins calcPriceDelta calcPrices calcProducerSurplus
#' @importFrom antitrust calcQuantities calcRevenues calcShares calcSlopes diversion elast hhi ownerToMatrix ownerToVec
#' @export CV HHI HypoMonTest calcDiagnostics calcMC calcMargins calcPriceDelta calcPrices calcProducerSurplus
#' @export calcQuantities calcRevenues calcShares calcSlopes diversion elast hhi ownerToMatrix ownerToVec
NULL

#' Re-exported structural helpers
#'
#' `counterfactual()` constructs antitrust's common structural counterfactual
#' object. `HHI()` computes the common concentration statistic. They are
#' re-exported so a vertical workflow does not require attaching antitrust.
#'
#' @name vertical-reexports
#' @aliases counterfactual HHI
NULL

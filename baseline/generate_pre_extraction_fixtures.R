#!/usr/bin/env Rscript

options(warn = 1)

antitrust_dir <- normalizePath("/home/ctaragin/Projects/antitrust", mustWork = TRUE)
expected_sha <- "24caac92a6c8ad4dc1d3d754c4c26ad0afd4248f"
actual_sha <- trimws(system2("git", c("-C", antitrust_dir, "rev-parse", "HEAD"),
                              stdout = TRUE))
if (length(actual_sha) != 1L || !identical(actual_sha, expected_sha)) {
  stop("fixture generation requires antitrust commit ", expected_sha,
       "; found ", paste(actual_sha, collapse = " "))
}

suppressPackageStartupMessages(pkgload::load_all(antitrust_dir, quiet = TRUE))

prices_down <- c(63.08158, 50.70465, 95.82960, 83.45267)
shares_down <- c(.1293482, .1422541, .4631014, .2152962)
margins_down <- c(13.04232, 13.04233, 29.53958, 29.53958) / prices_down
prices_up <- c(58.109, 53.31135, 58.109, 53.31135)
margins_up <- c(23.31, 14.78715, 23.31, 14.78715) / prices_up
owner_pre_down <- c("D1", "D1", "D2", "D2")
owner_pre_up <- c("U1", "U2", "U1", "U2")
labels <- paste0("product", seq_along(prices_down))

warning_messages <- function(expr) {
  warnings <- character()
  value <- withCallingHandlers(
    expr,
    warning = function(condition) {
      warnings <<- c(warnings, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = unique(warnings))
}

capture_error <- function(expr) {
  tryCatch({
    force(expr)
    NA_character_
  }, error = function(condition) conditionMessage(condition))
}

plain <- function(x) {
  if (is.matrix(x)) {
    return(structure(unname(x), dim = dim(x), dimnames = dimnames(x)))
  }
  if (is.numeric(x)) return(unname(x))
  x
}

capture_model <- function(model, warnings = character()) {
  side <- function(name) methods::slot(model, name)
  up <- side("up")
  down <- side("down")
  safe <- function(expr) tryCatch(expr, error = function(e) NULL)
  list(
    class = class(model)[[1L]],
    flags = list(
      supplyDown = model@supplyDown,
      isHorizontal = model@isHorizontal,
      isUpstream = model@isUpstream,
      constrain = model@constrain,
      chain_level = model@chain_level
    ),
    parameters = list(
      down = lapply(down@slopes, plain),
      bargpowerPre = plain(up@bargpowerPre),
      bargpowerPost = plain(up@bargpowerPost),
      nests = safe(plain(antitrust::getNestsParms(model)))
    ),
    costs = list(
      pre = lapply(suppressWarnings(antitrust::calcMC(model, TRUE)), plain),
      post = lapply(suppressWarnings(antitrust::calcMC(model, FALSE)), plain)
    ),
    prices = list(
      pre = list(up = plain(up@pricePre), down = plain(down@pricePre)),
      post = list(up = plain(up@pricePost), down = plain(down@pricePost))
    ),
    margins = list(
      pre = lapply(antitrust::calcMargins(model, TRUE, level = TRUE), plain),
      post = lapply(antitrust::calcMargins(model, FALSE, level = TRUE), plain)
    ),
    shares = list(
      pre = plain(antitrust::calcShares(model, TRUE)),
      post = plain(antitrust::calcShares(model, FALSE))
    ),
    quantities = list(
      pre = plain(antitrust::calcQuantities(model, TRUE)),
      post = plain(antitrust::calcQuantities(model, FALSE))
    ),
    revenues = list(
      pre = plain(antitrust::calcRevenues(model, TRUE)),
      post = plain(antitrust::calcRevenues(model, FALSE))
    ),
    ownership = list(
      pre = {
        owned <- antitrust::ownerToMatrix(model, TRUE)
        list(down = plain(owned@ownerDownPre), up = plain(owned@up@ownerPre),
             down_lambda = plain(owned@ownerDownLambdaPre),
             up_lambda = plain(owned@ownerUpLambdaPre))
      },
      post = {
        owned <- antitrust::ownerToMatrix(model, FALSE)
        list(down = plain(owned@ownerDownPost), up = plain(owned@up@ownerPost),
             down_lambda = plain(owned@ownerDownLambdaPost),
             up_lambda = plain(owned@ownerUpLambdaPost))
      }
    ),
    hhi = list(pre = plain(antitrust::hhi(model, TRUE)),
               post = plain(antitrust::hhi(model, FALSE))),
    cv = plain(antitrust::CV(model)),
    producer_surplus = list(
      pre = lapply(antitrust::calcProducerSurplus(model, TRUE), plain),
      post = lapply(antitrust::calcProducerSurplus(model, FALSE), plain)
    ),
    price_delta = lapply(antitrust::calcPriceDelta(model, levels = TRUE), plain),
    diagnostics = antitrust::calcDiagnostics(model),
    warnings = warnings
  )
}

build_case <- function(owner_post_up = owner_pre_up,
                       owner_post_down = owner_pre_down,
                       supply_down = "bertrand", nests = NULL,
                       chain_level = "full", mc_delta_up = rep(0, 4),
                       mc_delta_down = rep(0, 4)) {
  args <- list(
    supplyDown = supply_down,
    sharesDown = shares_down,
    pricesDown = prices_down,
    marginsDown = margins_down,
    ownerPreDown = owner_pre_down,
    ownerPostDown = owner_post_down,
    pricesUp = prices_up,
    marginsUp = margins_up,
    ownerPreUp = owner_pre_up,
    ownerPostUp = owner_post_up,
    mcDeltaDown = mc_delta_down,
    mcDeltaUp = mc_delta_up,
    insideSize = 100,
    priceOutside = 10,
    labels = labels,
    chain_level = chain_level
  )
  if (!is.null(nests)) args$nests <- nests
  warning_messages(do.call(antitrust::vertical.barg, args))
}

make_case <- function(...) {
  captured <- build_case(...)
  capture_model(captured$value, captured$warnings)
}

fixtures <- list(
  metadata = list(
    antitrust_sha = expected_sha,
    antitrust_version = unname(read.dcf(file.path(antitrust_dir, "DESCRIPTION"),
                                        fields = "Version")[[1L]]),
    r_version = R.version.string,
    tolerance = 1e-8,
    input = list(
      prices_down = prices_down, shares_down = shares_down,
      margins_down = margins_down, prices_up = prices_up,
      margins_up = margins_up, owner_pre_down = owner_pre_down,
      owner_pre_up = owner_pre_up, inside_size = 100,
      price_outside = 10, labels = labels
    )
  ),
  flat_upstream_horizontal = make_case(
    owner_post_up = rep("U1", 4),
    mc_delta_up = rep(-.01, 4), mc_delta_down = rep(-.01, 4)
  ),
  flat_vertical = make_case(
    owner_post_down = c("U1", "U1", "D2", "D2")
  ),
  flat_downstream_horizontal = make_case(
    owner_post_down = rep("D1", 4)
  ),
  second_score = make_case(
    owner_post_up = rep("U1", 4), supply_down = "2nd"
  ),
  nested_bertrand = make_case(
    owner_post_up = rep("U1", 4), nests = c("n1", "n1", "n2", "n2")
  ),
  wholesaler = make_case(
    owner_post_up = rep("U1", 4), chain_level = "wholesaler"
  ),
  retailer = make_case(
    owner_post_up = rep("U1", 4), chain_level = "retailer"
  ),
  errors = list(
    nested_second_score = capture_error(build_case(
      supply_down = "2nd", nests = c("n1", "n1", "n2", "n2")
    )),
    upp = capture_error(antitrust::upp(build_case()$value)),
    cmcr = capture_error(antitrust::cmcr(build_case()$value))
  )
)

fit_captured <- warning_messages(antitrust::calibrate(
  demand = "logit", conduct = "vertical_bargaining",
  prices = prices_down, shares = shares_down, margins = margins_down,
  ownerPre = owner_pre_down, pricesUp = prices_up, marginsUp = margins_up,
  ownerPreUp = owner_pre_up, insideSize = 100, priceOutside = 10,
  labels = labels
))
cf <- antitrust::add_step(
  antitrust::counterfactual(
    ownership = list(up = rep("U1", 4), down = owner_pre_down)
  ),
  costs = list(up = rep(-.01, 4), down = rep(-.01, 4))
)
path_captured <- warning_messages(antitrust::simulate(fit_captured$value, cf))
fixtures$sequential <- list(
  classes = vapply(path_captured$value@results, function(x) class(x)[[1L]], character(1)),
  step1 = capture_model(path_captured$value@results[[1L]]),
  step2 = capture_model(path_captured$value@results[[2L]]),
  cumulative_costs = lapply(path_captured$value@diagnostics$cumulative_costs, plain),
  warnings = unique(c(fit_captured$warnings, path_captured$warnings))
)

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(file_arg) != 1L) stop("run this generator with Rscript")
script_path <- normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
package_dir <- dirname(dirname(script_path))
output <- file.path(package_dir, "tests", "testthat", "fixtures",
                    "pre-extraction-vertical.rds")
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
saveRDS(fixtures, output, version = 3)
message("wrote ", output)

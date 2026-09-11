test_that("flat and vertical merger fixtures retain reviewed economics", {
  cases <- c("flat_upstream_horizontal", "flat_vertical",
             "flat_downstream_horizontal")
  for (case in cases) {
    testthat::expect_no_error(vertical_expect_fixture(case))
  }
})

test_that("second-score, nested, and chain-level fixtures retain reviewed economics", {
  cases <- c("second_score", "nested_bertrand", "wholesaler", "retailer")
  for (case in cases) {
    testthat::expect_no_error(vertical_expect_fixture(case))
  }
})

test_that("reviewed vertical corner-case errors remain deliberate", {
  args <- vertical_case_args("flat_vertical")
  args$supplyDown <- "2nd"
  args$nests <- c("n1", "n1", "n2", "n2")
  testthat::expect_error(do.call(vertical::vertical.barg, args),
                         "Nested Logit")

  fit <- vertical_make_case("flat_vertical")
  upp_error <- tryCatch(antitrust::upp(fit), error = identity)
  cmcr_error <- tryCatch(antitrust::cmcr(fit), error = identity)
  testthat::expect_s3_class(upp_error, "error")
  testthat::expect_s3_class(cmcr_error, "error")
  normalize_quotes <- function(x) gsub("[\u2018\u2019]", "'", x)
  testthat::expect_identical(
    normalize_quotes(conditionMessage(upp_error)),
    normalize_quotes(vertical_fixture$errors$upp)
  )
  testthat::expect_identical(
    normalize_quotes(conditionMessage(cmcr_error)),
    normalize_quotes(vertical_fixture$errors$cmcr)
  )
})

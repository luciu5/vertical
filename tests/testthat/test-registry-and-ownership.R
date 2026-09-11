test_that("vertical owns its three vertical model registrations", {
  registry <- supportedModels()
  expect_equal(nrow(registry), 3L)
  expect_true(all(registry$conduct == "vertical_bargaining"))
  expect_setequal(
    rownames(registry),
    c("logit::vertical_bargaining",
      "logit_nests::vertical_bargaining",
      "logit::vertical_bargaining::auction2nd")
  )
  antitrust_registry <- antitrust::supportedModels()
  expect_false(any(grepl("vertical", antitrust_registry$conduct)))
  expect_equal(
    model_spec("logit", "vertical_bargaining", "second_score")$id,
    "logit::vertical_bargaining::auction2nd"
  )
  expect_equal(
    model_spec("logit_nests", "vertical.barg")$id,
    "logit_nests::vertical_bargaining"
  )
})



################################################################################

testthat::test_that("lab_sel() works for vectors, giving list of length 3", {
  labels_all <-
    list(
      rtreat ~ "Trial treatment",
      civil ~ "Cohabitation",
      diabetes ~ "Known diabetes",
      hypertension ~ "Known hypertension",
      mrs_1 ~ "One month mRS",
      mrs_6 ~ "Six months mRS",
      '[Intercept]' ~ "Intercept"
    )
  result <-
    label_select(labels_all, c("hypertension", "diabetes", "mrs_1"))
  testthat::expect_type(result, "list")
  testthat::expect_length(result, 3)
})

################################################################################
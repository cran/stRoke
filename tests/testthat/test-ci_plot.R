# Unit Test - gpttools

test_that("ci_plot produces a valid plot, OLR", {
  data(talos)
  talos[, "mrs_1"] <- factor(talos[, "mrs_1"], ordered = TRUE)
  testthat::expect_true(inherits(ci_plot(
    ds = talos,
    x = "rtreat",
    y = "mrs_1",
    vars = c("hypertension", "diabetes")
  ),
  "ggplot"))
})

test_that("ci_plot produces a valid plot", {
  data(talos)
  talos[, "mrs_1"] <-
    factor(ifelse(talos[, "mrs_1"] %in% c("0", "1"), 1, 2))
  testthat::expect_true(inherits(ci_plot(
    ds = talos,
    x = "rtreat",
    y = "mrs_1",
    vars = c("hypertension", "diabetes")
  ),
  "ggplot"))
})

test_that("ci_plot gives error if outcome is not factor", {
  data(talos)
  testthat::expect_error(ci_plot(
    ds = talos,
    x = "rtreat",
    y = "mrs_1",
    vars = c("hypertension", "diabetes")
  ))
})

test_that("ci_plot produces a valid plot", {
  data(talos)
  talos[, "mrs_1"] <-
    factor(ifelse(talos[, "mrs_1"] %in% c("0", "1"), 1, 2))
  testthat::expect_true(inherits(
    ci_plot(
      ds = talos,
      x = "rtreat",
      y = "mrs_1",
      vars = c("hypertension", "diabetes"),
      lbls = c("Intercept", "Placebo",
               "Hypertension", "Diabetes")
    ),
    "ggplot"
  ))
})

test_that("ci_plot produces a valid plot with method='model'", {
  iris$ord<-factor(sample(1:3,size=nrow(iris),replace=TRUE),ordered=TRUE)
  lm <- MASS::polr(ord~., data=iris, Hess=TRUE, method="logistic")
  expect_true(inherits(
    ci_plot(
      ds = lm,
      method = "model"
    ),
    "ggplot"
  ))
})

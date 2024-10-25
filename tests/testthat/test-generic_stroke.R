test_that("generic_stroke() runs!", {
  iris$ord <-
    factor(sample(1:3, size = nrow(iris), replace = TRUE), ordered = TRUE)
  result <-
    suppressMessages(generic_stroke(
      df = iris,
      group = "Species",
      score = "ord",
      variables = colnames(iris)[1:3]
    ))
  expect_equal(length(result), 3)
  expect_equal(class(result), "list")
  expect_true("tbl_summary" %in% class(result[[1]]))
  expect_true("gg" %in% class(result[[2]]))
  expect_true("gg" %in% class(result[[3]]))
})
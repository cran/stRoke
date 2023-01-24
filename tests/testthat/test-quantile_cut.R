test_that("quatile_cut() works for detail.list==FALSE", {
  result <- quantile_cut(iris$Sepal.Length, 3, detail.list = FALSE)
  expect_equal(length(levels(result)), 3)
  expect_s3_class(result, "factor")
})

################################################################################

test_that("quatile_cut() works for inc.outs==TRUE", {
  result <-
    quantile_cut(iris$Sepal.Length,
                 3,
                 y = iris$Sepal.Length + 3,
                 inc.outs = FALSE)
  expect_true(any(is.na(result)))
  
  result <-
    quantile_cut(iris$Sepal.Length,
                 3,
                 y = iris$Sepal.Length + 3,
                 inc.outs = TRUE)
  expect_false(any(is.na(result)))
  expect_equal(length(levels(result)), 3)
  expect_s3_class(result, "factor")
})

################################################################################

test_that("quatile_cut() works for detail.list==TRUE", {
  result <- quantile_cut(iris$Sepal.Length, 3, detail.list = TRUE)
  expect_length(result, 2)
  expect_type(result, "list")
})

################################################################################

# Test created using remotes::install_github("JamesHWade/gpttools") 
# unit test addin.
test_that("quantile_cut works correctly", {
  x <- runif(100)
  groups <- 5
  y <- runif(100)
  expect_equal(
    quantile_cut(x, groups, y, na.rm = TRUE),
    cut(
      x,
      quantile(
        y,
        probs = seq(0, 1, 1 / groups),
        na.rm = TRUE,
        names = TRUE,
        type = 7
      ),
      include.lowest = TRUE,
      labels = NULL,
      ordered_result = FALSE
    )
  )
})

################################################################################


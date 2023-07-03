library(testthat)

# Global
data <- stRoke::talos
data$test <- "A"

test_that("Group must contain two levels", {
  expect_error(win_prob(data,response="mrs_6",group="test"))
})

test_that("Response and group must be specified if not given", {
  expect_equal(win_prob(data[c("mrs_6","rtreat")])[[1]], 
               win_prob(data, response = "mrs_6", group = "rtreat")[[1]])
})

test_that("Group ratio must be a numeric", {
  expect_error(win_prob(data[c("mrs_6","rtreat")], group.ratio = "a"))
})

test_that("Sample size must be a logical", {
  expect_error(win_prob(data[c("mrs_6","rtreat")], sample.size = "a"))
})

test_that("Include sample size", {
  expect_true(!is.na(win_prob(data[c("mrs_6","rtreat")], 
                              sample.size = TRUE)$ss_n))
})

test_that("Output length is 16, and print works", {
  result <-
    win_prob(data[c("mrs_6", "rtreat")], 
             sample.size = TRUE, 
             print.tables = TRUE)
  expect_length(result, 16)
  expect_s3_class(result, "win_Prob")
  expect_output(print.win_Prob(result))
  
})

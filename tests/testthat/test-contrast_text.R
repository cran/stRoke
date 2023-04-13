# Unit test for contrast_text()

library(testthat)

test_that("contrast_text() returns the correct text color", {
  expect_equal(contrast_text("#FFFFFF"), "black")
  expect_equal(contrast_text("#000000"), "white")
  expect_equal(contrast_text("#FFFFFF", light_text="blue", dark_text="green"), 
               "green")
  expect_equal(contrast_text("#000000", light_text="blue", dark_text="green"), 
               "blue")
})
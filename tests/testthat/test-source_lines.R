test_that("source_lines() reads and sources the correct lines from a file", {
  # Create a test file
  test_file <- tempfile(fileext = ".R")
  writeLines(c("# Line 1", "2+2", "# Line 3"), test_file)
  
  # Test that source_lines() reads and sources the correct lines
  testthat::expect_type(source_lines(test_file, 1:2, echo = TRUE), "list")
  testthat::expect_length(source_lines(test_file, 1:2, echo = TRUE), 2)
  testthat::expect_equal(source_lines(test_file, 1:2, echo = TRUE)[[1]], 4)
})
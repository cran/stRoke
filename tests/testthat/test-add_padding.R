test_that("chunks_of_n returns correct", {
  expect_length(add_padding(sample(1:200,5)),5)
  
  expect_equal(nchar(add_padding(sample(1:200,5),5)), rep(5,5))
  
  expect_equal(nchar(add_padding(
    sample(1:200, 5), length = 5, after = TRUE
  )), rep(5, 5))
  
  
  ## Errors
  expect_error(add_padding(matrix(sample(1:200,5)),5))
  
  expect_error(add_padding(matrix(sample(1:200,5)),5,pad = "123"))
  
  
})
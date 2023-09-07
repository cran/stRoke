# library(testthat)
test_that("str_extract returns correct", {
  ls <- do.call(c, lapply(sample(4:8, 20, T), function(i) {
    paste(sample(letters, i, T), collapse = "")
  }))
  
  ds <- do.call(c, lapply(1:20, function(i) {
    paste(sample(ls, 1), i, sample(ls, 1), "23", sep = "_")
  }))
  
  expect_equal(nchar(str_extract(ds, "([0-9]+)")),c(rep(1,9),rep(2,11)))
  
  expect_error(str_extract(data.frame(ds), "([0-9]+)"))
  
})
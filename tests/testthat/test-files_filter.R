test_that("files_filter() correctly filters files", {
  expect_type(files_filter(getwd(),"tests"),
              "character")
})
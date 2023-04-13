talos$id <- seq_len(nrow(talos))

test_that("ds2dd gives desired output", {
  expect_equal(ncol(ds2dd(talos, record.id = "id")), 18)
  expect_s3_class(ds2dd(talos, record.id = "id"), "data.frame")
  expect_s3_class(ds2dd(talos, record.id = 7), "data.frame")
})


test_that("ds2dd gives output with list of length two", {
  expect_equal(length(ds2dd(
    talos,
    record.id = "id",
    include.column.names = TRUE
  )), 2)
})


test_that("ds2dd gives correct errors", {
  expect_error(ds2dd(talos))
  expect_error(ds2dd(talos, form.name = c("basis", "incl")))
  expect_error(ds2dd(talos, field.type = c("text", "dropdown")))
  expect_error(ds2dd(talos, field.label = c("Name", "Age")))
})



colnames(talos) <-
  c("rtreat",
    "mRS 1",
    "mRS 6",
    "hypertension",
    "diabetes",
    "civil",
    "id")

test_that("ds2dd correctly renames", {
  expect_equal(ncol(ds2dd(talos, record.id = "id")), 18)
  expect_s3_class(ds2dd(talos, record.id = "id"), "data.frame")
})

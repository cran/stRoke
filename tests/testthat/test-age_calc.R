test_that("age_calc works for vectors of length 1 (scalars)", {
  result <- age_calc(as.Date("1945-10-23"), as.Date("2018-09-30"))
  expect_equal(round(result), 73)
})

################################################################################

# Unit Test - gpttools

test_that("age_calc works correctly for years", {
  expect_equal(age_calc(as.Date("2000-01-01"), as.Date("2020-01-01"),
                        units = "years"), 20)
})

test_that("age_calc gives error if enddate < dob", {
  expect_error(age_calc(as.Date("2020-01-01"), as.Date("2000-01-01"),
                        units = "years"))
})

test_that("age_calc works correctly for months", {
  expect_equal(age_calc(as.Date("2000-01-01"), as.Date("2020-01-01"),
                        units = "months"), 240)
})

test_that("age_calc works correctly for months", {
  expect_equal(round(age_calc(
    as.Date("2000-07-07"), as.Date("2020-01-01"), units = "months"
  )), 234)
})

test_that("age_calc works correctly for days", {
  expect_equal(age_calc(as.Date("2000-01-01"), as.Date("2020-01-01"),
                        units = "days"), 7305)
  expect_length(age_calc(as.Date("2000-01-01"), as.Date("2020-01-01"),
                         units = "days"), 1)
})

test_that("age_calc works correctly with leap years and precise set to TRUE", {
  expect_equal(age_calc(
    as.Date("2000-02-29"),
    as.Date("2020-02-29"),
    units = "years",
    precise = TRUE
  ),
  20)
})

test_that("age_calc throws an error when enddate is before dob", {
  expect_equal(age_calc(
    as.Date("2000-01-01"),
    as.Date("2014-05-11"),
    precise = FALSE,
    units = "years"
  ),
  14)
})

test_that("age_calc throws an error when wrong unit", {
  expect_error(age_calc(as.Date("2020-01-01"), as.Date("2000-01-01"),
                        units = "hours"))
})

test_that("age_calc throws an error when wrong format", {
  expect_error(age_calc("2020-01-01", as.Date("2000-01-01"), units = "hours"))
})

test_that("age_calc throws an error when wrong format", {
  expect_error(age_calc(as.Date("2020-01-01"), as.Date("2000-01-01"), 
                        units = "years"))
  expect_error(age_calc(as.Date("1982-01-01"), as.Date("2000-01-01"), 
                        units = "seconds"))
})

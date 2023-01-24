testthat::test_that("cpr_check() works for vectors, giving logicals", {
  result <-
    cpr_check(
      c(
        "2310450637",
        "010115-4000",
        "0101896000",
        "010189-3000",
        "300450-1030",
        "010150-4021"
      )
    )
  testthat::expect_equal(any(result), TRUE)
  testthat::expect_type(result, "logical")
  testthat::expect_equal(result[2], FALSE)
})

################################################################################

testthat::test_that("cpr_dob() works for vectors, giving logicals", {
  cpr <- c(
    "2310450637",
    "010115-4000",
    "0101896000",
    "010189-3000",
    "300450-1030",
    "010219-7021",
    "010150-4021"
  )
  testthat::expect_type(cpr_dob(cpr), "character")
  testthat::expect_length(cpr_dob(cpr), 7)
})

testthat::test_that("cpr_dob() works for vectors,
                    giving expected warnings and NAs", {
                      cpr <- c(
                        "2310450637",
                        "010115-4000",
                        "0101896000",
                        "010189-3000",
                        "01018AAAL9",
                        "300450-1030",
                        "010219-7021",
                        "0039-7021",
                        "010150-4021"
                      )
                      result <- suppressWarnings(cpr_dob(cpr))
                      testthat::expect_type(result, "character")
                      testthat::expect_length(result, 9)
                      testthat::expect_true(any(is.na(result)))
                      testthat::expect_warning(cpr_dob(cpr))
                    })

################################################################################

testthat::test_that("cpr_female() works for vectors, giving logicals", {
  result <- cpr_female(
    c(
      "2310450637",
      "010115-4000",
      "0101896000",
      "010189-3000",
      "300450-1030",
      "010150-4021"
    )
  )
  testthat::expect_type(result, "logical")
  testthat::expect_length(result, 6)
  testthat::expect_equal(result[2], TRUE)
})

testthat::test_that("cpr_female() works for vectors, giving logicals", {
  result <- cpr_female(
    c(
      "2310450637",
      "010115-4000",
      "0101896000",
      "010189-3000",
      "300450-1030",
      "010150-4021"
    )
  )
  testthat::expect_type(result, "logical")
  testthat::expect_length(result, 6)
  testthat::expect_equal(result[2], TRUE)
  testthat::expect_error(cpr_female(matrix(
    c(
      "2310450637",
      "010115-4000",
      "0101896000",
      "010189-3000",
      "300450-1030",
      "010150-4021"
    ),
    ncol = 3
  )))
})

################################################################################

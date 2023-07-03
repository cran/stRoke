test_that("pase_calc works", {
  expect_equal(median(pase_calc(stRoke::pase)[,13],na.rm=TRUE), 128.625)
  expect_equal(median(pase_calc(stRoke::pase,TRUE)[,13],na.rm=TRUE), 116.2)
})

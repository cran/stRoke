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

################################################################################

# library(devtools)
# 
# install_github("MangoTheCat/visualTest")
# library(visualTest)
# 
# test_that("New test of color_plot()", {
#   par(bg=NULL)
#   colors <- colors()[34:53]
#   
#   # old <- getwd()
#   # setwd("/Users/au301842/stRoke/tests/testthat")
#   # setwd(old)
#   
#   png(filename = "data/test1.png")
#   color_plot(colors,method="relative")
#   dev.off()
#   
#   # getFingerprint("data/test1.png")
#   
#   expect_equal(getFingerprint("data/test1.png"), "AD07D27813E1D867")
#   # isSimilar(tmp, "AD07D27813E1D867", threshold = 8)
#   
#   #############################
#   
#   # colors <- colors()[51:70]
#   png(filename = "data/test2.png")
#   color_plot(colors,labels = TRUE, borders = FALSE,cex_label = .5, ncol = 3, method="perceived_2")
#   dev.off()
#   
#   # getFingerprint("data/test2.png")
#   
#   expect_equal(getFingerprint("data/test2.png"), "8B0B54D4E4AF2BB1")
#   
#   #############################
#   
#   png(filename = "data/test3.png")
#   color_plot(colors,labels = FALSE, borders = TRUE, ncol = 6, method="perceived")
#   dev.off()
#   
#   # getFingerprint("data/test3.png")
#   
#   expect_equal(getFingerprint("data/test3.png"), "B706F0F1C119CCF8")
# })
################################################################################
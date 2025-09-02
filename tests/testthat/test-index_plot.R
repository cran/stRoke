test_that("index_plot() works!", {
  p1 <- index_plot(stRoke::score[score$event ==
                                   "A",])
  p2 <- index_plot(stRoke::score[score$event ==
                                   "A",], sub_plot = "_per")
  p3 <- index_plot(stRoke::score[score$event == "A",],
                   sub_plot = "_per",
                   facet.by = "event")
  p4 <- index_plot(stRoke::score,
                   sub_plot = "_per",
                   facet.by = "event")
  
  expect_true(all(sapply(list(p1,p2,p3,p4),ggplot2::is.ggplot)))
  
  expect_error(index_plot(
    stRoke::score[score$event == "A",],
    sub_plot = "_per",
    facet.by = c("id", "event")
  ))
})

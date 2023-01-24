test_that("index_plot() works!", {
  expect_type(index_plot(stRoke::score[score$event == "A",]), "list")
  expect_true(inherits(index_plot(stRoke::score[score$event ==
                                                  "A",]), "ggplot"))
  expect_type(index_plot(stRoke::score[score$event == "A",],
                         sub_plot = "_per"), "list")
  
  expect_true(inherits(index_plot(stRoke::score[score$event ==
                                                  "A",], sub_plot = "_per"), 
                       "ggplot"))
  expect_error(index_plot(
    stRoke::score[score$event == "A",],
    sub_plot = "_per",
    facet.by = c("id", "event")
  ))
  
  expect_type(index_plot(stRoke::score,
                         sub_plot = "_per",
                         facet.by = "event"),
              "list")
  expect_true(inherits(
    index_plot(stRoke::score[score$event == "A",],
               sub_plot = "_per",
               facet.by = "event"),
    "ggplot"
  ))
})

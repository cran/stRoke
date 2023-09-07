library(testthat)
test_that("chunks_of_n returns correct", {
  expect_length(chunks_of_n(seq_len(100), 7),15)
  
  expect_equal(lengths(chunks_of_n(seq_len(30), 7, even = TRUE), 
                       use.names = FALSE), c(6, 6, 6, 6, 6))
  
  # This is the example from the function, but I believe it fails in GitHub testing
  ds <- data.frame(nm = paste0("Sub",
                               add_padding(rownames(stRoke::talos))),
                   stRoke::talos)
  
  # ds <- data.frame(nm = paste0("Sub",rownames(stRoke::talos)), 
                   # stRoke::talos)
  
  expect_equal(head(names(chunks_of_n(ds, 7, 
                                      pattern = "Sub[0-9]{3}", label = "grp")),
                    1),"grp-Sub038-Sub011")
  
  expect_equal(
    ds[order(ds$nm),] |> 
      chunks_of_n(7, pattern = "Sub([0-9]+)", label = "grp") |>  
      head(1) |> names(),
    "grp-Sub001-Sub020"
  )
  
  expect_equal(
    ds[order(ds$nm),] |> 
      chunks_of_n(7, pattern = "Sub[0-9]{3}", label = "grp") |>  
      head(1) |> names(),
    "grp-Sub001-Sub020"
  )
  
  ## Errors
  expect_error(chunks_of_n(list(ds), 7, pattern = "Sub[0-9]{3}", label = "grp"))
  
  
})

test_that("n_chunks returns correct", {
  expect_length(n_chunks(seq_len(100), 7),7)
  
  expect_equal(lengths(n_chunks(seq_len(30), 7, even = TRUE), 
                       use.names = FALSE), rep(5,6))
  
  ## This is the example from the function, but I believe it fails in GitHub testing
  ds <- data.frame(nm = paste0("Sub",
                               add_padding(rownames(stRoke::talos))),
                   stRoke::talos)
  
  # ds <- data.frame(nm = paste0("Sub",rownames(stRoke::talos)), 
  #                  stRoke::talos)
  
  expect_equal(head(names(n_chunks(ds, 7, 
                                      pattern = "Sub([0-9]+)", label = "grp")),
                    1),"grp-Sub038-Sub603")
  
  expect_equal(
    ds[order(ds$nm), ] |> 
      n_chunks(7, pattern = "Sub([0-9]+)", label = "grp") |>  
      head(1) |> names(),
    "grp-Sub001-Sub072"
  )
  
  ## Errors
  expect_error(n_chunks(list(ds), 7, pattern = "Sub([0-9]+)", label = "grp"))
  
  
})

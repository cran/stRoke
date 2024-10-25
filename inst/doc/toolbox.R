## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(stRoke)

## ----age_calc-example---------------------------------------------------------
(age <- age_calc(as.Date("1945-10-23"), as.Date("2018-09-30")))
trunc(age)

## ----cpr_check-example--------------------------------------------------------
cpr_check(
  c(
    "2310450637",
    "010190-2000",
    "010115-4000",
    "300450-1030",
    "010150-4021",
    "010150-4AA1"
  )
)

## ----cpr_dob-example----------------------------------------------------------
cpr_dob(c(
  "2310450637",
  "010190-2000",
  "010115-4000",
  "300450-1030",
  "010150-4021"
))

## ----cpr_female-example-------------------------------------------------------
table(cpr_female(stRoke::cprs[, 1]))

## ----ci_plot-example----------------------------------------------------------
data(talos)
talos[, "mrs_1"] <- factor(talos[, "mrs_1"], ordered = TRUE)
ci_plot(
  ds = talos,
  x = "rtreat",
  y = "mrs_1",
  vars = c("hypertension", "diabetes")
)

## ----generic_stroke-example---------------------------------------------------
generic_stroke(stRoke::talos,
               "rtreat",
               "mrs_6",
               variables = c("hypertension", "diabetes", "civil"))

## ----index_plot-example-------------------------------------------------------
index_plot(stRoke::score[score$event == "A", ])

## ----win_prob-example---------------------------------------------------------
win_prob(
  data = stRoke::talos,
  response = "mrs_6",
  group = "rtreat",
  sample.size = TRUE,
  print.tables = TRUE
)


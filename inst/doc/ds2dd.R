## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(stRoke)

## -----------------------------------------------------------------------------
data("talos")
ds <- talos
# As the data set lacks an ID column, one is added
ds$id <- seq_len(nrow(ds))

## -----------------------------------------------------------------------------
datadictionary <- ds2dd(ds,record.id = "id",include.column.names = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  write.csv(datadictionary$DataDictionary,"datadictionary.csv")

## ----eval=FALSE---------------------------------------------------------------
#  REDCapR::redcap_metadata_write(
#    datadictionary$DataDictionary,
#    redcap_uri = keyring::key_get("DB_URI"),
#    token = keyring::key_get("DB_TOKEN")
#  )

## ----eval=FALSE---------------------------------------------------------------
#  # new column names are applied
#  colnames(ds) <- datadictionary$`Column names`
#  
#  REDCapR::redcap_write(
#    ds,
#    redcap_uri = keyring::key_get("DB_URI"),
#    token = keyring::key_get("DB_TOKEN")
#  )


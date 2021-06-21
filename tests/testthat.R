library(renv)
renv::restore()
library(testthat)
library(stevecovid19)

test_check("stevecovid19")

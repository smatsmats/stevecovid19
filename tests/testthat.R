library(testthat)
library(stevecovid19)

renv::restore()

testthat::test_check("stevecovid19")

library(estate)
context("helper")

test_that("test helper functions", {

  validType("vente") %>% expect_equal(TRUE)

})


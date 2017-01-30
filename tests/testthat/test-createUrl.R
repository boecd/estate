library(estate)
context("createUrl")

test_that("check if correct url is produced", {
  provider <- "papfr"
  type <- "vente"

  createUrl(provider=provider, type=type, page=1) %>%
    expect_equal(
      "http://www.pap.fr/annonce/vente-appartements-paris-75-g439-40-annonces-par-page")
  createUrl(provider=provider, type=type, page=3) %>%
    expect_equal(
      "http://www.pap.fr/annonce/ventes-appartements-paris-75-g439-40-annonces-par-page-3")
  createUrl(provider=provider, type=type, page=23) %>%
    expect_equal(
      "http://www.pap.fr/annonce/vente-appartement-paris-75-g439-40-annonces-par-page-23")
})

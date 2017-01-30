library(estate)
context("downloadHtml")

test_that("test download functions", {

  gooddir <- "tmp"
  baddir <- "notexisting"
  unlink(file.path(gooddir, list.files(gooddir)))

  goodpages <- c(1:2)
  badpages <- c("1", "2")

  goodtype <- "vente"
  badtype <- "ventes"

  msg1 <-
    downloadHtml(type = goodtype, pages = goodpages, htmldir = gooddir)
  expect_equal(msg1, paste("files downloaded to", gooddir))

  list.files(gooddir) %>%
    length() %>%
    expect_equal(length(goodpages))

  expect_error(downloadHtml(type = badtype, pages = goodpages, htmldir = gooddir), )
  expect_error(downloadHtml(type = goodtype, pages = badpages, htmldir = gooddir), )
  expect_error(downloadHtml(type = goodtype, pages = goodpages, htmldir = baddir), )

})


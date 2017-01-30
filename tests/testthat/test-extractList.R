library(estate)
context("extractList")

test_that("check if information can be extracted", {

  ## url <- file.path("../../inst/extdata", "papfr-40.html")
  ## url <- file.path("../../inst/extdata/vente/file2177148829a2.html")
  url <- file.path("resources/file179b6bb9f3af.html")
  ## doc <- XML::htmlParse("inst/extdata/vente/file2177148829a2.html", encoding = "utf-8")
  ## url <- file.path("inst/extdata", "papfr-40.html")
  doc <- XML::htmlParse(url, encoding = "utf-8")
  len <- 41

  extractLinkVignette(doc) %>% length %>% expect_equal(len)
  extractLinkPhoto(doc) %>% length %>% expect_equal(len)
  extractDescription(doc) %>% length %>% expect_equal(len)
  extractSummaryTable(doc) %>% nrow %>% expect_equal(len)
  extractPrice(doc) %>% length %>% expect_equal(len)

  extractDate(doc) %>% length %>% expect_equal(len-1)

  ## extractListRaw(url) %>% nrow %>% expect_equal(len)
  extractList(url) %>% nrow %>% expect_equal(len-1)

})

test_that("clean description and extract", {

  description_raw <-
    "\n\t\t\t\t\tParis 14e (75014).\n\t\t\t\t\tParis 14 ième, à deux stations de Montparnasse, \r\nStudette de 11,50 m² (11,43 Loi Carrez), au premier étage.\r\nEn bon état.\r\nSalle d'eau avec douche et vraies toilettes.\r\nBloc évier cuisinette (frigidaire, plaques,...\t\t\t\t"
  description_clean <-
    "Paris 14e (75014). Paris 14 ième, à deux stations de Montparnasse, Studette de 11,50 m² (11,43 Loi Carrez), au premier étage. En bon état. Salle d'eau avec douche et vraies toilettes. Bloc évier cuisinette (frigidaire, plaques,..."
  description_raw %>%
    stringClean() %>%
    expect_equal(description_clean)
  description_clean %>% extractCP() %>% expect_equal("75014")

})

test_that("clean summary and extract", {

  summary_raw <-
    "Pièces2\n\t\t\t\t\t\t\t\t\t\t\t\t\tChambre1\n\t\t\t\t\t\t\t\t\t\t\t\t\tSurface37 m2\n\t\t\t\t\t\t\t\t\t\t\t"
  summary_clean <- "Pièces2 Chambre1 Surface37 m2"
  summary_raw %>% stringClean() %>% expect_equal(summary_clean)

  summary_clean %>% extractSub(pattern = "Pièce") %>% expect_equal(2)
  summary_clean %>% extractSub(pattern = "Chambre") %>% expect_equal(1)
  summary_clean %>% extractSub(pattern = "Surface") %>% expect_equal(37)

})

test_that("clean date", {

  date_raw <-
    "Réf. : B49/1156 /  23 janvier 2017"
  date_clean <- as.Date("2017-01-23")
  date_raw %>% dateClean() %>% expect_equal(date_clean)

})

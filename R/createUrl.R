#' @importFrom magrittr %>%
#' @export

createUrl <- function(page=1, type="vente", provider="papfr") {

  if (provider=="papfr") {
    part1 <- providerConf[[provider]]$part1
    part2 <-
      ifelse(
        page < 22,
        providerConf[[provider]]$part2[[type]][[as.character(page)]],
        providerConf[[provider]]$part2[[type]][["fix"]]
      )
    part3 <- providerConf[[provider]]$part3
    part4 <- ifelse (
      page > 1,
      paste0("-", as.character(page)),
      ""
    )
    theurl <-
      paste0(part1, part2, "-", part3, part4)
  } else if (provider=="enalquiler") {
    part1 <- providerConf[[provider]]$part1
    part2 <- providerConf[[provider]]$part2
    pagefield <- paste0("&page=", page)
    theurl <-
      paste0(part1, part2, pagefield)
  } else {
    theurl <- ""
  }

  return(theurl)
}


#' @importFrom magrittr %>%
#' @export

downloadHtml <- function(type="vente",
                         pages=c(1:2),
                         htmldir="inst/extdata/vente",
                         provider="papfr") {

  stopifnot(type %in% c("vente", "location"))
  stopifnot(length(pages) > 0 && is.numeric(pages))
  stopifnot(dir.exists(htmldir))

  curlpool <- createUrlPool(type = type, pages = pages,
                            provider = provider)
  ## curldat <- list()
  htmldir <<- htmldir
  curl::multi_run(pool = curlpool)
  ## sapply(curldat,
  ##        writeHtmlFile,
  ##        "content",
  ##        htmldir)
  msg <- paste("files downloaded to", htmldir)
  return(msg)
}

createUrlPool <- function(type, pages, provider="papfr") {
  curlpool <-
    curl::new_pool(total_con = 20, host_con = 4, multiplex = TRUE)
  urls <- sapply(pages, createUrl, type = type, provider = provider)
  ## add urls to curlpool
  sapply(urls,
         curl::curl_fetch_multi,
         success,
         failure,
         curlpool)
  return(curlpool)
}

failure <- function(msg){
  cat("Oh noes! Request failed!", msg, "\n")
}

success <- function(res){
  ## cat("Request done! Status:", res$status, "\n")
  ## curldat <<- c(curldat, list(res))
  writeHtmlFile(listitem = res, listfield = "content", htmldir = htmldir)
}

writeHtmlFile <- function(listitem, listfield = "content", htmldir) {
  binfile <- tempfile(tmpdir = htmldir, fileext = ".html")
  filecon <- file(binfile, "wb")
  writeBin(object = listitem[[listfield]], con = filecon)
  close(filecon)
}


#' @importFrom magrittr %>%
#' @export

extractList <- function(url=stop("'url' must be provided")) {
  ## url <- createUrl(page=3)
  doc <- XML::htmlParse(url, encoding = "utf-8")
  ## title price photo description link type furnished rooms bedr size price_per_sqm location floor ascenseur cave refait haussmanien balcon date metro
  description = extractDescription(doc)
  summary_df <- extractSummaryTable(doc)
  estate_df_raw <-
    data.frame(
      price = extractPrice(doc),
      photo = extractLinkPhoto(doc),
      description = description,
      link = extractLinkVignette(doc),
      rooms = summary_df[["rooms"]],
      bedr = summary_df[["bedr"]],
      size = summary_df[["size"]],
      location = extractCP(description),
      stringsAsFactors = FALSE
    )
  estate_df_clean <-
    estate_df_raw %>%
    cleanList() %>%
    cbind.data.frame(date = extractDate(doc))
  return(estate_df_clean)
  ## return(nrow(estate_df_clean))
  ## return(estate_df_raw)
}

#' remove ads, using photo url; alternatively, could use NA zip code
cleanList <- function(df, featurecol="location") {
  obs_select <-
    df[[featurecol]] %>%
    is.na()
  df_select <-
    df[obs_select==FALSE, ]
  return(df_select)
}

extractLinkVignette <- function(doc, divclass="col-2-5 position-relative") {
  link_vignette <-
    XML::xpathSApply(doc,
                     paste0("//div[@class='", divclass, "']/a"),
                     XML::xmlGetAttr, "href") %>%
    ## .[substr(., 2, 8)=="annonce"] %>%
    paste0("http://www.pap.fr", .)
  return(link_vignette)
}

extractLinkPhoto <- function(doc, divclass="col-2-5 position-relative") {
  link_photo <-
    XML::xpathSApply(doc,
                     paste0("//div[@class='", divclass, "']/a/img"),
                     XML::xmlGetAttr, "src") # %>%
    ## .[stringr::str_detect(., pattern = "pap.fr")]
  return(link_photo)
}

extractDescription <- function(doc, pclass="item-description") {
  description_raw <-
    XML::xpathSApply(doc,
                     paste0("//p[@class='", pclass, "']"),
                     XML::xmlValue)
  description_clean <-
    description_raw %>%
    stringClean()
  return(description_clean)
}


extractDate <- function(doc, pclass="date") {
  date_raw <-
    XML::xpathSApply(doc,
                     paste0("//p[@class='", pclass, "']"),
                     XML::xmlValue)
  date_clean <-
    date_raw %>%
    dateClean()
  return(date_clean)
}

extractSummaryTable <- function(doc, ulclass="item-summary float-left") {
  summary_raw <-
    XML::xpathSApply(doc,
                     paste0("//ul[@class='", ulclass, "']"),
                     XML::xmlValue)
  summary_clean <-
    summary_raw %>%
    stringClean()
  rooms <-
    summary_clean %>%
    extractSub(pattern = "Pièce")
  bedr <-
    summary_clean %>%
    extractSub(pattern = "Chambre")
  size <-
    summary_clean %>%
    extractSub(pattern = "Surface")
  summary_df <-
    cbind.data.frame(rooms, bedr, size)
  return(summary_df)
}

extractPrice <- function(doc, spanclass="price") {
  price_raw <-
    XML::xpathSApply(doc,
                     paste0("//span[@class='", spanclass, "']"),
                     XML::xmlValue)
  price_clean <-
    price_raw %>%
    sub(" €", "", .) %>%
    gsub("[.]", "", .) %>%
    as.integer()
  return(price_clean)
}

stringClean <- function(character) {
  character_clean <-
    character %>%
    gsub("\t", " ", .) %>%
    gsub("\n", " ", .) %>%              # remove newline
    gsub("\r", " ", .) %>%              # remove carriage return
    gsub("[ ]+", " ", .) %>%            # remove whitespace
    stringr::str_trim(side = "both") %>%
    gsub("\t ", "\t", .) %>%            # remove tabs
    gsub(" \t", "\t", .) %>%
    gsub("[\t]+", "\t", .)
  return(character_clean)
}

extractSub <- function(character, pattern="Pièce") {
  regpattern <- paste0("(", pattern, "s|", pattern, ")")
  character_sub <-
    character %>%
    stringr::str_extract(pattern = paste0(regpattern, "[0-9]+")) %>%
    sub(regpattern, "", .) %>%
    as.integer()
  return(character_sub)
}

extractCP <- function(character) {
  cp <-
    character %>%
    stringr::str_extract(pattern = "[(][0-9]+[)][.]") %>%
    stringr::str_extract(pattern = "[0-9]+")
  return(cp)
}

dateClean <- function(character) {
  french_months <- c('janvier', 'février', 'mars', 'avril', 'mai', 'juin', 'juillet', 'août', 'septembre', 'octobre', 'novembre', 'décembre')
 date_string <-
    character %>%
    stringr::str_extract(pattern = "[0-9]+[ ][a-z]+[ ][0-9]+$") %>%
    stringr::str_split(" ")
  day <- date_string %>% sapply("[[", 1)
  month <- date_string %>%
    sapply("[[", 2) %>%
    match(french_months) %>%
    sprintf(fmt = "%02d")
  year <- date_string %>% sapply("[[", 3)
  date_stand <-
    paste(year, month, day, sep = "-") %>%
    as.Date()
  return(date_stand)
}

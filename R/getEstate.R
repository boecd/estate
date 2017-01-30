#' Get estate
#'
#' Get real estate data
#'
#' This function scrapes data from pap.fr using the XML and
#' the stringr package.
#'
#' @param type choose to scrape offers for rent or sale.
#' @param pages select the number of pages, each of them containing
#' 40 items.
#'
#' @author Bo Werth
#' @keywords scrape
#' @seealso www.pap.fr
#' @export
#' @examples
#' data <- getEstate(type = "vente", pages = 50)
#' table.all <- getEstate(type = "location", pages = 2)
#' data2 <- transformEstate(data=table.all)

getEstate <- function(type="vente", pages=50)
  {

    hrefFun <- function(x) {
      xpathSApply(x, "./a", xmlAttrs)
    }

    ## require(XML)
    ## require(stringr)
    page <- "http://www.pap.fr/annonce/"
    spec <- "paris-75-g439-40-annonces-par-page"

    if (type=="vente"){

      label <- rbind.data.frame(c("vente-appartements",NA),
                                c("appartement-a-vendre",2),
                                c("ventes-appartements",3),
                                c("vente-appartement-particulier",4),
                                c("immobilier-vente-appartement",5),
                                c("appartement-en-vente",6),
                                c("achat-vente-appartement",7),
                                c("vente-d-appartement",8),
                                c("ventes-appart",9),
                                c("ventes-appartement-sans-agence",10),
                                c("appartement-a-acheter",11),
                                c("vente-appartement-entre-particuliers",12),
                                c("vente-appartement-particuliers",13),
                                c("particulier-vend-appartement",14),
                                c("vendre-appartement",15),
                                c("annonce-appartement",16),
                                c("annonces-appartements",17),
                                c("annonces-appartement-particulier",18),
                                c("petite-annonce-appartement",19),
                                c("recherche-appartement",20),
                                c("recherche-appartement-a-vendre",21))
      label.fix <- "vente-appartement"

    }

    if (type=="location") {

      label <- rbind.data.frame(c("locations-appartement",NA),
                                c("locations-appartement",2),
                                c("locations-appartement-particulier",3),
                                c("immobilier-location-appartement",4),
                                c("locations-d-appartement",5),
                                c("location-appart",6),
                                c("offre-location-appartement",7),
                                c("appartement-a-louer",8),
                                c("appartement-en-location",9),
                                c("recherche-location-appartement",10),
                                c("annonce-location-appartement",11),
                                c("annonces-location-appartement",12),
                                c("locations-appartement-entre-particuliers",13),
                                c("location-appartements-sans-agence",14),
                                c("location-appart-particulier",15),
                                c("appart-a-louer",16),
                                c("recherche-appartement-louer",17),
                                c("appartement-a-louer-particulier",18),
                                c("offres-location-appartement",19),
                                c("location-appartement",20),
                                c("location-appartement",21))
      label.fix <- "location-appartement"

    }

    ## i <- 1
    ## i <- 5
    table.all <- NULL
    cat("\n preparing data...\n\n")
    for (i in c(1:pages)) {
      cat(paste0("   loading page ", i, " of ", pages, "\n"))
      if (i == 1) {
        theurl <- paste0(page,as.character(label[i,1]), "-", spec)
      } else if (i > 1 & i < 22) {
        theurl <- paste0(page,as.character(label[i,1]), "-", spec, "-", i)
      } else {
        theurl <- paste0(page, label.fix, "-", spec, "-", i)
      }
      ## read contents
      ## new method
      doc = htmlParse(theurl, encoding = "utf-8")
      ## plain.text <- xpathSApply(doc, "//p", xmlValue)

      link.vignettes <- xpathSApply(doc, "//div[@class='vignette-annonce']/a", xmlGetAttr, "href")
      link.vignettes <- matrix(data = link.vignettes, ncol = 2, byrow = TRUE)
      link.vignettes <- link.vignettes[, 1]
      link.vignettes <- paste0("http://www.pap.fr", link.vignettes)
      link.photos <- xpathSApply(doc, "//div[@class='vignette-annonce']/a/img", xmlGetAttr, "src")

      plain.text <- xpathSApply(doc, "//li", xmlValue)[69:229]
      ## h(plain.text)
      ## plain.text <- gsub("\t", " ", plain.text)
      plain.text <- gsub("\n", " ", plain.text)
      plain.text <- gsub("\r", " ", plain.text)
      plain.text <- gsub("[ ]+", " ", plain.text)
      plain.text <- str_trim(plain.text, side = "both")

      plain.text <- gsub("\t ", "\t", plain.text)
      plain.text <- gsub(" \t", "\t", plain.text)
      plain.text <- gsub("[\t]+", "\t", plain.text)
      ## h(plain.text)

      plain.text <- plain.text[-1]

      ## i <- 4
      ## ## for (i in c(1:length(plain.text))) {
      ## while (i < length(plain.text)) {
      ##   first <- sapply(strsplit(plain.text[i], " "), "[[", 1)

      ##   if (first=="Surface") {
      ##     row.copy <- plain.text[i]

      ##   print(first)
      ##   i <- i + 4
      ## }

      ## plain.text[125:130]
      ## head(, 20)
      ## h(plain.text)
      ## Description, Pieces, Chambres, Surface (sometimes missing),
      ## names(ads.df) <- c("desc", "rooms", "bedr", "size")
      block.all <- NULL
      i <- 1
      while (i < (length(plain.text)-4)) {
        desc <- plain.text[i]
        i <- i + 1
        rooms <- plain.text[i]
        i <- i + 1
        first <-  sapply(strsplit(plain.text[i], "\t"), "[[", 1)
        if (first=="Chambre" | first=="Chambres") {
          bedr <- plain.text[i]
          i <- i + 1
        } else {
          bedr <- ""
        }
        first <-  sapply(strsplit(plain.text[i], "\t"), "[[", 1)
        if (first=="Surface") {
          size <- plain.text[i]
          i <- i + 1
        } else {
          size <- ""
        }
        ## block.all <- c(block.all, block)
        block.all <- c(block.all, desc, rooms, bedr, size)
      }

      ## head(block.all, 10)
      ## View(block.all)

      ## ads <- matrix(data = plain.text, nrow = 40, ncol = 4, byrow = TRUE, na = "NA")
      ads <- matrix(data = block.all, nrow = 40, ncol = 4, byrow = TRUE)
      ## ads[1:4,]

      ads.df <- data.frame(ads)
      names(ads.df) <- c("desc", "rooms", "bedr", "size")
      ads.df$rooms <- sub("Pièces\t", "", ads.df$rooms)
      ads.df$rooms <- sub("Pièce\t", "", ads.df$rooms)
      ads.df$bedr <- sub("Chambres\t", "", ads.df$bedr)
      ads.df$bedr <- sub("Chambre\t", "", ads.df$bedr)
      ads.df$size <- sub("Surface\t", "", ads.df$size)
      ads.df$size <- sub(" m2", "", ads.df$size)
      ads.df$rooms <- as.factor(ads.df$rooms)
      ads.df$bedr <- as.factor(ads.df$bedr)
      ads.df$size <- as.numeric(ads.df$size)
      ## h(ads.df)
      table <- ads.df
      ## View(block.all)
      ## View(table)
      table$link <- link.vignettes[1:40]
      table$photo <- link.photos[1:40]
      table$type <- type

      table.all <- rbind(table.all, table)
    }
    return (table.all)

    ## table <- readHTMLTable(theurl, encoding = "utf-8")
    ## table <- table[[1]]
    ## ## read hyperlinks
    ## table2 <- readHTMLTable(theurl, elFun = hrefFun, stringsAsFactors = FALSE)
    ## table2 <- table2[[1]]
    ##   row.all <- NULL
    ##   for (j in c(2:nrow(table)))
    ##     {
    ##       if (is.na(table[j+1,2]))
    ##         {
    ##           row <- cbind.data.frame(table[j,1], table[j,2], "none", table[j+1,1], table2[j,1])
    ##         } else {
    ##           row <- cbind.data.frame(table[j,1], table[j,2], table[j+1,1], table[j+1,2], table2[j,1])
    ##         }
    ##       row[,1] <- gsub("studio", "studio 1 pièce", row[,1], fixed = TRUE)
    ##       row[,4] <- gsub("\t", "", row[,4], fixed = TRUE)
    ##       row[,4] <- gsub("\n", "", row[,4], fixed = TRUE)
    ##       row[,4] <- gsub("\r", "", row[,4], fixed = TRUE)
    ##       names(row) <- c("title","price","photo","description","link")
    ##       row.all <- rbind(row.all, row)
    ##     }
    ##   row.all <- row.all[!is.na(str_locate(tolower(row.all[,1]), type)),]
    ##   row.all <- row.all[!is.na(row.all$title),]
    ##   row.all <- row.all[!is.na(row.all$price),]
    ##   ## row.all$page <- i
    ##   table.all <- rbind(table.all,row.all)
    ## }
    ## cat("\n Done!\n")

  }

transformEstate <- function(data=table.all) {
    ## names(table.all)
    ## data <- table.all[,!colnames(table.all)=="page"]
    ## data <- table.all
    ## data <- data[!substr(data[,1], 1, 1)=="+",]

    ## data$type <- type
    ## data$link <- paste0("http://www.pap.fr", data$link)
    ## data$photo <- gsub("[^[:digit:]]","",data$photo)

    ## is the property furnished?
    ## data <- data[str_detect(data[, "desc"], ignore.case("chambre"))==FALSE,]
    data$furnished <- FALSE
    data$furnished[str_detect(data[, "desc"], ignore.case("meublée"))==TRUE] <- TRUE
    data[, "desc"] <- sub("meublée ", "", data[, "desc"])

    ## extract information from title
    ## x <- strsplit(as.character(data[, "desc"]), " ", fixed = TRUE)
    x <- strsplit(as.character(data[, "desc"]), "\t", fixed = TRUE)
    ## ## type of property: appartment, studio...
    ## x1 <- sapply(x, "[[", 1)
    ## x1 <- strsplit(x1, " ", fixed = TRUE)
    ## data$type <- sapply(x1, "[[", 2)

    data$price <- sapply(x, "[[", 2)
    data$price <- sub(" €", "", data$price)
    data$price <- as.numeric(gsub("\\.", "", data$price))
    data$price[data$type=="vente"] <- data$price[data$type=="vente"] * 10^(-3)

    ## h(x)
    ## data$type <- sapply(x, "[[", 2)
    ## data$rooms2 <- sapply(x, "[[", 3)
    ## data$rooms2 <- gsub("[^[:digit:]]","",data$rooms2)
    ## data$size2 <- sapply(x, "[[", 4)
    ## data$size2 <- as.numeric(sub(" m²", "", data$size2))

    ## ratio
    data$price_per_sqm <- data$price / data$size
    ## harmonize location
    ## h(x)
    ## for (i in 1:nrow(data)) {
    ##   if(!is.na(x[[i]][6])) data$location[i] <- x[[i]][6] else data$location[i] <- NA
    ## }
    ## data$location <- sub("E", "", data$location)
    ## data$location <- sub("r", "", data$location)
    ## data$location <- sub("\\(75016\\)", "16", data$location)
    ## data$location <- sub("\\(75116\\)", "16", data$location)
    ## data$location <- as.numeric(data$location)
    ## location information either in 6 or 7
    location6 <- sapply(x, "[[", 6)
    location6.sub <- sapply(strsplit(location6, " "), "[[", 1)!="Paris"
    location6[location6.sub] <- sapply(x, "[[", 7)[location6.sub]
    location6 <- sub("Paris ", "", location6)
    data$location <- sapply(strsplit(location6, " "), "[[", 1)
    data$location <- as.numeric(as.character((sub("e", "", data$location))))
    data$location <- factor(data$location, levels = sort(unique(data$location)))
    ## h(data[, c(2:length(data))])
    ## h(data[, 1])

    ## extract information from description #
    ## location
    ## location2_start <- str_locate(data[,4], format(Sys.time(), "%Y"))
    ## location2_stop <- str_locate(data[,4], "\\.")
    ## for (i in 1:nrow(data)) {
    ##   data$location2_[i] <- substr(data[i,4], start = location2_start[i,2]+1, stop = location2_stop[i,1]-1)
    ## }
    ## ## harmonize location2
    ## data$location2 <- gsub("[^[:digit:]]","",data$location2)
    ## data$location2 <- sub("75116","16",data$location2)
    ## data$location2[data$location2=="" | is.na(data$location2)] <- "75"
    ## data$location2 <- as.numeric(data$location2)
    ## floor
    data$floor <- NA
    data$floor[is.na(data$floor)] <- str_extract(data[is.na(data$floor), "desc"], "au [[:digit:]]e")
    data$floor[is.na(data$floor)] <- str_extract(data[is.na(data$floor), "desc"], "[[:digit:]]e étage")
    data$floor[is.na(data$floor)] <- str_extract(data[is.na(data$floor), "desc"], "[[:digit:]]ème étage")
    data$floor[is.na(data$floor)] <- str_extract(data[is.na(data$floor), "desc"], "dernier étage")
    data$floor <- gsub("dernier","99", data$floor)
    data$floor <- gsub("[^[:digit:]]","",data$floor)
    data$floor <- factor(data$floor, levels = sort(unique(as.numeric(data$floor))))

    ## elevator
    data$ascenseur <- FALSE
    data$ascenseur[str_detect(data[, "desc"], ignore.case("sans ascenseur"))==FALSE & str_detect(data[, "desc"], ignore.case("ascenseur"))==TRUE] <- TRUE
    ## cellar
    data$cave <- FALSE
    data$cave[str_detect(data[, "desc"], ignore.case("cave"))==TRUE] <- TRUE
    ## renovated
    data$refait <- FALSE
    data$refait[str_detect(data[, "desc"], ignore.case("refait"))==TRUE] <- TRUE
    data$refait[str_detect(data[, "desc"], ignore.case("rénové"))==TRUE] <- TRUE
    ## haussmannien
    data$haussmanien <- FALSE
    data$haussmanien[str_detect(data[, "desc"], ignore.case("haussmanien"))==TRUE] <- TRUE
    ## balcony
    data$balcon <- FALSE
    data$balcon[str_detect(data[, "desc"], ignore.case("balcon"))==TRUE] <- TRUE

    ## remove information
    data[, "desc"] <- sub("Voir site perso", "", data[, "desc"])
    data[, "desc"] <- sub("Agences s'abstenir", "", data[, "desc"])
    data[, "desc"] <- sub("Mise à jour le ", "", data[, "desc"])
    data[, "desc"] <- sub("Nouvelle du ", "", data[, "desc"])

    ## x <- strsplit(data[, "desc"], split = " ")

    french.months <- c('janvier', 'février', 'mars', 'avril', 'mai', 'juin', 'juillet', 'août', 'septembre', 'octobre', 'novembre', 'décembre')
    ## data$date <- paste0(substr(sapply(x, '[[', 4), 1, 4), '-', sprintf(match(sapply(x, '[[', 3), french.months), fmt = "%02d"), '-', sapply(x, '[[', 2))
    ## date in position 3 or 4
    date3 <- sapply(x, '[[', 3)
    ## date3[date3.sub]
    ## x[[47]]
    ## date3[47]

    date3.split <- strsplit(date3, " ")
    date3.sub <- sapply(date3.split, "length")!=3

    ## date3.sub2 <- sapply(date3.split[!date3.sub], "[[", 3)!=format(Sys.time(), "%Y")
    ## date3.sub[] <- TRUE
    ## date3.sub[!date3.sub][date3.sub2] <- TRUE
    ## date3.split[!date3.sub][sapply(date3.split[!date3.sub], "[[", 3)!=format(Sys.time(), "%Y")]
    date3[date3.sub] <- sapply(x, '[[', 4)[date3.sub]

    date3.split <- strsplit(date3, " ")
    date3.sub <- sapply(date3.split, "[[", 3)!=format(Sys.time(), "%Y")
    date3[date3.sub] <- sapply(x, '[[', 4)[date3.sub]

    date3.split <- strsplit(date3, " ")
    data$date <- paste0(sapply(date3.split, "[[", 3), '-', sprintf(match(sapply(date3.split, "[[", 2), french.months), fmt = "%02d"), '-', sapply(date3.split, '[[', 1))
    data$date <- as.Date(data$date)
    ## data[,4] <- sub(format(Sys.time(), "%Y"), "", data[,4])
    ## data <- data[!substr(data$title, 1, 7)=="Accueil",]

    ## data$metro <- sub("suite", "", str_extract(data[,4], "suite.+"))
    ##
    ## data <- data[!is.na(data$price_per_sqm),]
    ## data <- data[order(data$price_per_sqm),]
    ##
    return(data)
  }

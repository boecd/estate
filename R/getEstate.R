#' Get estate
#'
#' Get real estate data
#'
#' This function scrapes data from \code{\link{pap.fr}} using the \code{XML} and
#' the \code{stringr} package.
#'
#' @param type choose to scrape offers for rent or sale.
#' @param pages select the number of pages, each of them containing
#' 40 items.
#'
#' @author Bo Werth
#' @keywords scrape
#' @seealso \code{www.pap.fr}
#' @export
#' @examples
#' data <- getEstate(type = "vente", pages = 50)

getEstate <- function(type="vente", pages=50)
  {
    ##
    hrefFun <- function(x)
      {
        xpathSApply(x,"./a",xmlAttrs)  
      }
    ##
    require(XML)
    require(stringr)
    page <- "http://www.pap.fr/annonce/"
    spec <- "paris-75-g439-40-annonces-par-page"

    if (type=="vente")
      {
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
    
    if (type=="location")
      {
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
    
    table.all <- NULL
    cat("\n preparing data...\n\n")
    for (i in c(1:pages))
      {
        cat(paste0("   loading page ", i, " of ", pages, "\n"))
        if (i == 1)
          {
            theurl <- paste0(page,as.character(label[i,1]), "-", spec)
          } else if (i > 1 & i < 22)
            {
              theurl <- paste0(page,as.character(label[i,1]), "-", spec, "-", i)
            } else
              {
                theurl <- paste0(page, label.fix, "-", spec, "-", i)
              }
        ## read contents
        table <- readHTMLTable(theurl, encoding = "utf-8")
        table <- table[[1]]
        ## read hyperlinks
        table2 <- readHTMLTable(theurl, elFun = hrefFun, stringsAsFactors = FALSE)
        table2 <- table2[[1]]
        
        row.all <- NULL
        for (j in c(1:nrow(table)))
          {
            if (is.na(table[j+1,2]))
              {
                row <- cbind.data.frame(table[j,1], table[j,2], "none", table[j+1,1], table2[j,1])
              } else {
                row <- cbind.data.frame(table[j,1], table[j,2], table[j+1,1], table[j+1,2], table2[j,1])
              }
            row[,1] <- gsub("studio", "studio 1 pièce", row[,1], fixed = TRUE)
            row[,4] <- gsub("\t", "", row[,4], fixed = TRUE)
            row[,4] <- gsub("\n", "", row[,4], fixed = TRUE)
            row[,4] <- gsub("\r", "", row[,4], fixed = TRUE)
            names(row) <- c("title","price","photo","description","link")
            row.all <- rbind(row.all, row)
          }
        row.all <- row.all[!is.na(str_locate(tolower(row.all[,1]), type)),]
        row.all <- row.all[!is.na(row.all$title),]
        row.all <- row.all[!is.na(row.all$price),]
        ## row.all$page <- i
        table.all <- rbind(table.all,row.all)
      }
    ## View(table.all)
    ## View(table)
    ## View(row.all)
    
    cat("\n Done! Use plotEstate to visualize\n")
    data <- table.all[,!colnames(table.all)=="page"]
    data <- data[!substr(data[,1], 1, 1)=="+",]

    data$type <- type
    data$link <- paste0("http://www.pap.fr", data$link)
    data$photo <- gsub("[^[:digit:]]","",data$photo)
    ## extract information from title
    x <- strsplit(as.character(data[,1]), " ", fixed = TRUE)
    ## data$type <- sapply(x, "[[", 2)
    data$rooms <- sapply(x, "[[", 3)
    data$rooms <- gsub("[^[:digit:]]","",data$rooms)
    ##
    data$size <- sapply(x, "[[", 4)
    data$size <- as.numeric(sub(" m²","",data$size))
    data$price <- sub(" €","",data$price)
    data$price <- as.numeric(gsub("\\.","",data$price)) * 10^(-3)
    ## ratio
    data$price_per_sqm <- data$price / data$size
    ## harmonize location
    for (i in 1:nrow(data)) {  
      if(!is.na(x[[i]][6])) data$location[i] <- x[[i]][6] else data$location[i] <- NA
    }
    data$location <- sub("E", "", data$location)
    data$location <- sub("r", "", data$location)
    data$location <- sub("\\(75016\\)", "16", data$location)
    data$location <- sub("\\(75116\\)", "16", data$location)
    data$location <- as.numeric(data$location)

    ## extract information from description #
    ## location
    location2_start <- str_locate(data[,4], format(Sys.time(), "%Y"))
    location2_stop <- str_locate(data[,4], "\\.")
    for (i in 1:nrow(data)) {  
      data$location2_[i] <- substr(data[i,4], start = location2_start[i,2]+1, stop = location2_stop[i,1]-1)
    }
    ## harmonize location2
    data$location2 <- gsub("[^[:digit:]]","",data$location2)
    data$location2 <- sub("75116","16",data$location2)
    data$location2[data$location2=="" | is.na(data$location2)] <- "75"
    data$location2 <- as.numeric(data$location2)
    ## floor
    data$floor <- NA
    data$floor[is.na(data$floor)] <- str_extract(data[,4], "au [0-9]e")
    data$floor[is.na(data$floor)] <- str_extract(data[,4], "[0-9]e étage")
    data$floor[is.na(data$floor)] <- str_extract(data[,4], "dernier étage")
    data$floor <- gsub("dernier","99", data$floor)
    data$floor <- gsub("[^[:digit:]]","",data$floor)
    ## elevator
    data$ascenseur <- TRUE
    data$ascenseur[str_detect(data[,4], ignore.case("sans ascenseur"))==TRUE | str_detect(data[,4], ignore.case("ascenseur"))==FALSE] <- FALSE
    ## cellar
    data$cave <- FALSE
    data$cave[str_detect(data[,4], ignore.case("cave"))==TRUE] <- TRUE
    ## renovated
    data$refait <- FALSE
    data$refait[str_detect(data[,4], ignore.case("refait"))==TRUE] <- TRUE
    data$refait[str_detect(data[,4], ignore.case("rénové"))==TRUE] <- TRUE
    ## haussmannien
    data$haussmanien <- FALSE
    data$haussmanien[str_detect(data[,4], ignore.case("haussmanien"))==TRUE] <- TRUE
    ## balcony
    data$balcon <- FALSE
    data$balcon[str_detect(data[,4], ignore.case("balcon"))==TRUE] <- TRUE

    ## remove information
    data[,4] <- sub("Voir site perso","",data[,4])
    data[,4] <- sub("Agences s'abstenir","",data[,4])
    data[,4] <- sub("Mise à jour le ","",data[,4])
    data[,4] <- sub("Nouvelle du ","",data[,4])

    x <- strsplit(data[,4], split = " ")
head(x)
    french.months <- c('janvier', 'février', 'mars', 'avril', 'mai', 'juin', 'juillet', 'août', 'septembre', 'octobre', 'novembre', 'décembre')
    data$date <- paste0(substr(sapply(x, '[[', 4), 1, 4), '-', sprintf(match(sapply(x, '[[', 3), french.months), fmt = "%02d"), '-', sapply(x, '[[', 2))
    data$date <- as.Date(data$date)

    data[,4] <- sub(format(Sys.time(), "%Y"),"",data[,4])
    
    data$metro <- sub("suite","",str_extract(data[,4], "suite.+"))
    ##
    data <- data[order(data$price_per_sqm),]
    ##
    return(data)
  }

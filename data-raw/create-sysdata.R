library(magrittr)

providerConf <- list()

providerConf$papfr <-
  file.path("data-raw", "provider_pap_fr.json") %>%
  jsonlite::fromJSON()

## str(provider)

save(list=c("providerConf"), file = file.path("R", "sysdata.rda"))

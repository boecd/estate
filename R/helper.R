#' @importFrom magrittr %>%
#' @export

validType <- function(type) {
  permitted_types <- providerConf$papfr$part2
  if(!type %in% names(permitted_types))
    stop(paste("type must be one of"), permitted_types,
    ("and was"), type)
  else
    TRUE
}

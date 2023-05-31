#' Retrieves GDELT CAMEO type code book
#'
#' @return
#' @importFrom magrittr %>%
#' @importFrom readr read_tsv
#' @export
#'
#' @examples
#' get_codes_cameo_type()
get_codes_cameo_type <- function() {
  url <- 'http://gdeltproject.org/data/lookups/CAMEO.type.txt'
  code_df <-
    url %>%
    read_tsv() %>%
    suppressWarnings() %>%
    suppressMessages()

  names(code_df) <-
    c('codeCAMEOType', 'nameCAMEOType')

  code_df
}

#' Retrieves CAMEO CAMEO event code book
#'
#'
#' @return
#' @export
#' @import dplyr readr stringr purrr tidyr
#' @examples
#' get_codes_cameo_events()

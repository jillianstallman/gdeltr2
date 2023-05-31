#' Retrieves GDELT CAMEO country code book
#'
#'
#' @return
#' @importFrom magrittr %>%
#' @importFrom readr read_tsv
#' @export
#'
#' @examples
#' get_codes_cameo_country()
get_codes_cameo_country <- function() {
  url <-
    'http://gdeltproject.org/data/lookups/CAMEO.country.txt'
  code_df <-
    url %>%
    read_tsv() %>%
    suppressWarnings() %>%
    suppressMessages()

  names(code_df) <-
    c('codeISO', 'nameCountry')

  code_df
}
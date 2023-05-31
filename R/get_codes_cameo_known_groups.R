#' Retrieves GDELT CAMEO known group code book
#'
#' @return
#' @importFrom readr read_tsv
#' @importFrom magrittr %>%
#' @export
#' @examples
#' get_codes_cameo_known_groups()
get_codes_cameo_known_groups <- function() {
  url <-
    'http://gdeltproject.org/data/lookups/CAMEO.knowngroup.txt'
  code_df <-
    url %>%
    read_tsv() %>%
    suppressWarnings() %>%
    suppressMessages()

  names(code_df) <-
    c('codeCAMEOGroup', 'nameCAMEOGroup')

  return(code_df)
}
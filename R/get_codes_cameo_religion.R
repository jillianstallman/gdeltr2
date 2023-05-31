#' Retrives GDELT CAMEO religion code book
#'
#'
#' @return
#' @export
#' @importFrom magrittr %>%
#' @importFrom readr read_tsv
#' @examples
#' get_codes_cameo_religion()
get_codes_cameo_religion <- function() {
  url <-
    'http://gdeltproject.org/data/lookups/CAMEO.religion.txt'
  code_df <-
    url %>%
    read_tsv() %>%
    suppressWarnings() %>%
    suppressMessages()

  names(code_df) <-
    c('codeCAMEOReligion', 'nameCAMEOReligion')

  return(code_df)
}

#' Retrieves GDELT CAMEO ethnic code book
#'
#' @return
#' @export
#' @import dplyr readr stringr purrr tidyr
#' @examples
#' get_codes_cameo_ethnic()
get_codes_cameo_ethnic <- function() {
  url <-
    'http://gdeltproject.org/data/lookups/CAMEO.ethnic.txt'
  code_df <-
    url %>%
    read_tsv() %>%
    suppressWarnings() %>%
    suppressMessages() %>%
    set_names(c('codeCAMEOEthnicity', 'nameCAMEOEthnicity')) %>%
    mutate(codeCAMEOEthnicity = codeCAMEOEthnicity %>% str_to_upper())

  return(code_df)
}
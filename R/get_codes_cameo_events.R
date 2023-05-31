get_codes_cameo_events <- function() {
  url <-
    'http://gdeltproject.org/data/lookups/CAMEO.eventcodes.txt'
  code_df <-
    url %>%
    read_tsv() %>%
    suppressWarnings() %>%
    suppressMessages()

  names(code_df) <-
    c('idCAMEOEvent', 'descriptionCAMEOEvent')

  code_df <-
    code_df %>%
    dplyr::mutate(
      isParentCode = ifelse(idCAMEOEvent %>% nchar() == 2, T, F),
      idParentCode = idCAMEOEvent %>% substr(1, 2)
    ) %>%
    dplyr::select(idParentCode, everything()) %>%
    mutate_at(c("idParentCode", "idCAMEOEvent"),
              funs(as.numeric))

  return(code_df)
}
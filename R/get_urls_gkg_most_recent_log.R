#' Gets most recent GKG log URLs
#'
#' @return
#' @import stringr
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom readr read_tsv
#' @importFrom tidyr separate
#' @export
#'
#' @examples
#' get_urls_gkg_most_recent_log()
get_urls_gkg_most_recent_log <- function() {
  log_df <-
    'http://data.gdeltproject.org/gdeltv2/lastupdate.txt' %>%
    read_tsv(col_names = F) %>%
    separate(
      col = X1,
      into = c('idFile', 'idHash', 'urlData'),
      sep = '\\ '
    ) %>%
    suppressWarnings() %>%
    suppressMessages()

  log_df <-
    log_df %>%
    dplyr::mutate(dateTimeFile = urlData %>% str_replace_all('http://data.gdeltproject.org/gdeltv2/', '')) %>%
    separate(dateTimeFile,
             into = c('timestamp', 'nameFile', 'typeFile', 'isZip')) %>%
    dplyr::mutate(
      typeFile = typeFile %>% str_to_lower(),
      isZip = ifelse(isZip %>% str_detect("ZIP|zip"), T, F),
      idFile = idFile %>% as.integer()
    ) %>%
    mutate_at(c('idHash', 'nameFile', 'urlData'),
              funs(str_trim))

  return(log_df)
}
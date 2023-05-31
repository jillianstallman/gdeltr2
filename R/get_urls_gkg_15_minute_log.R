#' Loads gdelt v2 Global Knowledge Graph master log data, updated every 15 minutes
#'
#' @return
#' @export
#' @import dplyr
#' @import stringr
#' @importFrom lubridate ymd_hms
#' @importFrom lubridate with_tz
#' @importFrom readr read_tsv
#' @importFrom magrittr %>%
#' @importFrom tidyr separate
#' @examples
#' get_urls_gkg_15_minute_log()
get_urls_gkg_15_minute_log <- function() {
  url <-
    'http://data.gdeltproject.org/gdeltv2/masterfilelist.txt'

  log_df <-
    url %>%
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
             into = c('timestamp', 'nameFile', 'typeFile', 'isZip'))

  log_df <-
    log_df %>%
    dplyr::mutate(
      dateTimeData = timestamp %>% as.numeric() %>% lubridate::ymd_hms() %>% lubridate::with_tz(Sys.timezone()),
      dateData = dateTimeData %>% as.Date(),
      typeFile = typeFile %>% str_to_lower(),
      idFile = idFile %>% as.integer()
    ) %>%
    dplyr::mutate_at(.vars = c('idHash', 'nameFile', 'urlData'),
                     .funs = str_trim) %>%
    suppressWarnings()

  return(log_df)

}
#' Gets GDELT Event data, by year from 1979-2005, by year month 2006 - 2013, then by dat
#'
#' @param return_message
#'
#' @return
#' @import stringr
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom readr read_tsv
#' @importFrom tidyr separate
#' @importFrom lubridate ymd
#' @export
#'
#' @examples
#' get_urls_gdelt_event_log()

get_urls_gdelt_event_log <- function(return_message = T) {
  url <-
    'http://data.gdeltproject.org/events/md5sums'

  urlData <-
    url %>%
    read_tsv(col_names = F) %>%
    separate(col = X1,
             into = c('idHash', 'stemData'),
             sep = '\\  ') %>%
    dplyr::mutate(
      urlData = 'http://data.gdeltproject.org/events/' %>% paste0(stemData),
      slugDatabaseGDELT = 'EVENTS',
      isZipFile = ifelse(stemData %>% str_detect(".zip"), T, F)
    ) %>%
    suppressWarnings() %>%
    suppressMessages()

  urlData <-
    urlData %>%
    separate(
      col = stemData,
      into = c('periodData', 'nameFile', 'typeFile', 'zip_file'),
      sep = '\\.'
    ) %>%
    dplyr::select(-zip_file) %>%
    dplyr::mutate(
      periodData = ifelse(periodData == 'GDELT', typeFile, periodData),
      isDaysData = ifelse(periodData %>% nchar == 8, T, F)
    ) %>%
    dplyr::select(-c(nameFile, typeFile)) %>%
    suppressWarnings()

  urlData <-
    urlData %>%
    dplyr::filter(isDaysData == F) %>%
    dplyr::mutate(dateData = NA) %>%
    bind_rows(
      urlData %>%
        dplyr::filter(isDaysData == T) %>%
        dplyr::mutate(dateData = periodData %>% lubridate::ymd() %>% as.Date())
    ) %>%
    dplyr::select(idHash,
                  dateData,
                  isZipFile,
                  isDaysData,
                  urlData,
                  everything())

  if (return_message) {
    count.files <-
      urlData %>%
      nrow

    min.date <-
      urlData$dateData %>% min(na.rm = T)

    max.date <-
      urlData$dateData %>% max(na.rm = T)

    "You got " %>%
      paste0(count.files,
             ' GDELT Global Knowledge Graph URLS from ',
             min.date,
             ' to ',
             max.date) %>%
      cat(fill = T)
  }

  return(urlData)
}
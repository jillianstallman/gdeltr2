#' Collects all the URLS for the GDELT Event Data.
#'
#' These are by year from 1979-2005, by month from January 2006 to March 2013 (inclusive), then by day
#' starting from 2023-04-01 to the current day.
#'
#' The URL can be found by clicking:
#' https://www.gdeltproject.org/data.html#rawdatafiles
#' then "Download GDELT 1.0 Events" to lead to
#' http://data.gdeltproject.org/events/index.html
#' and finally clicking "md5sums" to yield
#' http://data.gdeltproject.org/events/md5sums
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

  # read the data into hash, filename (e.g. 1979.zip), get URL, database (EVENTS), and whether a ZIP file (TRUE)
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

  #
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
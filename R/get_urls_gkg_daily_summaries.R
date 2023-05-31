#' Gets Global Knowledge Graph summary files by day since April 2013
#'
#' @param remove_count_files
#' @param return_message
#'
#' @return
#' @export
#' @import stringr
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom readr read_tsv
#' @importFrom lubridate ymd
#' @importFrom tidyr separate
#' @examples
#' get_urls_gkg_daily_summaries(remove_count_files = T)
get_urls_gkg_daily_summaries <-
  function(remove_count_files = F,
           return_message = T) {
    url <-
      'http://data.gdeltproject.org/gkg/md5sums'

    urlData <-
      url %>%
      read_tsv(col_names = F) %>%
      separate(col = X1,
               into = c('idHash', 'stemData'),
               sep = '\\  ') %>%
      dplyr::mutate(urlData = 'http://data.gdeltproject.org/gkg/' %>% paste0(stemData),
                    slugDatabaseGDELT = 'gkg') %>%
      separate(
        col = stemData,
        into = c('dateData', 'nameFile', 'typeFile', 'isZipFile'),
        sep = '\\.'
      ) %>%
      dplyr::mutate(
        isZipFile = ifelse(isZipFile == "zip", T, F),
        isCountFile = ifelse(nameFile == 'gkgcounts', T, F),
        dateData = dateData %>% lubridate::ymd() %>% as.Date()
      ) %>%
      dplyr::select(-c(nameFile, typeFile)) %>%
      dplyr::select(idHash,
                    dateData,
                    isZipFile,
                    isCountFile,
                    urlData,
                    everything()) %>%
      suppressWarnings() %>%
      suppressMessages()

    if (remove_count_files) {
      urlData <-
        urlData %>%
        dplyr::filter(isCountFile == F)
    }

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
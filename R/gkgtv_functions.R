#' Gets most recent GKG TV log URLs
#'
#' @return
#' @import stringr
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom readr read_tsv
#' @importFrom tidyr separate
#' @export
#'
#' @examples get_urls_gkg_most_recent_log()
get_urls_gkgtv_most_recent_log <- function() {
  urlData <-
    'http://data.gdeltproject.org/gdeltv2_iatelevision/lastupdate.txt' %>%
    read_tsv(col_names = F) %>%
    dplyr::select(-1) %>%
    suppressMessages()

  names(urlData) <-
    c('idHash', 'urlData')

  urlData <-
    urlData %>%
    mutate(
      dateData = urlData %>% str_replace_all(
        'http://data.gdeltproject.org/gdeltv2_iatelevision/|.gkg.csv.gz',
        ''
      ) %>% as.Date('%Y%m%d')
    ) %>%
    dplyr::select(dateData, everything())

  return(urlData)
}

#' Retrieves GKG TV Schema
#'
#' @return
#' @importFrom dplyr tibble
#' @examples
#' get_tv_schema

get_tv_schema  <- function() {
  tv_schema <-
    tibble(
      nameGDELT = c(
        "GKGRECORDID",
        "DATE",
        "SourceCollectionIdentifier",
        "SourceCommonName",
        "DocumentIdentifier",
        "Counts",
        "V2Counts",
        "Themes",
        "V2Themes",
        "Locations",
        "V2Locations",
        "Persons",
        "V2Persons",
        "Organizations",
        "V2Organizations",
        "V2Tone",
        "Dates",
        "GCAM",
        "SharingImage",
        "RelatedImages",
        "SocialImageEmbeds",
        "SocialVideoEmbeds",
        "Quotations",
        "AllNames",
        "Amounts",
        "TranslationInfo",
        "Extras"
      ),
      nameActual = c(
        "idGKG",
        "dateDocument",
        "idSourceCollectionIdentifier",
        "nameSource",
        "documentSource",
        "counts",
        "countsCharLoc",
        "themes",
        "themesCharLoc",
        "locations",
        "locationsCharLoc",
        "persons",
        "personsCharLoc",
        "organizations",
        "organizationsCharLoc",
        "tone",
        "dates",
        "gcam",
        "urlImage",
        "urlImageRelated",
        "urlSocialMediaImageEmbeds",
        "urlSocialMediaVideoEmbeds",
        "quotations",
        "mentionedNamesCounts",
        "mentionedNumericsCounts",
        "translationInfo",
        "xmlExtras"
      )
    )
  return(tv_schema)

}
#' Gets Global Knowledge Graph TV Summary Files
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
dictionary_gkg_tv_daily_summaries <-
  function(return_message = T) {
    url <-
      'http://data.gdeltproject.org/gdeltv2_iatelevision/masterfilelist.txt'

    urlData <-
      url %>%
      read_tsv(col_names = F) %>%
      dplyr::select(-1) %>%
      suppressMessages()

    names(urlData) <-
      c('idHash', 'urlData')

    urlData <-
      urlData %>%
      mutate(
        dateData = urlData %>% str_replace_all(
          'http://data.gdeltproject.org/gdeltv2_iatelevision/|.gkg.csv.gz',
          ''
        ) %>% as.Date('%Y%m%d')
      ) %>%
      dplyr::select(dateData, everything())

    if (return_message) {
      count.files <-
        urlData %>%
        nrow()

      min.date <-
        urlData$dateData %>% min(na.rm = T)

      max.date <-
        urlData$dateData %>% max(na.rm = T)

      "You got " %>%
        paste0(count.files,
               ' GDELT TV Global Knowledge Graph URLS from ',
               min.date,
               ' to ',
               max.date) %>%
        cat(fill = T)
    }

    return(urlData)
  }

.get_data_gkg_tv <-
  function(url = 'http://data.gdeltproject.org/gdeltv2_iatelevision/20160609.gkg.csv.gz',
           return_message = T) {
    ok_url <-
      url %>% url_ok() %>% suppressWarnings()
    if (ok_url == FALSE) {
      stop("Invalid url")
    }

    gkg_tv_data <-
      url %>%
      curl() %>%
      gzcon() %>%
      read_tsv(col_names = F) %>%
      suppressWarnings() %>%
      suppressMessages()

    names(gkg_tv_data) <-
      get_tv_schema() %>% .$nameActual

    gkg_tv_data <-
      gkg_tv_data %>%
      dplyr::mutate(
        idSourceCollectionIdentifier = idSourceCollectionIdentifier %>% as.numeric(),
        isDocumentURL = ifelse(documentSource %>% str_detect('http'), T, F)
      ) %>%
      dplyr::select(idGKG:idSourceCollectionIdentifier,
                    isDocumentURL,
                    everything()) %>%
      dplyr::rename(dateTimeDocument = dateDocument) %>%
      dplyr::mutate(dateTimeDocument = dateTimeDocument %>% lubridate::ymd_hms() %>% with_tz(Sys.timezone())) %>%
      suppressMessages() %>%
      suppressWarnings()

    gkg_tv_data <-
      gkg_tv_data %>%
      mutate(docDetails = documentSource %>% sub('\\_', '\\-', .) %>% sub('\\_', '\\-', .)  %>% sub('\\_', '\\-', .)) %>%
      separate(
        docDetails,
        sep = '\\-',
        into = c('idTVNetwork', 'date', 'time', 'nameTVShow')
      ) %>%
      dplyr::select(-c(date, time)) %>%
      mutate(
        nameTVShow = nameTVShow %>% str_replace_all('\\_', ' '),
        urlArchiveVideo = documentSource %>% paste0('https://archive.org/details/', .)
      ) %>%
      dplyr::select(idGKG:documentSource,
                    urlArchiveVideo,
                    idTVNetwork,
                    nameTVShow,
                    everything())

    if (return_message) {
      "Downloaded, parsed and imported " %>%
        paste0(url) %>%
        cat(fill = T)

    }
    return(gkg_tv_data)
  }

.get_data_gkg_tv_day <- function(date_data = "2016-06-01",
                                 only_most_recent = F,
                                 return_message = T) {
  if (only_most_recent) {
    date_data <-
      Sys.Date() - 2
  }

  if (!date_data %>% substr(5, 5) == "-") {
    stop("Sorry data must be in YMD format, ie, 2016-06-01")
  }

  date_data <-
    date_data %>%
    ymd %>% as.Date()


  if (date_data < "2009-06-04") {
    stop("Sorry data starts on June 4th, 2009")
  }

  if (date_data > Sys.Date() - 2) {
    stop("Sorry data can't go into the future is on a 2 day lag")
  }

  if (only_most_recent) {
    gkg_recent <-
      get_urls_gkgtv_most_recent_log()
    urls <-
      gkg_recent %>%
      .$urlData
  } else {
    if (!'gkg_tv_urls' %>% exists()) {
      gkg_tv_urls <-
        dictionary_gkg_tv_daily_summaries()

      assign(x = 'gkg_tv_urls', eval(gkg_tv_urls), env = .GlobalEnv)
    }

    urls <-
      gkg_tv_urls %>%
      dplyr::filter(dateData == date_data) %>%
      .$urlData
  }

  get_data_gkg_tv_safe <-
    possibly(.get_data_gkg_tv, tibble())

  all_data <-
    urls %>%
    map_dfr(function(x) {
      get_data_gkg_tv_safe(url = x,
                           return_message = return_message)
    }) %>%
    distinct() %>%
    suppressMessages() %>%
    suppressWarnings()

  if (return_message) {
    "You retrieved " %>%
      paste0(all_data %>% nrow, " gkg tv events for ", date_data) %>%
      cat(fill = T)
  }

  return(all_data)
}


#' Gets GKG TV data for specified days
#'
#' @param dates specified dates, year, month day format
#' @param only_most_recent returns only the most recent period
#' \code{c(T, F)}
#' @param return_message
#'
#' @return
#' @export
#' @import purrr
#' @examples
gkg_tv_days <-
  function(dates = c("2016-06-01", "2016-02-01"),
           only_most_recent = F,
           return_message = T) {
    get_data_gkg_tv_day_safe <-
      possibly(.get_data_gkg_tv_day, tibble())

    all_data <-
      dates %>%
      map_dfr(
        function(x)
          get_data_gkg_tv_day_safe(
            date_data = x,
            only_most_recent = only_most_recent,
            return_message = return_message
          )
      ) %>%
      suppressWarnings()

    return(all_data)
  }
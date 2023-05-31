#' Retrieves gkg summary file for a given day
#'
#' @param date_data Date of the data, must be in year, month day form.
#' @param file_directory location of where the file is to be saved
#' @param is_count_file options
#' \code{c(TRUE, FALSE)}
#' @param remove_files
#' \code{c(TRUE, FALSE)}
#' @param empty_trash Empty trash
#' \code{c(TRUE, FALSE)}
#' @param return_message
#' \code{c(TRUE, FALSE)}
#' @importFrom purrr flatten_chr
#' @importFrom readr parse_number
#' @importFrom purrr compact
#' @import dplyr utils dplyr purrr readr
#' @importFrom urltools domain
#' @importFrom curl curl_download curl
#' @return
#'
#' @examples

get_data_gkg_day_summary <- function(date_data = "2016-06-01",
                                     file_directory = NULL,
                                     folder_name = 'gdelt_data',
                                     is_count_file = F,
                                     remove_files = T,
                                     empty_trash = T,
                                     return_message = T) {
  options(scipen = 99999)
  if (!date_data %>% substr(5, 5) == "-") {
    stop("Sorry data must be in YMD format, ie, 2016-06-01")
  }

  date_data <-
    date_data %>%
    ymd %>% as.Date()


  if (date_data < "2013-04-01") {
    stop("Sorry data starts on April 1st, 2013")
  }

  if (date_data > Sys.Date()) {
    stop("Sorry data can't go into the future")
  }
  if (!'summary_data_urls' %>% exists()) {
    summary_data_urls <-
      get_urls_gkg_daily_summaries(return_message = return_message)

    assign(x = 'summary_data_urls', eval(summary_data_urls), env = .GlobalEnv)
  }

  if (is_count_file) {
    summary_data_urls <-
      summary_data_urls %>%
      dplyr::filter(isCountFile == T)
  } else {
    summary_data_urls <-
      summary_data_urls %>%
      dplyr::filter(isCountFile == F)
  }

  urls <-
    summary_data_urls %>%
    dplyr::filter(dateData == date_data) %>%
    .$urlData

  get_gdelt_url_data_safe <-
    possibly(get_gdelt_url_data, tibble())

  all_data <-
    urls %>%
    map_dfr(function(x) {
      get_gdelt_url_data_safe(
        url = x,
        remove_files = remove_files,
        file_directory = file_directory,
        folder_name = folder_name,
        return_message = return_message,
        empty_trash = empty_trash
      )
    }) %>%
    distinct()

  if ('countObject' %in% names(all_data)) {
    all_data <-
      all_data %>%
      dplyr::mutate(countObject = countObject %>% as.numeric())
  }
  if ('idCAMEOEvents' %in% names(all_data)) {
    all_data <-
      all_data %>%
      mutate(idCAMEOEvents = idCAMEOEvents %>% as.character())
  }

  if (is_count_file == F) {
    all_data <-
      all_data %>%
      dplyr::rename(dateEvent = date)
  }

  all_data <-
    all_data %>%
    mutate(isCountFile = if_else(is_count_file, T, F))


  if (return_message) {
    "You retrieved " %>%
      paste0(all_data %>% nrow(), " gkg summary events for ", date_data) %>%
      cat(fill = T)
  }

  return(all_data)
}
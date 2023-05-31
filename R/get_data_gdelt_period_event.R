#' Retreive GDELT data for a given period
#'
#' @param period the GDELT period
#' @param file_directory
#' @param remove_files
#' @param empty_trash
#' @param return_message
#' @importFrom purrr compact
#' @importFrom purrr flatten_chr
#' @importFrom readr parse_number
#' @importFrom purrr compact
#' @import dplyr
#' @import utils
#' @importFrom urltools domain
#' @importFrom curl curl_download curl
#' @return
#'
#' @examples
#' get_data_gdelt_period_event(period = 1983)
get_data_gdelt_period_event <- function(period = 1983,
                                        file_directory = NULL,
                                        folder_name = 'gdelt_data',
                                        remove_files = T,
                                        empty_trash = T,
                                        return_message = T) {
  period <-
    period %>%
    as.character() %>%
    str_replace_all("\\-", '')

  if (!'gdelt_event_urls' %>% exists()) {
    gdelt_event_urls <-
      get_urls_gdelt_event_log(return_message = return_message)

    assign(x = 'gdelt_event_urls', eval(gdelt_event_urls), env = .GlobalEnv)
  }
  periods <-
    gdelt_event_urls$periodData
  if (!period %in% periods) {
    gdelt_event_message <-
      "Period can only be a 4 digit year between 1979 and 2005\nEXAMPLE: 1983\nA 6 digit year and month from 2006 to March 2013\nEXAMPLE: 201208\nOr an 8 digit year, month, day from March 1, 2013 until today\nEXAMPLE: 20140303"

    stop(gdelt_event_message)
  }

  urls <-
    gdelt_event_urls %>%
    dplyr::filter(periodData == period) %>%
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
    })

  if (return_message) {
    "You retrieved " %>%
      paste0(all_data %>% nrow, " GDELT events for the period of ", period) %>%
      cat(fill = T)
  }

  return(all_data)
}
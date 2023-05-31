#' Returns GDELT event data for a given periods
#'
#' @param periods
#' @param file_directory
#' @param is_count_file
#' @param remove_files
#' @param empty_trash
#' @param return_message
#' @import dplyr stringr purrr tidyr readr lubridate
#' @return
#' @export
#'
#' @examples
#' get_data_gdelt_periods_event (periods = c(1983))
get_data_gdelt_periods_event <- function(periods = c(1983, 1989),
                                         file_directory = NULL,
                                         folder_name = 'gdelt_data',
                                         remove_files = T,
                                         empty_trash = T,
                                         return_message = T) {
  get_data_gdelt_period_event_safe <-
    possibly(get_data_gdelt_period_event, tibble())
  periods <-
    periods %>%
    str_replace_all('\\-', '')
  all_data <-
    seq_along(periods) %>%
    map_dfr(
      function(x)
        get_data_gdelt_period_event_safe(
          period = periods[x],
          file_directory = file_directory,
          folder_name = folder_name,
          remove_files = remove_files,
          empty_trash = empty_trash,
          return_message = return_message
        )
    ) %>%
    suppressWarnings()

  return(all_data)
}
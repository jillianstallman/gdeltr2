#' Gets days summary GDELT GKG data by table
#'
#' @param dates
#' @param file_directory
#' @param remove_files
#' @param empty_trash
#' @param return_message
#'
#' @return
#' @export
#' @examples
#' get_data_gkg_days_summary(dates = c("2016-07-18"), is_count_file = c(T, F))

get_data_gkg_days_summary <- function(dates = c("2016-06-01"),
                                      is_count_file = c(T, F),
                                      file_directory = NULL,
                                      folder_name = 'gdelt_data',
                                      remove_files = T,
                                      empty_trash = T,
                                      nest_data = F,
                                      return_message = T) {
  get_data_gkg_day_summary_safe <-
    possibly(get_data_gkg_day_summary, tibble())

  var_matrix <-
    expand.grid(
      date = dates,
      is_count_file = is_count_file,
      stringsAsFactors = F
    ) %>%
    as_tibble %>%
    suppressWarnings()

  all_data <-
    seq_len(var_matrix %>% nrow) %>%
    map_dfr(
      function(x)
        get_data_gkg_day_summary_safe(
          date_data = var_matrix$date[x],
          is_count_file = var_matrix$is_count_file[x],
          file_directory = file_directory,
          folder_name = folder_name,
          remove_files = remove_files,
          empty_trash = empty_trash,
          return_message = return_message
        ) %>% suppressWarnings()
    ) %>%
    arrange(idGKG) %>%
    suppressWarnings()

  return(all_data)
}
#' Convert date columns to numeric
#'
#' @param data
#' @param column_keyword Keyword of the data frame's columns to look for that signifies dates
#' \code{c('date')}
#' @param exclude_col_type The types of column to exclude from converting to numeric
#' \code{c('character')}
#' @param return_message  return a message
#' @importFrom tibble rownames_to_column
#' @return
#' @export
#'
#' @examples
date_columns_to_numeric <-
  function(data,
           column_keyword = 'date',
           exclude_col_type = c('character'),
           return_message = T) {
    name_df <-
      data %>%
      dplyr::select(dplyr::matches(column_keyword)) %>%
      map(class) %>%
      unlist() %>%
      data.frame(col_class = .) %>%
      rownames_to_column()

    exclude_cols <-
      name_df %>%
      dplyr::filter(col_class %in% col_type) %>%
      .$rowname

    mutate_cols <-
      data %>%
      dplyr::select(dplyr::matches(column_keyword)) %>%
      dplyr::select(-one_of(exclude_cols)) %>% names

    data <-
      data %>%
      mutate_at(mutate_cols, as.numeric)

    if (return_message) {
      "You convereted " %>%
        paste0(paste0(mutate_cols, collapse = ', '), ' to numeric') %>%
        cat(fill = T)
    }

    return(data)

  }



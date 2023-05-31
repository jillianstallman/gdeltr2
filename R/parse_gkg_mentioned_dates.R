

#' Returns mentioned dates from gkg data frame
#'
#' @param gdelt_data
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
parse_gkg_mentioned_dates <- function(gdelt_data,
                                      filter_na = T,
                                      return_wide = T) {
  parse_dates <-
    function(field = "4#6#16#0#734;4#4#26#0#2258",
             return_wide = F) {
      options(scipen = 99999, digits = 5)
      if (field %>% is.na) {
        if (return_wide) {
          field_data <-
            tibble(idDateResolution = NA)
        } else {
          field_data <-
            tibble(idDateResolution = NA,
                   idDateArticle = NA)
        }
      }  else {
        fields <-
          field %>%
          str_split('\\;') %>%
          flatten_chr() %>%
          .[!. %in% '']

        fields_df <-
          tibble(field_item = fields) %>%
          dplyr::mutate(idDateArticle = 1:n()) %>%
          separate(
            col = field_item,
            sep = '\\#',
            into = c('idDateResolution', 'month', 'day', 'year', 'charLoc')
          ) %>%
          suppressMessages() %>%
          suppressWarnings()

        fields_df <-
          fields_df %>%
          mutate_at(fields_df  %>% names(),
                    funs(. %>% as.numeric())) %>%
          dplyr::left_join(tibble(
            idDateResolution = 1:4,
            dateResolution = c(
              'ex_mon_date',
              'year_only',
              'month_date' ,
              'fully_resolved'
            )
          )) %>%
          suppressMessages() %>%
          dplyr::select(idDateResolution, dateResolution, everything())

        if (return_wide) {
          fields_df <-
            fields_df %>%
            gather(item, value, -idDateArticle) %>%
            arrange(idDateArticle) %>%
            unite(item, item, idDateArticle, sep = '.')

          order_fields <-
            fields_df$item

          field_data <-
            fields_df %>%
            spread(item, value) %>%
            dplyr::select_(.dots = order_fields)

          field_data <-
            field_data %>%
            mutate_at(field_data %>% dplyr::select(dplyr::matches(
              "idDateResolution|month|day|year|charLoc"
            )) %>% names(),
            funs(. %>% as.numeric()))

        } else {
          field_data <-
            fields_df

          field_data <-
            field_data %>%
            dplyr::select(idDateArticle, everything())
        }
      }

      return(field_data)
    }

  if (!'dates' %in% names(gdelt_data)) {
    stop("Sorry missing date column")
  }
  counts_data <-
    gdelt_data %>%
    dplyr::select(idGKG, dates)

  if (filter_na) {
    counts_data <-
      counts_data %>%
      dplyr::filter(!dates %>% is.na)
  }

  all_counts <-
    seq_along(counts_data$dates) %>%
    map_dfr(function(x) {
      parse_dates(field = counts_data$dates[x], return_wide =  F) %>%
        dplyr::mutate(idGKG = counts_data$idGKG[x])
    }) %>%
    dplyr::select(idGKG, everything()) %>%
    dplyr::select(-idDateResolution)

  all_counts <-
    all_counts %>%
    get_clean_count_data(count_col = 'idDateArticle', return_wide = return_wide) %>%
    separate(
      idGKG,
      into = c('GKG', 'dateTime'),
      sep = '\\-',
      remove = F
    ) %>%
    mutate(dateTime = dateTime %>% as.numeric) %>%
    arrange(dateTime) %>%
    dplyr::select(-c(dateTime, GKG)) %>%
    suppressWarnings()

  if (!return_wide) {
    all_counts <-
      all_counts %>%
      .resolve_long_names()
  }

  return(all_counts)
}
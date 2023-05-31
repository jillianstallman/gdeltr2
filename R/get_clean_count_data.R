#' Gets clean count data
#'
#' @param all_counts
#' @param count_col
#' @param return_wide
#'
#' @return
#'
#' @examples
get_clean_count_data <-
  function(all_counts,
           extra_key = NA,
           count_col = 'idArticle.tone',
           return_wide = F) {
    if (!extra_key %>% is.na()) {
      clean_data <-
        all_counts %>%
        dplyr::rename_(count = count_col,
                       ek = extra_key) %>%
        group_by(idGKG) %>%
        mutate(count = 1:n()) %>%
        ungroup %>%
        mutate(count = count - 1) %>%
        gather(item, value, -c(idGKG, count, ek), na.rm = T) %>%
        mutate(item = ifelse(count == 0, item, item %>% paste0(count)),
               value = value %>% str_trim) %>%
        separate(
          idGKG,
          into = c('GKG', 'dateTime'),
          sep = '\\-',
          remove = F
        ) %>%
        mutate(dateTime = dateTime %>% as.numeric) %>%
        arrange(dateTime, count) %>%
        dplyr::select(-c(dateTime, GKG))
    } else {
      clean_data <-
        all_counts %>%
        dplyr::rename_(count = count_col) %>%
        group_by(idGKG) %>%
        mutate(count = 1:n()) %>%
        ungroup %>%
        mutate(count = count - 1)
      if ('charLoc' %in% names(clean_data)) {
        clean_data <-
          clean_data %>%
          gather(item, value, -c(idGKG, count, charLoc), na.rm = T) %>%
          mutate(
            item = ifelse(count == 0, item, item %>% paste0(count)),
            value = value %>% str_trim
          ) %>%
          distinct() %>%
          separate(
            idGKG,
            into = c('GKG', 'dateTime'),
            sep = '\\-',
            remove = F
          ) %>%
          mutate(dateTime = dateTime %>% as.numeric) %>%
          arrange(dateTime, count) %>%
          dplyr::select(-c(dateTime, GKG))
      } else {
        clean_data <-
          clean_data %>%
          gather(item, value, -c(idGKG, count), na.rm = T) %>%
          mutate(
            item = ifelse(count == 0, item, item %>% paste0(count)),
            value = value %>% str_trim
          ) %>%
          distinct() %>%
          separate(
            idGKG,
            into = c('GKG', 'dateTime'),
            sep = '\\-',
            remove = F
          ) %>%
          mutate(dateTime = dateTime %>% as.numeric) %>%
          arrange(dateTime, count) %>%
          dplyr::select(-c(dateTime, GKG))
      }
    }

    if (return_wide) {
      if (!extra_key %>% is.na()) {
        names_order <-
          c('idGKG', 'ek',  clean_data$item %>% unique)
      } else {
        names_order <-
          c('idGKG',  clean_data$item %>% unique)
      }

      clean_data <-
        clean_data %>%
        dplyr::select(-count) %>%
        spread(item, value)

      keywords <-
        c(
          'amountValue',
          'latitude',
          'scoreGoldstein',
          'articleWordCount',
          'longitude',
          'countEvent',
          'idTypeLocation',
          'charLoc',
          'length',
          'count',
          'month',
          'day',
          'year',
          'score'
        )

      length_nums <-
        clean_data %>%
        dplyr::select(dplyr::matches(keywords %>% paste0(collapse = '|'))) %>%
        dplyr::select(-dplyr::matches('namePerson|idCountry')) %>%
        names %>% length

      if (length_nums > 0) {
        numeric_vars <-
          clean_data %>%
          dplyr::select(dplyr::matches(keywords %>% paste0(collapse = '|'))) %>%
          dplyr::select(-dplyr::matches('namePerson|idCountry')) %>%
          names

        clean_data <-
          clean_data %>%
          dplyr::mutate_at(numeric_vars,
                           funs(. %>% as.numeric()))

      }

      clean_data <-
        clean_data %>%
        dplyr::select_(.dots = names_order) %>%
        separate(
          idGKG,
          into = c('GKG', 'dateTime'),
          sep = '\\-',
          remove = F
        ) %>%
        mutate(dateTime = dateTime %>% as.numeric) %>%
        arrange(dateTime) %>%
        dplyr::select(-c(GKG, dateTime))

      if (!extra_key %>% is.na()) {
        names(clean_data)[2] <-
          extra_key
      }
    } else {
      clean_data <-
        clean_data %>%
        dplyr::select(-count) %>%
        separate(
          idGKG,
          into = c('GKG', 'dateTime'),
          sep = '\\-',
          remove = F
        ) %>%
        mutate(dateTime = dateTime %>% as.numeric) %>%
        arrange(dateTime) %>%
        dplyr::select(-c(GKG, dateTime))

    }

    return(clean_data)
  }
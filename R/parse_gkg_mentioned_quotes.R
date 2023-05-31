#' Returns mentioned quotes from a gkg data frame
#'
#' @param gdelt_data
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
parse_gkg_mentioned_quotes <-
  function(gdelt_data,
           filter_na = T,
           return_wide = T) {
    parse_quotes <-
      function(field = "495|51||knowingly aided and abetted an international kidnap#865|50||nothing less than an international child abduction#2764|49|| staff member should be singled out for dismissal#3373|48||make any serious attempt to independently verify#4059|46||wants to go through every single little detail#4802|156||And xC2 ; xA0 ; you're keeping all of xC2 ; xA0 ; them xC2 ; xA0 ; - xC2 ; xA0 ; except one sacrificial lamb - xC2 ; xA0 ; to run the show?#4879|28||How do you think that looks?#6093|60||an extraordinary conspiracy to remove the children illegally#6150|50||nothing less than an international child abduction#6828|408||I xE2 ; x80 ; xA6 ; have found nothing that supports a finding that any Australian Government official somehow knowingly assisted the mother to do something that was wrong xE2 ; x80 ; xA6 ; I do not find xE2 ; x80 ; xA6 ; that any Australian Embassy officials who helped the mother did so knowing that the mother did not have the father consent to remove the girls permanently from Italy",
               return_wide = F) {
        options(scipen = 99999, digits = 5)
        if (field %>% is.na) {
          if (return_wide) {
            field_data <-
              tibble(idArticleQuote = NA)
          } else {
            field_data <-
              tibble(quote = NA,
                     idArticleQuote = NA)
          }
        }  else {
          fields <-
            field %>%
            str_split('\\#') %>%
            flatten_chr() %>%
            .[!. %in% '']

          fields_df <-
            tibble(quote_items = fields) %>%
            dplyr::mutate(idArticleQuote = 1:n()) %>%
            separate(
              col = quote_items,
              sep = '\\|',
              into = c('charLoc', 'lengthQuote', 'verbIntro', 'textQuote')
            ) %>%
            suppressMessages() %>%
            suppressWarnings()

          fields_df <-
            fields_df %>%
            dplyr::mutate(textQuote = textQuote %>% str_trim) %>%
            mutate_at(c('charLoc', 'lengthQuote'),
                      funs(. %>% as.numeric())) %>%
            dplyr::select(idArticleQuote, everything())

          fields_df$verbIntro[fields_df$verbIntro == ''] <-
            NA

          if (return_wide) {
            fields_df <-
              fields_df %>%
              gather(item, value, -idArticleQuote) %>%
              arrange(idArticleQuote) %>%
              unite(item, item, idArticleQuote, sep = '.')

            order_fields <-
              fields_df$item

            field_data <-
              fields_df %>%
              spread(item, value) %>%
              dplyr::select_(.dots = order_fields)

            field_data <-
              field_data %>%
              mutate_at(field_data %>% dplyr::select(dplyr::matches("charLoc|lengthQuote")) %>% names(),
                        funs(. %>% as.numeric()))

          } else {
            field_data <-
              fields_df

            field_data <-
              field_data %>%
              dplyr::select(idArticleQuote, everything())
          }
        }

        return(field_data)
      }

    if (!'quotations' %in% names(gdelt_data)) {
      stop("Sorry missing quotations column")
    }

    counts_data <-
      gdelt_data %>%
      dplyr::select(idGKG, quotations)

    if (filter_na) {
      counts_data <-
        counts_data %>%
        dplyr::filter(!quotations %>% is.na())
    }

    all_counts <-
      seq_along(counts_data$quotations) %>%
      map_dfr(function(x) {
        parse_quotes(field = counts_data$quotations[x], return_wide = F) %>%
          dplyr::mutate(idGKG = counts_data$idGKG[x])
      }) %>%
      dplyr::select(idGKG, everything())

    if (all_counts$verbIntro %>% is.na() %>% as.numeric() %>% sum == nrow(all_counts)) {
      all_counts <-
        all_counts %>%
        dplyr::select(-verbIntro)
    }

    all_counts <-
      all_counts %>%
      get_clean_count_data(count_col = 'idArticleQuote', return_wide = T) %>%
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
        gather(item, value, -idGKG, na.rm = TRUE) %>%
        .resolve_long_names()
    }

    return(all_counts)
  }
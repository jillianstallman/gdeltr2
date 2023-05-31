#' Returns article tones from a gkg data frame
#'
#' @param gdelt_data
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
parse_gkg_mentioned_article_tone <-
  function(gdelt_data,
           filter_na = T,
           return_wide = T) {
    parse_article_tones <-
      function(field = "-4.65116279069767,1.55038759689922,62015503875969,7.75193798449612,13.1782945736434,0,134",
               return_wide = F) {
        options(scipen = 99999, digits = 5)
        if (field %>% is.na) {
          if (return_wide) {
            field_data <-
              tibble(amount.tone = NA)
          } else {
            field_data <-
              tibble(amount.tone = NA, idArticle.tone = 1)
          }
        }  else {
          fields <-
            field %>%
            str_split('\\,') %>%
            flatten_chr() %>%
            .[!. %in% ''] %>%
            as.numeric()

          fields_df <-
            tibble(amount.tone = fields) %>%
            dplyr::mutate(idArticle.tone = 1:n())
          if (return_wide) {
            fields_df <-
              fields_df %>%
              gather(item, value, -idArticle.tone) %>%
              arrange(idArticle.tone) %>%
              unite(item, item, idArticle.tone, sep = '.')

            order_fields <-
              fields_df$item

            field_data <-
              fields_df %>%
              spread(item, value) %>%
              dplyr::select_(.dots = order_fields)

          } else {
            field_data <-
              fields_df

            field_data <-
              field_data %>%
              dplyr::select(idArticle.tone, amount.tone)
          }
        }

        return(field_data)
      }

    if (!'tone' %in% names(gdelt_data)) {
      stop("Sorry missing tone column")
    }
    counts_data <-
      gdelt_data %>%
      dplyr::select(idGKG, tone)

    all_counts <-
      seq_along(counts_data$tone) %>%
      map_dfr(function(x) {
        parse_article_tones(field = counts_data$tone[x], return_wide = F) %>%
          dplyr::mutate(idGKG = counts_data$idGKG[x])
      }) %>%
      dplyr::select(idGKG, everything()) %>%
      dplyr::rename(scoreTone = amount.tone)

    if (filter_na) {
      all_counts <-
        all_counts %>%
        dplyr::filter(!scoreTone %>% is.na)
    }

    all_counts <-
      all_counts %>%
      get_clean_count_data(count_col = 'idArticle.tone',
                           extra_key = NA,
                           return_wide = return_wide) %>%
      arrange(idGKG) %>%
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

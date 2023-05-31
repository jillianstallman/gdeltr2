#' Returns mentioned CAMEO event count from a gkg data frame
#'
#' @param gdelt_data
#' @param count_column options \code{c('count', 'counts'))}
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
parse_gkg_mentioned_event_counts <- function(gdelt_data,
                                             count_column = 'counts',
                                             filter_na = T,
                                             return_wide = T) {
  count_cols <-
    c('counts',
      'count',
      'countsCharLoc',
      'countCharLoc',
      'charLoc')

  if (!count_column %in% count_cols) {
    stop("Sorry count column can only be\n" %>%
           paste0(paste0(count_cols, collapse = '\n')))
  }

  if (count_column %in% c('counts', 'count')) {
    count_column <-
      'counts'
  }

  if (count_column %in% c('countsCharLoc', 'countCharLoc', 'charLoc')) {
    count_column <-
      'countsCharLoc'
  }

  parse_field_count <-
    function(field = "KIDNAP#60##4#Beirut, Beyrouth, Lebanon#LE#LE04#33.8719#35.5097#-801546;KIDNAP#2##1#Lebanon#LE#LE#33.8333#35.8333#LE;KIDNAP#4##1#Australia#AS#AS#-27#133#AS;",
             return_wide = F) {
      options(scipen = 99999, digits = 5)
      if (field %>% is.na) {
        if (return_wide) {
          field_data <-
            tibble(codeGKGTheme = NA)
        } else {
          field_data <-
            tibble(codeGKGTheme = NA,
                   idArticle.field = 1)
        }
      }  else {
        fields <-
          field %>%
          str_split('\\;') %>%
          flatten_chr() %>%
          .[!. %in% '']

        fields_df <-
          tibble(field_item = fields) %>%
          dplyr::mutate(idArticle.field = 1:n()) %>%
          separate(
            col = field_item,
            sep = '\\#',
            into = c(
              'codeGKGTheme',
              'countEvent',
              'entityEvent',
              'idTypeLocation',
              'location',
              'idCountry',
              'idADM1Code',
              'latitude',
              'longitude',
              'idFeature',
              'charLoc'
            )
          ) %>%
          suppressMessages() %>%
          suppressWarnings()

        fields_df <-
          fields_df %>%
          mutate_at(
            fields_df %>% dplyr::select(dplyr::matches("count|charLoc|idTypeLocation")) %>% dplyr::select(-idCountry) %>% names(),
            funs(. %>% as.numeric())
          ) %>%
          mutate_at(fields_df %>% dplyr::select(dplyr::matches("latitude|longitude")) %>% names(),
                    (funs(as.numeric(., digits = 5))))

        fields_df$entityEvent[fields_df$entityEvent == ''] <-
          NA

        fields_df$location[fields_df$location == ''] <-
          NA

        fields_df$idCountry[fields_df$idCountry == ''] <-
          NA

        fields_df$idADM1Code[fields_df$idADM1Code == ''] <-
          NA

        fields_df$idFeature[fields_df$idFeature == ''] <-
          NA

        fields_df <-
          fields_df %>%
          dplyr::left_join(tibble(
            idTypeLocation = 1:5,
            typeLocation = c(
              'country',
              'usState',
              'usCity',
              'worldCity',
              'worldState'
            )
          )) %>%
          dplyr::select(codeGKGTheme:idTypeLocation,
                        typeLocation,
                        everything()) %>%
          suppressMessages()

        if (return_wide) {
          fields_df <-
            fields_df %>%
            gather(item, value, -idArticle.field) %>%
            arrange(idArticle.field) %>%
            unite(item, item, idArticle.field, sep = '.')

          order_fields <-
            fields_df$item

          field_data <-
            fields_df %>%
            spread(item, value) %>%
            dplyr::select_(.dots = order_fields)

          field_data <-
            field_data %>%
            mutate_at(
              fields_df %>% dplyr::select(dplyr::matches("count|charLoc|idTypeLocation")) %>% dplyr::select(-one_of("idCountry")) %>% names(),
              funs(. %>% as.numeric())
            ) %>%
            mutate_at(fields_df %>% dplyr::select(dplyr::matches("latitude|longitude")) %>% names(),
                      (funs(as.numeric(., digits = 5))))

        } else {
          field_data <-
            fields_df

          field_data <-
            field_data %>%
            dplyr::select(idArticle.field, everything())
        }
      }

      return(field_data)
    }

  if (!count_column %in% names(gdelt_data)) {
    stop("Sorry missing count column")
  }

  col_names <-
    c('idGKG', count_column)

  counts_data <-
    gdelt_data %>%
    dplyr::select_(.dots = col_names)

  names(counts_data)[2] <-
    'count_col'

  if (filter_na) {
    counts_data <-
      counts_data %>%
      dplyr::filter(!count_col %>% is.na())
  }

  all_counts <-
    seq_along(counts_data$count_col) %>%
    map_dfr(function(x) {
      parse_field_count(field = counts_data$count_col[x],
                        return_wide = F) %>%
        dplyr::mutate(idGKG = counts_data$idGKG[x])
    }) %>%
    dplyr::select(idGKG, everything()) %>%
    select(which(colMeans(is.na(.)) < 1))

  all_counts <-
    all_counts %>%
    get_clean_count_data(count_col = 'idArticle.field', return_wide = return_wide) %>%
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
#' Returns mentioned locations from a gkg data frame
#'
#' @param gdelt_data
#' @param location_column options \code{c('location', 'locations', 'locationsCharLoc', 'locationCharLoc', 'charLoc'))}
#' @param isCharLoc
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
parse_gkg_mentioned_locations <-
  function(gdelt_data,
           location_column = 'locations',
           isCharLoc = F,
           filter_na = T,
           return_wide = T) {
    location_cols <-
      c('location',
        'locations',
        'locationsCharLoc',
        'locationCharLoc',
        'charLoc')

    if (!location_column %in% location_cols) {
      stop("Sorry location column can only be\n" %>%
             paste0(paste0(location_cols, collapse = '\n')))
    }

    if (location_column %in% c('location', 'locations')) {
      location_column <-
        'locations'
    }

    if (location_column %in% c('locationsCharLoc', 'locationCharLoc', 'charLoc')) {
      location_column <-
        'locationsCharLoc'
    }
    parse_location_count <-
      function(field = "4#Leichhardt, New South Wales, Australia#AS#AS02#4944#-33.8833#151.15#-1583352#203;4#Daintree, Queensland, Australia#AS#AS04#40202#-1625#145.317#-1568710#421;4#Daintree, Queensland, Australia#AS#AS04#40202#-1625#145.317#-1568710#2224",
               return_wide = F) {
        options(scipen = 99999, digits = 5)
        if (field %>% is.na) {
          if (return_wide) {
            field_data <-
              tibble(location = NA)
          } else {
            field_data <-
              tibble(location = NA, idArticle.location = 1)
          }
        }  else {
          fields <-
            field %>%
            str_split('\\;') %>%
            flatten_chr() %>%
            .[!. %in% '']

          fields_df <-
            tibble(field_item = fields) %>%
            dplyr::mutate(idArticle.location = 1:n())


          if (isCharLoc) {
            fields_df <-
              fields_df %>%
              separate(
                col = field_item,
                sep = '\\#',
                into = c(
                  'idTypeLocation',
                  'location',
                  'idCountry',
                  'idADM1Code',
                  'idADM2Code',
                  'latitude',
                  'longitude',
                  'idFeature',
                  'charLoc'
                )
              ) %>%
              suppressMessages() %>%
              suppressWarnings()

          } else {
            fields_df <-
              fields_df %>%
              separate(
                col = field_item,
                sep = '\\#',
                into = c(
                  'idTypeLocation',
                  'location',
                  'idCountry',
                  'idADM1Code',
                  'latitude',
                  'longitude',
                  'idFeature'
                )
              ) %>%
              suppressMessages() %>%
              suppressWarnings()
          }

          fields_df <-
            fields_df %>%
            mutate_at(
              fields_df %>% dplyr::select(dplyr::matches("idTypeLocation|charLoc")) %>% names(),
              funs(. %>% as.numeric())
            ) %>%
            mutate_at(fields_df %>% dplyr::select(dplyr::matches("latitude|longitude")) %>% names(),
                      (funs(as.numeric(., digits = 5)))) %>%
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
            suppressMessages() %>%
            dplyr::select(idTypeLocation, typeLocation, everything()) %>%
            suppressWarnings()


          fields_df$location[fields_df$location == ''] <-
            NA

          fields_df$idCountry[fields_df$idCountry == ''] <-
            NA

          fields_df$idADM1Code[fields_df$idADM1Code == ''] <-
            NA

          fields_df$idFeature[fields_df$idFeature == ''] <-
            NA

          if (return_wide) {
            fields_df <-
              fields_df %>%
              gather(item, value, -idArticle.location) %>%
              arrange(idArticle.location) %>%
              unite(item, item, idArticle.location, sep = '.')

            order_fields <-
              fields_df$item

            field_data <-
              fields_df %>%
              spread(item, value) %>%
              dplyr::select_(.dots = order_fields)

            field_data <-
              field_data %>%
              mutate_at(
                fields_df %>% dplyr::select(dplyr::matches("idTypeLocation|charLoc")) %>% names(),
                funs(. %>% as.numeric())
              ) %>%
              mutate_at(fields_df %>% dplyr::select(dplyr::matches("latitude|longitude")) %>% names(),
                        (funs(as.numeric(., digits = 5))))
          } else {
            field_data <-
              fields_df

            field_data <-
              field_data %>%
              dplyr::select(idArticle.location, everything())
          }
        }

        return(field_data)
      }

    if (!location_column %in% names(gdelt_data)) {
      stop("Sorry missing location column")
    }

    col_names <-
      c('idGKG', location_column)

    counts_data <-
      gdelt_data %>%
      dplyr::select_(.dots = col_names)

    names(counts_data)[2] <-
      'loc_col'

    if (filter_na) {
      counts_data <-
        counts_data %>%
        dplyr::filter(!loc_col %>% is.na())
    }

    all_counts <-
      seq_along(counts_data$loc_col) %>%
      map_dfr(function(x) {
        parse_location_count(field = counts_data$loc_col[x],
                             return_wide = F) %>%
          dplyr::mutate(idGKG = counts_data$idGKG[x])
      }) %>%
      dplyr::select(idGKG, everything())

    if (filter_na) {
      all_counts <-
        all_counts %>%
        dplyr::filter(!location %>% is.na())
    }
    all_counts <-
      all_counts %>%
      get_clean_count_data(count_col = 'idArticle.location', return_wide = return_wide) %>%
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
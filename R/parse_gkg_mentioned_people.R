#' Returns long or wide mentioned people from a GKG data frame
#'
#' @param gdelt_data
#' @param people_column options \code{c('person', 'persons', 'persons.count', 'personsCharLoc', 'charLoc'))}
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
parse_gkg_mentioned_people <- function(gdelt_data,
                                       people_column = 'personsCharLoc',
                                       filter_na = T,
                                       return_wide = T) {
  people_count_cols <-
    c('person',
      'persons',
      'persons.count',
      'personsCharLoc',
      'charLoc')

  if (!people_column %in% people_count_cols) {
    stop("Sorry people column can only be\n" %>%
           paste0(paste0(people_count_cols, collapse = '\n')))
  }

  if (people_column %in% c('person', 'persons')) {
    people_column <-
      'persons'
  }

  if (people_column %in% c('persons.count', 'personsCharLoc', 'charLoc')) {
    people_column <-
      'personsCharLoc'
  }

  parse_mentioned_people_counts <-
    function(field = "Chaudhry Nisar Ali Khan,63",
             return_wide = F) {
      options(scipen = 99999)
      if (field %>% is.na) {
        if (return_wide) {
          field_data <-
            tibble(namePerson1 = NA, charLoc1 = NA)
        } else {
          field_data <-
            tibble(
              namePerson = NA,
              charLoc = NA,
              idArticlePerson = 1
            )
        }
      } else {
        fields <-
          field %>%
          str_split('\\;') %>%
          flatten_chr() %>%
          .[!. %in% '']

        fields_df <-
          tibble(field = fields) %>%
          dplyr::mutate(idArticlePerson = 1:n()) %>%
          separate(field,
                   into = c('namePerson', 'charLoc'),
                   sep = '\\,') %>%
          suppressWarnings() %>%
          suppressMessages()

        if (return_wide) {
          fields_df <-
            fields_df %>%
            gather(item, value, -idArticlePerson) %>%
            arrange(idArticlePerson) %>%
            unite(item, item, idArticlePerson, sep = '.') %>%
            suppressWarnings()

          order_fields <-
            fields_df$item

          field_data <-
            fields_df %>%
            spread(item, value) %>%
            dplyr::select_(.dots = order_fields)

          field_data <-
            field_data %>%
            mutate_at(
              field_data %>% dplyr::select(dplyr::matches('charLoc')) %>% names(),
              funs(. %>% as.character() %>% parse_number())
            )

        } else {
          field_data <-
            fields_df

          field_data <-
            field_data %>%
            dplyr::select(idArticlePerson, namePerson, charLoc) %>%
            dplyr::mutate(charLoc = charLoc %>% as.numeric)
        }
      }

      return(field_data)
    }

  if (!people_column %in% names(gdelt_data)) {
    stop("Sorry missing people column")
  }

  col_names <-
    c('idGKG', people_column)

  counts_data <-
    gdelt_data %>%
    dplyr::select_(.dots = col_names)

  names(counts_data)[2] <-
    'people_col'

  if (filter_na) {
    counts_data <-
      counts_data %>%
      dplyr::filter(!people_col %>% is.na())
  }

  all_counts <-
    seq_along(counts_data$people_col) %>%
    map_dfr(function(x) {
      parse_mentioned_people_counts(field = counts_data$people_col[x],
                                    return_wide = F) %>%
        dplyr::mutate(idGKG = counts_data$idGKG[x])
    }) %>%
    dplyr::select(idGKG, everything())

  if (people_column == 'persons') {
    all_counts <-
      all_counts %>%
      dplyr::select(-charLoc)
  }

  all_counts <-
    all_counts %>%
    get_clean_count_data(count_col = 'idArticlePerson', return_wide = return_wide) %>%
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
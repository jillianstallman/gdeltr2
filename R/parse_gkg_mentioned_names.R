#' Returns mentioned names from a GKG data frame.
#'
#' @param gdelt_data
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
parse_gkg_mentioned_names <- function(gdelt_data,
                                      filter_na = T,
                                      return_wide = T) {
  parse_mentioned_names_counts <-
    function(field = "Interior Minister Chaudhry Nisar Ali Khan,47;Mullah Mansour,87;Afghan Taliban,180;Mullah Mansour,382;Mullah Mansor,753;Mullah Mansour,815;Mullah Mansour,1025",
             return_wide = return_wide) {
      options(scipen = 99999)
      if (field %>% is.na()) {
        if (return_wide) {
          field_data <-
            tibble(nameMentionedName1 = NA,
                   charLoc1 = NA)
        } else {
          field_data <-
            tibble(
              nameMentionedName = NA,
              charLoc = NA,
              idArticleMentionedName = 1
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
          dplyr::mutate(idArticleMentionedName = 1:n()) %>%
          separate(field,
                   into = c('nameMentionedName', 'charLoc'),
                   sep = '\\,') %>%
          mutate(charLoc = charLoc %>% as.numeric()) %>%
          suppressMessages() %>%
          suppressWarnings()

        if (return_wide) {
          fields_df <-
            fields_df %>%
            gather(item, value, -c(idArticleMentionedName, charLoc)) %>%
            arrange(idArticleMentionedName) %>%
            unite(item, item, idArticleMentionedName, sep = '')

          order_fields <-
            fields_df$item

          field_data <-
            fields_df %>%
            dplyr::select(-dplyr::matches("charLoc")) %>%
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
            dplyr::select(idArticleMentionedName,
                          charLoc,
                          nameMentionedName)

        }
      }

      return(field_data)
    }

  if (!'mentionedNamesCounts' %in% names(gdelt_data)) {
    stop("Sorry missing metioned name column")
  }
  counts_data <-
    gdelt_data %>%
    dplyr::select(idGKG, mentionedNamesCounts)

  all_counts <-
    seq_along(counts_data$mentionedNamesCounts) %>%
    map_dfr(function(x) {
      parse_mentioned_names_counts(field = counts_data$mentionedNamesCounts[x],
                                   return_wide = F) %>%
        dplyr::mutate(idGKG = counts_data$idGKG[x])
    }) %>%
    dplyr::select(idGKG, everything())

  if (filter_na) {
    if ('nameMentionedName' %in% names(all_counts)) {
      all_counts <-
        all_counts %>%
        dplyr::filter(!nameMentionedName %>% is.na())
    }
  }

  all_counts <-
    all_counts %>%
    get_clean_count_data(count_col = 'idArticleMentionedName',
                         return_wide = F) %>%
    separate(
      idGKG,
      into = c('GKG', 'dateTime'),
      sep = '\\-',
      remove = F
    ) %>%
    mutate(dateTime = dateTime %>% as.numeric()) %>%
    select(-dplyr::matches("charLoc")) %>%
    arrange(dateTime) %>%
    dplyr::select(-c(dateTime, GKG)) %>%
    suppressWarnings()

  if (return_wide) {
    all_counts <-
      all_counts %>%
      spread(item, value)
  }

  if (!return_wide) {
    all_counts <-
      all_counts %>%
      .resolve_long_names()
  }

  return(all_counts)
}
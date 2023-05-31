#' Returns long or wide mentioned numerics from a GKG data frame
#'
#' @param gdelt_data
#' @param filter_na
#' @param include_char_locg
#' @param return_wide
#' @importFrom tidyr gather
#' @importFrom tidyr unite
#' @importFrom tidyr spread
#' @importFrom tidyr separate
#' @importFrom purrr map
#' @importFrom purrr compact
#' @import stringr purrr
#' @return
#' @export
#'
#' @examples
parse_gkg_mentioned_numerics <- function(gdelt_data,
                                         filter_na = T,
                                         include_char_loc = T,
                                         return_wide = T) {
  parse_mentioned_numerics <-
    function(field = "170,Scotland Road,1600;170,Scotland Road,2475;",
             return_wide = F) {
      options(scipen = 99999)
      if (field %>% is.na) {
        if (return_wide) {
          field_data <-
            tibble(
              amountValue1 = NA,
              amountTerm1 = NA,
              charLoc = NA
            )
        } else {
          field_data <-
            tibble(
              amountValue = NA,
              amountTerm = NA,
              charLoc = NA,
              idArticleNumericItem = 1
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
          dplyr::mutate(idArticleNumericItem = 1:n()) %>%
          separate(
            field,
            into = c('amountValue', 'amountTerm', 'charLoc'),
            sep = '\\,'
          ) %>%
          dplyr::mutate(amountTerm = amountTerm %>% str_trim) %>%
          suppressMessages() %>%
          suppressWarnings()

        if (return_wide) {
          fields_df <-
            fields_df %>%
            gather(item, value, -idArticleNumericItem) %>%
            arrange(idArticleNumericItem) %>%
            unite(item, item, idArticleNumericItem, sep = '.')

          order_fields <-
            fields_df$item

          field_data <-
            fields_df %>%
            spread(item, value) %>%
            dplyr::select_(.dots = order_fields)

          field_data <-
            field_data %>%
            dplyr::mutate_at(
              field_data %>% dplyr::select(dplyr::matches('amountValue|charLoc')) %>% names(),
              funs(. %>% as.character() %>% parse_number())
            )

        } else {
          field_data <-
            fields_df

          field_data <-
            field_data %>%
            dplyr::select(idArticleNumericItem,
                          amountValue,
                          amountTerm,
                          charLoc) %>%
            dplyr::mutate(charLoc = charLoc %>% as.numeric,
                          amountValue = amountValue %>% as.numeric)
        }
      }

      return(field_data)
    }

  if (!'mentionedNumericsCounts' %in% names(gdelt_data)) {
    stop("Sorry missing numeric count column")
  }
  counts_data <-
    gdelt_data %>%
    dplyr::select(idGKG, mentionedNumericsCounts)

  if (filter_na) {
    counts_data <-
      counts_data %>%
      dplyr::filter(!mentionedNumericsCounts %>% is.na)
  }

  all_counts <-
    seq_along(counts_data$mentionedNumericsCounts) %>%
    map_dfr(function(x) {
      parse_mentioned_numerics(field = counts_data$mentionedNumericsCounts[x],
                               return_wide = F) %>%
        dplyr::mutate(idGKG = counts_data$idGKG[x])
    }) %>%
    dplyr::select(idGKG, everything())

  if (include_char_loc == F) {
    all_counts <-
      all_counts %>%
      dplyr::select(-charLoc)
  }

  all_counts <-
    all_counts %>%
    get_clean_count_data(count_col = 'idArticleNumericItem', return_wide = return_wide) %>%
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
#' Returns GCAM codes from a gkg data frame
#'
#' @param gdelt_data
#' @param merge_gcam_codes
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
parse_gkg_mentioned_gcams <- function(gdelt_data,
                                      merge_gcam_codes = F,
                                      filter_na = T,
                                      return_wide = T) {
  parse_gcam_data <-
    function(field = "wc:284,c121:5",
             return_wide = F) {
      options(scipen = 99999)
      if (field %>% is.na) {
        if (return_wide) {
          field_data <-
            tibble(idGCAM = NA)
        } else {
          field_data <-
            tibble(idGCAM = NA,
                   idArticleGCAM = 1)
        }
      }  else {
        fields <-
          field %>%
          str_split('\\,') %>%
          flatten_chr() %>%
          .[!. %in% '']

        articleWordCount <-
          fields[1] %>%
          as.character() %>%
          parse_number()

        fields_df <-
          tibble(articleWordCount,
                 idGCAM = fields[2:length(fields)]) %>%
          separate(idGCAM,
                   into = c('idGCAM', 'scoreGoldsteinWords'),
                   sep = '\\:') %>%
          dplyr::mutate(
            idArticleGCAM = 1:n(),
            scoreGoldsteinWords = scoreGoldsteinWords %>% as.numeric(., digits = 4)
          )


        if (return_wide) {
          fields_df <-
            fields_df %>%
            gather(item, value, -c(articleWordCount, idArticleGCAM)) %>%
            arrange(idArticleGCAM) %>%
            unite(item, item, idArticleGCAM, sep = '.')

          order_fields <-
            fields_df$item

          field_data <-
            fields_df %>%
            spread(item, value) %>%
            dplyr::select_(.dots = order_fields)

          field_data <-
            field_data %>%
            mutate_at(field_data %>% dplyr::select(dplyr::matches("scoreGoldsteinWords")) %>% names(),
                      funs(. %>% as.numeric()))

        } else {
          field_data <-
            fields_df

          field_data <-
            field_data %>%
            dplyr::select(idArticleGCAM, everything())
        }
      }

      return(field_data)
    }

  if (!'gcam' %in% names(gdelt_data)) {
    stop("Sorry missing video embed column")
  }

  counts_data <-
    gdelt_data %>%
    dplyr::select(idGKG, gcam)

  if (filter_na) {
    counts_data <-
      counts_data %>%
      dplyr::filter(!gcam %>% is.na())
  }

  all_counts <-
    seq_along(counts_data$gcam) %>%
    map_dfr(function(x) {
      parse_gcam_data(field = counts_data$gcam[x],
                      return_wide = F) %>%
        dplyr::mutate(idGKG = counts_data$idGKG[x])
    }) %>%
    dplyr::select(idGKG, everything())

  if (merge_gcam_codes) {
    all_counts <-
      all_counts %>%
      dplyr::left_join(
        get_codes_gcam() %>%
          dplyr::select(
            idGCAM,
            typeDictionary,
            dictionaryHumanName,
            dimensionHumanName
          )
      )
  }

  all_counts <-
    all_counts %>%
    get_clean_count_data(count_col = 'idArticleGCAM',
                         extra_key = 'articleWordCount',
                         return_wide = return_wide) %>%
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
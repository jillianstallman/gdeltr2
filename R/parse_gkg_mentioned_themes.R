#' Returns mentioned themes from a gkg data frame
#'
#' @param gdelt_data
#' @param filter_na
#' @param organization_column options \code{c('theme', 'themes', 'countThemes', 'themesCharLoc', 'charLoc'))}
#' @param return_wide
#' @importFrom purrr map
#' @import purrr
#' @return
#' @export
#'
#' @examples

parse_gkg_mentioned_themes <- function(gdelt_data,
                                       filter_na = T,
                                       theme_column = 'themes',
                                       return_wide = T) {
  theme_count_cols <-
    c('theme',
      'themes',
      'countThemes',
      'themesCharLoc',
      'charLoc')

  if (!theme_column %in% theme_count_cols) {
    stop("Sorry theme column can only be\n" %>%
           paste0(paste0(theme_count_cols, collapse = '\n')))
  }

  if (theme_column %in% c('theme', 'themes')) {
    theme_column <-
      'themes'
  }

  if (theme_column %in% c('countThemes', 'themesCharLoc', 'charLoc')) {
    theme_column <-
      'themesCharLoc'
  }

  parse_mentioned_names_themes <-
    function(field = "https://youtube.com/esctodaytv;https://youtube.com/embed/5ymFX91HwM0?wmode=transparent&#038;modestbranding=1&#038;autohide=1&#038;showinfo=0&#038;rel=0;",
             return_wide = F) {
      options(scipen = 99999)
      if (field %>% is.na) {
        if (return_wide) {
          field_data <-
            tibble(codeTheme1 = NA, charLoc1 = NA)
        } else {
          field_data <-
            tibble(
              codeTheme = NA,
              charLoc = NA,
              idArticleGKGTheme = 1
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
          dplyr::mutate(idArticleGKGTheme = 1:n()) %>%
          separate(field,
                   into = c('codeTheme', 'charLoc'),
                   sep = '\\,') %>%
          suppressWarnings()

        if (return_wide) {
          fields_df <-
            fields_df %>%
            gather(item, value, -idArticleGKGTheme) %>%
            arrange(idArticleGKGTheme) %>%
            unite(item, item, idArticleGKGTheme, sep = '.')

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
            dplyr::select(idArticleGKGTheme, codeTheme, charLoc) %>%
            dplyr::mutate(charLoc = charLoc %>% as.numeric)
        }
      }

      return(field_data)
    }

  if (!theme_column %in% names(gdelt_data)) {
    stop("Sorry missing organization column")
  }

  col_names <-
    c('idGKG', theme_column)

  counts_data <-
    gdelt_data %>%
    dplyr::select_(.dots = col_names)
  names(counts_data)[2] <-
    'theme_col'

  if (filter_na) {
    counts_data <-
      counts_data %>%
      dplyr::filter(!theme_col %>% is.na())
  }

  all_counts <-
    seq_along(counts_data$theme_col) %>%
    map_dfr(function(x) {
      parse_mentioned_names_themes(field = counts_data$theme_col[x],
                                   return_wide = F) %>%
        dplyr::mutate(idGKG = counts_data$idGKG[x])
    }) %>%
    dplyr::select(idGKG, everything())

  if (theme_column == 'themes') {
    all_counts <-
      all_counts %>%
      dplyr::select(-charLoc)
  }

  all_counts <-
    all_counts %>%
    get_clean_count_data(count_col = 'idArticleGKGTheme', return_wide = return_wide) %>%
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
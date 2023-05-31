

#' Returns source name or source url from a gkg data frame
#'
#' @param gdelt_data
#' @param source_column options \code{c('sources', 'source', 'sources.url', 'source.url'))}
#' @param filter_na
#' @param return_wide
#' @importFrom urltools domain
#'
#' @return
#' @export
#'
#' @examples
parse_gkg_mentioned_source_data <-
  function(gdelt_data,
           source_column = 'sources',
           filter_na = T,
           return_wide = F) {
    source_options <-
      c('sources', 'source', 'urlSources', 'urlSource')
    if (!source_column %in% source_options) {
      stop("Sorry source column can only be\n" %>%
             paste0(paste0(source_options, collapse = '\n')))
    }

    if (source_column %in% c('sources', 'source')) {
      source_column <-
        'sources'
    }

    if (source_column %in% c('urlSources', 'urlSource')) {
      source_column <-
        'urlSources'
    }
    parse_source_name <-
      function(field = "businesstimes.com.sg;businesstimes.com.sg",
               return_wide = F) {
        options(scipen = 99999)
        if (field %>% is.na) {
          if (return_wide) {
            field_data <-
              tibble(nameSource = NA)
          } else {
            field_data <-
              tibble(nameSource = NA,
                     idArticle.source1 = 1)
          }
        }  else {
          fields <-
            field %>%
            str_split('\\;') %>%
            flatten_chr() %>%
            .[!. %in% ''] %>%
            unique

          fields_df <-
            tibble(nameSource = fields) %>%
            dplyr::mutate(idArticle.source = 1:n())
          if (return_wide) {
            fields_df <-
              fields_df %>%
              gather(item, value, -idArticle.source) %>%
              arrange(idArticle.source) %>%
              unite(item, item, idArticle.source, sep = '.')

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
              dplyr::select(idArticle.source,
                            everything())
          }
        }

        return(field_data)
      }

    if (!source_column %in% names(gdelt_data)) {
      stop("Sorry missing source column")
    }

    col_names <-
      c('idGKG', source_column)

    counts_data <-
      gdelt_data %>%
      dplyr::select_(.dots = col_names)

    names(counts_data)[2] <-
      'source_col'

    if (filter_na) {
      counts_data <-
        counts_data %>%
        dplyr::filter(!source_col %>% is.na())
    }

    all_counts <-
      seq_along(counts_data$source_col) %>%
      map_dfr(function(x) {
        parse_source_name(field = counts_data$source_col[x],
                          return_wide = return_wide) %>%
          dplyr::mutate(idGKG = counts_data$idGKG[x])
      })

    if (source_column == 'urlSources') {
      names(all_counts)[2] <-
        c('urlSource')
    }

    if (source_column == 'sources') {
      names(all_counts)[2] <-
        c('nameSource')
    }

    all_counts <-
      all_counts %>%
      dplyr::select(idGKG, everything())

    if (!return_wide) {
      all_counts <-
        all_counts %>%
        .resolve_long_names()
    }

    return(all_counts)
  }
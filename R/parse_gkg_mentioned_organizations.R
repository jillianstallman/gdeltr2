#' Returns long or wide mentioned organizations from a GKG data frame
#'
#' @param gdelt_data
#' @param organization_column options \code{c('organization', 'organizations', 'organizations.count', 'organizationsCharLoc', 'charLoc'))}
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
parse_gkg_mentioned_organizations <- function(gdelt_data,
                                              organization_column = 'organizations',
                                              filter_na = T,
                                              return_wide = T) {
  organization_count_cols <-
    c(
      'organization',
      'organizations',
      'organizations.count',
      'organizationsCharLoc',
      'charLoc'
    )

  if (!organization_column %in% organization_count_cols) {
    stop("Sorry people column can only be\n" %>%
           paste0(paste0(organization_count_cols, collapse = '\n')))
  }

  if (organization_column %in% c('organization', 'organizations')) {
    organization_column <-
      'organizations'
  }

  if (organization_column %in% c('organizations.count',
                                 'organizationsCharLoc',
                                 'charLoc')) {
    organization_column <-
      'personsCharLoc'
  }

  parse_mentioned_organization_counts <-
    function(field = "Twitter,2912;Pegasystems,169;Pegasystems,1238;Pegasystems,1829;Pegasystems,2079;Pegasystems,2888;Nasdaq,193;Nasdaq,2086;Ilena Ryan Pegasystems Inc,2892;Pegasystems Inc,173;Pegasystems Inc,2892",
             return_wide = F) {
      options(scipen = 99999)
      if (field %>% is.na) {
        if (return_wide) {
          field_data <-
            tibble(nameOrganization1 = NA,
                   charLoc1 = NA)
        } else {
          field_data <-
            tibble(
              nameOrganization = NA,
              charLoc = NA,
              idArticle.organization = 1
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
          dplyr::mutate(idArticle.organization = 1:n()) %>%
          separate(field,
                   into = c('nameOrganization', 'charLoc'),
                   sep = '\\,') %>%
          suppressMessages() %>%
          suppressWarnings()

        if (return_wide) {
          fields_df <-
            fields_df %>%
            gather(item, value, -idArticle.organization) %>%
            arrange(idArticle.organization) %>%
            unite(item, item, idArticle.organization, sep = '.')

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
            dplyr::select(idArticle.organization, nameOrganization, charLoc) %>%
            dplyr::mutate(charLoc = charLoc %>% as.numeric)
        }
      }

      return(field_data)
    }

  if (!organization_column %in% names(gdelt_data)) {
    stop("Sorry missing organization column")
  }

  col_names <-
    c('idGKG', organization_column)

  counts_data <-
    gdelt_data %>%
    dplyr::select_(.dots = col_names)

  names(counts_data)[2] <-
    'org_col'

  if (filter_na) {
    counts_data <-
      counts_data %>%
      dplyr::filter(!org_col %>% is.na())
  }

  all_counts <-
    seq_along(counts_data$org_col) %>%
    map_dfr(function(x) {
      parse_mentioned_organization_counts(field = counts_data$org_col[x],
                                          return_wide = F) %>%
        dplyr::mutate(idGKG = counts_data$idGKG[x])
    }) %>%
    dplyr::select(idGKG, everything())

  if (organization_column == 'organizations') {
    all_counts <-
      all_counts %>%
      dplyr::select(-charLoc)
  }

  all_counts <-
    all_counts %>%
    get_clean_count_data(count_col = 'idArticle.organization', return_wide = return_wide) %>%
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
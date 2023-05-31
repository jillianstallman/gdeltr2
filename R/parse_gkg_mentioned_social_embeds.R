#' Returns social embed information from a gkg data frame
#'
#' @param gdelt_data
#' @param social_embed_column options \code{c('urlSocialMediaImageEmbeds', 'images', 'urlSocialMediaVideoEmbeds', 'video', 'videos'))}
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples

parse_gkg_mentioned_social_embeds <-
  function(gdelt_data,
           social_embed_column = 'urlSocialMediaImageEmbeds',
           filter_na = T,
           return_wide = T) {
    image_video_cols <-
      c(
        'urlSocialMediaImageEmbeds',
        'images',
        'image',
        'urlSocialMediaVideoEmbeds',
        'video',
        'videos'
      )
    if (!social_embed_column %in% image_video_cols) {
      stop("Social embed column can only be\n" %>% paste0(paste0(image_video_cols, collapse = '\n')))
    }

    if (social_embed_column %in% c("image", "images")) {
      social_embed_column <-
        'urlSocialMediaImageEmbeds'
    }

    if (social_embed_column  %in% c("video", "videos")) {
      social_embed_column <-
        'urlSocialMediaVideoEmbeds'
    }
    parse_embeds <-
      function(field = "http://instagram.com/p/9YfHJtMx0N;http://instagram.com/p/BFz1t7Tsx8t;http://instagram.com/p/BEZrBSKsx8U;http://instagram.com/p/BEw5_T-Mx3B;",
               return_wide = F) {
        options(scipen = 99999)
        if (field %>% is.na) {
          if (return_wide) {
            field_data <-
              tibble(urlSocialMediaImageEmbed = NA)
          } else {
            field_data <-
              tibble(
                urlSocialMediaImageEmbed = NA,
                idArticleSocialMediaImageEmbed = 1
              )
          }
        }  else {
          fields <-
            field %>%
            str_split('\\;') %>%
            flatten_chr() %>%
            .[!. %in% '']

          fields_df <-
            tibble(urlSocialMediaImageEmbed = fields) %>%
            dplyr::mutate(
              idArticleSocialMediaImageEmbed = 1:n(),
              domainSocialMediaImageEmbed = urlSocialMediaImageEmbed %>% urltools::domain()
            )
          if (return_wide) {
            fields_df <-
              fields_df %>%
              gather(item, value, -idArticleSocialMediaImageEmbed) %>%
              arrange(idArticleSocialMediaImageEmbed) %>%
              unite(item, item, idArticleSocialMediaImageEmbed, sep = '.')

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
              dplyr::select(
                idArticleSocialMediaImageEmbed,
                domainSocialMediaImageEmbed,
                urlSocialMediaImageEmbed
              )
          }
        }

        return(field_data)
      }

    if (!social_embed_column %in% names(gdelt_data)) {
      stop("Sorry missing source embed column")
    }

    col_names <-
      c('idGKG', social_embed_column)

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
        parse_embeds(field = counts_data$source_col[x],
                     return_wide = F) %>%
          dplyr::mutate(idGKG = counts_data$idGKG[x])
      })

    names(all_counts)[1] <-
      'idColumn'

    if (social_embed_column == 'urlSocialMediaImageEmbeds') {
      names(all_counts)[3] <-
        c('urlSocialMediaImageEmbed')

      names(all_counts)[1] <-
        c('idArticleSocialMediaImageEmbed')
    }

    if (social_embed_column == 'urlSocialMediaVideoEmbeds') {
      names(all_counts)[3] <-
        c('urlSocialMediaVideo.embed')
      names(all_counts)[1] <-
        c('idArticle.social_media.video_embed')
    }

    all_counts <-
      all_counts %>%
      dplyr::select(dplyr::matches("id^[A-Z]"), everything())

    id_col <-
      all_counts %>% select(dplyr::matches("^id[A-Z]")) %>% names() %>% .[[1]] %>% suppressWarnings()
    all_counts <-
      all_counts %>%
      get_clean_count_data(count_col = id_col, return_wide = return_wide) %>%
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
        gather(item, value, -idGKG, na.rm = TRUE) %>%
        .resolve_long_names()
    }

    return(all_counts)
  }
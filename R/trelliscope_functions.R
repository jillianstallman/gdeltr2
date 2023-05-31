# trelliscope -------------------------------------------------------------
check_for_trelliscope_js <-
  function() {
    missing <-
      installed.packages() %>% dplyr::as_tibble() %>%
      dplyr::filter(Package == 'trelliscopejs') %>%
      nrow() == 0
    if (missing) {
      devtools::install_github("hafen/trelliscopejs")
    }
  }

.filter_domain_data <-
  function(data,
           domains = c(
             'wsj.com',
             'law360',
             'nytimes.com',
             'dailymail',
             'recode',
             'architizer',
             'globest',
             'realert',
             'seekingalpha.com',
             'bloomberg.com',
             'curbed.com',
             'commercialobserver.com',
             'globest.com',
             'realdeal.com',
             'techcrunch.com',
             'nytimes.com',
             'architizer',
             'pehub',
             'rew-online',
             'nypost'
           ),
           random_domains = 20,
           only_pictures = TRUE) {
    search_domains <-
      c()
    if (!random_domains %>% is_null()) {
      randoms <-
        data$domainSource %>% unique() %>% sample(random_domains)
      search_domains <-
        c(search_domains, randoms) %>% unique()
    }

    if (!domains %>% is_null()) {
      search_domains <-
        c(search_domains, domains)
    }

    search_domains <-
      search_domains %>%
      unique() %>%
      paste(collapse = '|')

    data <-
      data %>%
      filter(domainSource %>% str_detect(search_domains))

    if (only_pictures) {
      data <-
        data %>%
        filter(!urlImage %>% is.na())
    }

    return(data)
  }

#' Visualize Cloudvision trelliscope
#'
#' @param data
#' @param extra_columns
#' @param trelliscope_parameters \itemize{
#' \item title: Trelliscope title
#' \item rows: Trelliscope rows
#' \item columns: Trelliscope columns
#' \item path: file path to save trelliscope, if NULL no where
#' }
#' @param vgkg_parse \itemize{
#' \item faces,
#' \item labels,
#' \item landmarks,
#' \item languages,
#' \item logos,
#' \item ocr
#' }
#' @param domains
#' @param random_domains
#' @param only_pictures
#'
#' @return
#' @export
#' @import dplyr purrr tidyr trelliscopejs stringr
#' @examples
visualize_vgkg_trelliscope <-
  function(data,
           vgkg_parse = 'landmarks',
           domains = c(
             'wsj.com',
             'seekingalpha.com',
             'bloomberg.com',
             'curbed.com',
             'commercialobserver.com',
             'globest.com',
             'realdeal.com',
             'techcrunch.com',
             'nytimes.com',
             'architizer',
             'pehub',
             'rew-online.com',
             'nypost.com'
           ),
           trelliscope_parameters = list(
             title = NULL,
             path = NULL,
             rows = NULL,
             columns = NULL
           ),
           random_domains = 0,
           only_pictures = TRUE,
           extra_columns = NULL) {
    check_for_trelliscope_js()

    if (!extra_columns %>% is_null()) {
      df_extra <-
        data %>%
        dplyr::select(idVGKG, one_of(extra_columns))
    }
    has_domains <-
      !domains %>% is_null() |
      !random_domains %>% is_null()
    if (has_domains) {
      data <-
        data %>%
        .filter_domain_data(domains = domains, random_domains = random_domains)
    }
    if (!vgkg_parse %>% is_null()) {
      vgkg_options <-
        c('faces',
          'labels',
          'landmarks',
          'languages',
          'logos',
          'ocr')
      vgkg_parse <-
        vgkg_parse %>% str_to_lower()
      if (!vgkg_parse %in% vgkg_options) {
        stop(list(
          "Parse options can only be:\n",
          paste(vgkg_options, collapse = '\n')
        ) %>% reduce(paste0))
      }

      is_faces <-
        vgkg_parse == 'faces'

      if (is_faces) {
        data <-
          data %>%
          parse_vgkg_faces(filter_na = TRUE, return_wide = TRUE) %>%
          left_join(data %>% select(idVGKG:urlImage, dateDocument)) %>%
          suppressMessages() %>%
          suppressWarnings()

        main_name <-
          c('scoreDetectionConfidence',
            'scoreEmotionAngerLikelihood')
      }

      is_labels <-
        vgkg_parse == 'labels'

      if (is_labels) {
        data <-
          data %>%
          parse_vgkg_labels(filter_na = TRUE, return_wide = FALSE) %>%
          left_join(data %>% select(idVGKG:urlImage, dateDocument)) %>%
          suppressMessages() %>%
          suppressWarnings()

        main_name <-
          c('nameLabel', 'scoreConfidenceLabel')
      }

      is_landmark <-
        vgkg_parse == 'landmarks'

      if (is_landmark) {
        data <-
          data %>%
          parse_vgkg_landmarks(filter_na = TRUE, return_wide = FALSE) %>%
          left_join(data %>% select(idVGKG:urlImage, dateDocument)) %>%
          suppressMessages() %>%
          suppressWarnings()

        main_name <-
          c('nameLandmark', 'scoreConfidenceLandmark')
      }

      is_language <-
        vgkg_parse == 'languages'

      if (is_language) {
        data <-
          data %>%
          parse_vgkg_languages(filter_na = TRUE, return_wide = FALSE) %>%
          left_join(data %>% select(idVGKG:urlImage, dateDocument)) %>%
          suppressMessages() %>%
          suppressWarnings()
        main_name <-
          c('idLanguage')
      }

      is_ocr <-
        vgkg_parse == 'ocr'

      if (is_ocr) {
        data <-
          data %>%
          parse_vgkg_ocr(filter_na = TRUE, return_wide = FALSE) %>%
          left_join(data %>% select(idVGKG:urlImage, dateDocument)) %>%
          suppressMessages() %>%
          suppressWarnings()
        main_name <-
          c('textOCR')
      }

      is_logos <-
        vgkg_parse == 'logos'

      if (is_logos) {
        data <-
          data %>%
          parse_vgkg_logos(filter_na = TRUE, return_wide = FALSE) %>%
          left_join(data %>% select(idVGKG:urlImage, dateDocument)) %>%
          suppressMessages() %>%
          suppressWarnings()
        main_name <-
          c('nameLogo')
      }

    } else {
      main_name <-
        ''
    }

    if (!vgkg_parse %>% is_null()) {
      parse_title <-
        vgkg_parse %>% str_to_title()
    } else {
      parse_title <- ''
    }

    dates <-
      data$dateTimeDocument %>% as.Date("%Y-%m-%d") %>% unique() %>%
      suppressWarnings()

    dates <-
      dates[!dates %>% is.na()] %>% .[[1]]


    title <-
      list('GDELT VGKG Explorer for ',  parse_title, ' ', dates) %>%
      reduce(paste0)


    if (!extra_columns %>% is_null()) {
      data <-
        data %>%
        left_join(df_extra) %>%
        suppressMessages()
    }

    data <-
      data %>%
      mutate_if(is.logical,
                funs(ifelse(. %>% is.na(), FALSE, .)))

    if (!trelliscope_parameters$title %>% is_null()) {
      title <-
        trelliscope_parameters$title
    }

    if (trelliscope_parameters$rows %>% is_null()) {
      rows <- 1
    } else {
      rows <-
        trelliscope_parameters$rows
    }

    if (trelliscope_parameters$columns %>% is_null()) {
      columns <- 2
    } else {
      columns <-
        trelliscope_parameters$columns
    }
    no_path <-
      trelliscope_parameters$path %>% is_null()

    if (no_path) {
      viz <-
        data %>%
        dplyr::select(which(colMeans(is.na(.)) < .5)) %>%
        mutate(
          panel = trelliscopejs::img_panel(urlImage),
          urlArticle = trelliscopejs::cog_href(documentSource),
          idItem = 1:n()
        ) %>%
        select(idItem, idVGKG, everything()) %>%
        arrange(idItem) %>%
        trelliscopejs::trelliscope(
          name = title,
          nrow = rows,
          ncol = columns,
          state = list(labels = c(
            "domainSource", "idVGKG", main_name[[1]], 'urlArticle'
          ))
        )
    } else {
      file_path <-
        trelliscope_parameters$path
      viz <-
        data %>%
        dplyr::select(which(colMeans(is.na(.)) < .5)) %>%
        mutate(
          panel = trelliscopejs::img_panel(urlImage),
          urlArticle = trelliscopejs::cog_href(documentSource),
          idItem = 1:n()
        ) %>%
        select(idItem, idVGKG, everything()) %>%
        arrange(idItem) %>%
        trelliscopejs::trelliscope(
          name = title,
          nrow = rows,
          ncol = columns,
          path = file_path,
          state = list(labels = c(
            "domainSource", "idVGKG", main_name, 'urlArticle'
          ))
        )
    }

    return(viz)
  }

#' Trelliscope gkg data
#'
#' @param data
#' @param domains
#' @param trelliscope_parameters \itemize{
#' \item title: Trelliscope title
#' \item rows: Trelliscope rows
#' \item columns: Trelliscope columns
#' \item path: file path to save trelliscope, if NULL no where
#' }
#' @param extra_columns
#' @param random_domains
#' @param only_pictures
#' @param gkg_parse \itemize{
#' \item 'tone',
#' \item 'dates',
#' \item 'events',
#' \item 'gcam',
#' \item 'locations',
#' \item 'names',
#' \item 'numerics',
#' \item 'organizations',
#' \item 'people',
#' \item 'quotes',
#' \item 'social',
#' \item 'themes',
#' \item 'xml
#' }
#'
#' @return
#' @export
#' @import dplyr purrr tidyr trelliscopejs stringr
#' @examples
visualize_gkg_trelliscope <-
  function(data,
           domains = c(
             'wsj.com',
             'law360',
             'nytimes.com',
             'dailymail',
             'recode',
             'architizer',
             'globest',
             'realert',
             'seekingalpha.com',
             'bloomberg.com',
             'curbed.com',
             'commercialobserver.com',
             'globest.com',
             'realdeal.com',
             'techcrunch.com',
             'nytimes.com',
             'architizer',
             'pehub',
             'rew-online',
             'nypost'
           ),
           random_domains = 0,
           only_pictures = TRUE,
           extra_columns = NULL,
           trelliscope_parameters = list(
             title = NULL,
             path = NULL,
             rows = NULL,
             columns = NULL
           ),
           gkg_parse = 'names') {
    check_for_trelliscope_js()
    has_domains <-
      !domains %>% is_null() |
      !random_domains %>% is_null()
    if (has_domains) {
      data <-
        data %>%
        .filter_domain_data(domains = domains, random_domains = random_domains)
    }

    if (!gkg_parse %>% is_null()) {
      gkg_options <-
        c(
          'tone',
          'dates',
          'events',
          'gcam',
          'locations',
          'names',
          'numerics',
          'organizations',
          'people',
          'quotes',
          'social',
          'themes',
          'xml'
        )
      gkg_parse <-
        gkg_parse %>% str_to_lower()
      if (!gkg_parse %in% gkg_options) {
        stop(list(
          "Parse options can only be:\n",
          paste(gkg_options, collapse = '\n')
        ) %>% reduce(paste0))
      }

      is_tone <-
        gkg_parse == 'tone'

      if (is_tone) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_article_tone(filter_na = TRUE, return_wide = FALSE) %>%
          left_join(data %>% select(idGKG:domainSource, urlImage)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('scoreTone')
      }

      is_xml <-
        gkg_parse == 'xml'

      if (is_xml) {
        plot_data <-
          data %>%
          filter(!xmlExtras %>% is.na()) %>%
          parse_xml_extras(return_wide = FALSE) %>%
          left_join(data %>% select(idGKG:domainSource, urlImage)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('item', 'value')
      }

      is_date <-
        gkg_parse == 'date'

      if (is_date) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_dates(filter_na = TRUE, return_wide = FALSE) %>%
          left_join(data %>% select(idGKG:domainSource, urlImage)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('dateResolution')
      }

      is_events <-
        gkg_parse == 'events'

      if (is_events) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_event_counts(return_wide = FALSE, filter_na = TRUE) %>%
          left_join(get_codes_gkg_themes()) %>%
          left_join(data %>% select(idGKG:domainSource, urlImage)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('codeGKGTheme',
            'countEvent',
            'entityEvent',
            'idADM1Code')
      }

      is_gcam <-
        gkg_parse == 'gcam'

      if (is_gcam) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_gcams(return_wide = FALSE, filter_na = TRUE) %>%
          left_join(get_codes_gcam()) %>%
          left_join(data %>% select(idGKG:domainSource, urlImage)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('idGCAM', 'dimensionHumanName')
      }

      is_locations <-
        gkg_parse == 'locations'

      if (is_locations) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_locations(return_wide = FALSE, filter_na = TRUE) %>%
          left_join(get_codes_stability_locations()) %>%
          left_join(data %>% select(idGKG:domainSource, urlImage)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('idADM1Code', 'location')
      }

      is_names <-
        gkg_parse == 'names'

      if (is_names) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_names(return_wide = FALSE, filter_na = TRUE) %>%
          left_join(data %>% select(idGKG:domainSource, urlImage)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('nameMentionedName')
      }

      is_numerics <-
        gkg_parse == 'numerics'

      if (is_numerics) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_numerics(return_wide = FALSE, filter_na = TRUE) %>%
          left_join(data %>% select(idGKG:domainSource, urlImage)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('amountTerm', 'amountValue')
      }

      is_organizations <-
        gkg_parse == 'organizations'

      if (is_organizations) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_organizations(return_wide = FALSE, filter_na = TRUE) %>%
          left_join(data %>% select(idGKG:domainSource, urlImage)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('nameOrganization')
      }

      is_people <-
        gkg_parse == 'people'

      if (is_people) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_people(return_wide = FALSE, filter_na = TRUE) %>%
          left_join(data %>% select(idGKG:domainSource, urlImage)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('namePerson')
      }

      is_quotes <-
        gkg_parse == 'quotes'

      if (is_quotes) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_quotes(return_wide = FALSE, filter_na = TRUE) %>%
          left_join(data %>% select(idGKG:domainSource, urlImage)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('lengthQuote', 'textQuote')
      }

      is_social <-
        gkg_parse == 'social'

      if (is_social) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_social_embeds() %>%
          left_join(data %>% select(idGKG:domainSource, urlImage)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('domainSocialMediaImageEmbed')
      }

      is_themes <-
        gkg_parse == 'themes'

      if (is_themes) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_themes(return_wide = F) %>%
          left_join(get_codes_gkg_themes() %>% dplyr::rename(codeTheme = codeGKGTheme)) %>%
          left_join(data %>% select(idGKG:domainSource, urlImage)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('codeTheme')
      }

    } else {
      main_name <-
        ''
    }

    if (!gkg_parse %>% is_null()) {
      parse_title <-
        gkg_parse %>% str_to_title()
    } else {
      parse_title <- ''
    }
    dates <-
      data$dateTimeDocument %>% as.Date("%Y-%m-%d") %>% unique()

    dates <-
      dates[!dates %>% is.na()] %>% .[[1]]

    title <-
      list('GDELT GKG Explorer for ',  parse_title, ' ', dates) %>%
      reduce(paste0) %>% str_trim()

    if (!extra_columns %>% is_null()) {
      plot_data <-
        plot_data %>%
        left_join(
          data %>% dplyr::select(one_of(c('idGKG', extra_columns)))
        ) %>%
        suppressMessages()
    }

    plot_data <-
      plot_data %>%
      mutate_if(is.logical,
                funs(ifelse(. %>% is.na(), FALSE, .)))

    if (!trelliscope_parameters$title %>% is_null()) {
      title <-
        trelliscope_parameters$title
    }

    if (trelliscope_parameters$rows %>% is_null()) {
      rows <- 1
    } else {
      rows <-
        trelliscope_parameters$rows
    }

    if (trelliscope_parameters$columns %>% is_null()) {
      columns <- 2
    } else {
      columns <-
        trelliscope_parameters$columns
    }
    no_path <-
      trelliscope_parameters$path %>% is_null()

    if (no_path) {
      viz <-
        plot_data %>%
        mutate(
          panel = trelliscopejs::img_panel(urlImage),
          urlArticle = trelliscopejs::cog_href(documentSource),
          idItem = 1:n()
        ) %>%
        select(idItem, idGKG, everything()) %>%
        arrange(idItem) %>%
        trelliscopejs::trelliscope(
          name = title,
          nrow = rows,
          ncol = columns,
          state = list(labels = c(
            "domainSource", "idGKG", main_name, 'urlArticle'
          ))
        )
    } else {
      file_path <-
        trelliscope_parameters$path
      viz <-
        plot_data %>%
        mutate(
          panel = trelliscopejs::img_panel(urlImage),
          urlArticle = trelliscopejs::cog_href(documentSource),
          idItem = 1:n()
        ) %>%
        select(idItem, idGKG, everything()) %>%
        arrange(idItem) %>%
        trelliscopejs::trelliscope(
          name = title,
          nrow = rows,
          ncol = columns,
          path = file_path,
          state = list(labels = c(
            "domainSource", "idGKG", main_name, 'urlArticle'
          ))
        )
    }
    return(viz)
  }


#' Trelliscope gkg data
#'
#' @param data
#' @param domains
#' @param random_domains
#' @param only_pictures
#' @param gkg_parse \itemize{
#' \item 'tone',
#' \item 'dates',
#' \item 'events',
#' \item 'gcam',
#' \item 'locations',
#' \item 'names',
#' \item 'numerics',
#' \item 'organizations',
#' \item 'people',
#' \item 'quotes',
#' \item 'social',
#' \item 'themes',
#' \item 'xml
#' }
#'
#' @return
#' @export
#' @import dplyr purrr tidyr trelliscopejs stringr
#' @examples
visualize_gkg_tv_trelliscope <-
  function(data,
           stations = c(
             "CNN",
             "Bloomberg",
             "FBC",
             "BBCNews",
             "WTTG"
           ),
           random_stations = 5,
           only_pictures = TRUE,
           gkg_parse = 'people') {
    check_for_trelliscope_js()

    station_search <-
      c()
    has_random_stations <-
      !random_stations %>% is_null()
    if (has_random_stations) {
      random_tv <-
        data$nameSource %>%
        unique() %>%
        sample(random_stations)

      station_search <-
        c(station_search, random_tv)
    }

    if (!stations %>% is_null()) {
      station_search <-
        c(station_search, stations) %>%
        str_to_upper()
    }

    station_search <-
      station_search %>% str_to_upper() %>%
      unique() %>% paste0(collapse = '|')

    data <-
      data %>%
      filter(nameSource %>% str_detect(station_search))

    if (!gkg_parse %>% is_null()) {
      gkg_options <-
        c(
          'tone',
          'dates',
          'events',
          'gcam',
          'locations',
          'names',
          'numerics',
          'organizations',
          'people',
          'quotes',
          'social',
          'themes',
          'xml'
        )
      gkg_parse <-
        gkg_parse %>% str_to_lower()
      if (!gkg_parse %in% gkg_options) {
        stop(list(
          "Parse options can only be:\n",
          paste(gkg_options, collapse = '\n')
        ) %>% reduce(paste0))
      }

      is_tone <-
        gkg_parse == 'tone'

      if (is_tone) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_article_tone(filter_na = TRUE, return_wide = FALSE) %>%
          left_join(data %>% select(idGKG:nameTVShow)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('scoreTone')
      }

      is_xml <-
        gkg_parse == 'xml'

      if (is_xml) {
        plot_data <-
          data %>%
          filter(!xmlExtras %>% is.na()) %>%
          parse_xml_extras(return_wide = FALSE) %>%
          left_join(data %>% select(idGKG:nameTVShow)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('item', 'value')
      }

      is_date <-
        gkg_parse == 'date'

      if (is_date) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_dates(filter_na = TRUE, return_wide = FALSE) %>%
          left_join(data %>% select(idGKG:nameTVShow)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('dateResolution')
      }

      is_events <-
        gkg_parse == 'events'

      if (is_events) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_event_counts(return_wide = FALSE, filter_na = TRUE) %>%
          left_join(get_codes_gkg_themes()) %>%
          left_join(data %>% select(idGKG:nameTVShow)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('codeGKGTheme',
            'countEvent',
            'entityEvent',
            'idADM1Code')
      }

      is_gcam <-
        gkg_parse == 'gcam'

      if (is_gcam) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_gcams(return_wide = FALSE, filter_na = TRUE) %>%
          left_join(get_codes_gcam()) %>%
          left_join(data %>% select(idGKG:nameTVShow)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('idGCAM', 'dimensionHumanName')
      }

      is_locations <-
        gkg_parse == 'locations'

      if (is_locations) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_locations(return_wide = FALSE, filter_na = TRUE) %>%
          left_join(get_codes_stability_locations()) %>%
          left_join(data %>% select(idGKG:nameTVShow)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('idADM1Code', 'location')
      }

      is_names <-
        gkg_parse == 'names'

      if (is_names) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_names(return_wide = FALSE, filter_na = TRUE) %>%
          left_join(data %>% select(idGKG:nameTVShow)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('nameMentionedName')
      }

      is_numerics <-
        gkg_parse == 'numerics'

      if (is_numerics) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_numerics(return_wide = FALSE, filter_na = TRUE) %>%
          left_join(data %>% select(idGKG:nameTVShow)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('amountTerm', 'amountValue')
      }

      is_organizations <-
        gkg_parse == 'organizations'

      if (is_organizations) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_organizations(return_wide = FALSE, filter_na = TRUE) %>%
          left_join(data %>% select(idGKG:nameTVShow)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('nameOrganization')
      }

      is_people <-
        gkg_parse == 'people'

      if (is_people) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_people(people_column = 'personsCharLoc', return_wide = FALSE, filter_na = TRUE) %>%
          left_join(data %>% select(idGKG:nameTVShow)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('namePerson')
      }

      is_quotes <-
        gkg_parse == 'quotes'

      if (is_quotes) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_quotes(return_wide = FALSE, filter_na = TRUE) %>%
          left_join(data %>% select(idGKG:nameTVShow)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('lengthQuote', 'textQuote')
      }

      is_social <-
        gkg_parse == 'social'

      if (is_social) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_social_embeds() %>%
          left_join(data %>% select(idGKG:nameTVShow)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('domainSocialMediaImageEmbed')
      }

      is_themes <-
        gkg_parse == 'themes'

      if (is_themes) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_themes(return_wide = F) %>%
          left_join(get_codes_gkg_themes() %>% dplyr::rename(codeTheme = codeGKGTheme)) %>%
          left_join(data %>% select(idGKG:nameTVShow)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('codeTheme')
      }

    } else {
      main_name <-
        ''
    }

    if (!gkg_parse %>% is_null()) {
      parse_title <-
        gkg_parse %>% str_to_title()
    } else {
      parse_title <- ''
    }
    dates <-
      data$dateTimeDocument %>% as.Date("%Y-%m-%d") %>% unique()

    dates <-
      dates[!dates %>% is.na()] %>% .[[1]]

    title <-
      list('GDELT GKG TV Explorer for ',  parse_title, ' ', dates) %>%
      reduce(paste0) %>% str_trim()

    viz <-
      plot_data %>%
      mutate(urlImage = "http://www.novelupdates.com/img/noimagefound.jpg") %>%
      mutate(
        panel = trelliscopejs::img_panel(urlImage),
        urlArchiveVideo = trelliscopejs::cog_href(urlArchiveVideo),
        idItem = 1:n()
      ) %>%
      select(idItem, idGKG, everything()) %>%
      arrange(idItem) %>%
      trelliscopejs::trelliscope(
        name = title,
        nrow = 1,
        ncol = 2,
        state = list(
          labels = c(
            'dateTimeDocument',
            'urlArchiveVideo',
            "idTVNetwork",
            "nameTVShow",
            main_name,
            'idTVNetwork'
          )
        )
      )

    return(viz)
  }
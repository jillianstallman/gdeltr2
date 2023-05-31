
#' Visual Knowledge Graph label count
#'
#' Counts classified CloudVision labels
#'
#' @return
#' @export
#'
#' @examples
vgkg_label_count <-
  function() {
    data <-
      "http://data.gdeltproject.org/blog/2019-vgkg-entitieslists/20191018-vgkg-labelslist.csv.gz" %>%
      read_csv() %>%
      as_tibble()

    data <-
      data %>%
      mutate(entity = str_to_upper(entity)) %>%
      setNames(c("typeEntity", "midGoogle", "countClassifications"))

    data

  }


#' VGKG Classified Entity Count
#'
#' Counts classified entities from 2017
#' to 2019
#'
#' @return
#' @export
#'
#' @examples
vgkg_entity_count <-
  function(){
    data <-
      fread("http://data.gdeltproject.org/blog/2019-vgkg-entitieslists/20191018-vgkg-webentitieslist.csv.gz") %>%
      as_tibble()

    data <-
      data %>%
      mutate(entity = str_to_upper(entity)) %>%
      setNames(c("typeEntity", "midGoogle", "countClassifications"))

    data
  }

#' get_vgkg_schema
#'
#' @return
#' @importFrom dplyr tibble
#'
#' @examples
get_vgkg_schema  <- function() {
  cv_schema <-
    tibble(
      nameGDELT = c(
        "DATE",
        "DocumentIdentifier",
        "ImageURL",
        "Labels",
        "GeoLandmarks",
        "Logos",
        "SafeSearch",
        "Faces",
        "OCR",
        "LangHints",
        "WidthHeight",
        "RawJSON"
      ),
      nameActual =
        c(
          "dateTimeDocument",
          "documentSource",
          "urlImage",
          "xmlLabels",
          "xmlGeoLandmarks",
          "xmlLogos",
          "xmlSafeSearch",
          "xmlFaces",
          "xmlOCR",
          "codesLanguages",
          "dimWidthHeight",
          "jsonCloudVision"
        )

    )
  return(cv_schema)
}

#' get_urls_vgkg
#'
#' @return
#' @export
#' @importFrom lubridate ymd_hms
#' @importFrom dplyr tibble
#' @examples
get_urls_vgkg <- function() {
  options(scipen = 999999)
  cloud_vision_start_hms <-
    20160222113000

  time_now <-
    Sys.time() %>% format("%Y%m%d%H%M%S") %>% as.numeric()

  all_dates <-
    seq(ymd_hms(cloud_vision_start_hms),
        to = time_now %>% ymd_hms,
        by = '15 min')

  url_df <-
    tibble(
      dateTimeData = all_dates,
      isoPeriod = dateTimeData %>% format("%Y%m%d%H%M%S") %>% as.numeric(),
      urlCloudVisionTags = 'http://data.gdeltproject.org/gdeltv2_cloudvision/' %>% paste0(isoPeriod, '.imagetags.csv.gz'),
      urlTranslationTags = 'http://data.gdeltproject.org/gdeltv2_cloudvision/' %>% paste0(isoPeriod, '.translation.imagetags.csv.gz')
    ) %>%
    mutate(dateData = dateTimeData %>% as.Date()) %>%
    arrange(desc(dateTimeData))
  return(url_df)
}

#' Gets most recent CV log URLs
#'
#' @return
#' @import stringr
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom readr read_tsv
#' @importFrom tidyr spread
#'
#' @examples get_urls_gkg_most_recent_log()
get_urls_vgkg_most_recent  <- function() {
  log_df <-
    'http://data.gdeltproject.org/gdeltv2_cloudvision/lastupdate.txt' %>%
    read_tsv(col_names = F) %>%
    suppressMessages()

  names(log_df) <-
    c('value')

  values <-
    log_df$value %>% str_split("\\ ") %>%
    flatten_chr


  items <-
    c('idVGKPeriod', 'idVGKHash', 'urlCloudVisionTags')

  log_df <-
    tibble(item = items, value = values) %>%
    spread(item, value) %>%
    mutate(idVGKPeriod = idVGKPeriod %>% as.numeric())

  return(log_df)
}

.get_data_vgkg_url <-
  function(url = 'http://data.gdeltproject.org/gdeltv2_cloudvision/20160606234500.imagetagsv1.csv.gz',
           remove_json_column = TRUE,
           return_message = TRUE) {
    ok_url <-
      url %>% url_ok() %>% suppressWarnings()
    if (!ok_url) {
      stop("Invalid url")
    }

    cloud_vision_data <-
      url %>%
      curl() %>%
      gzcon() %>%
      read_tsv(col_names = F) %>%
      suppressWarnings() %>%
      suppressMessages()

    names(cloud_vision_data) <-
      get_vgkg_schema() %>% pull(nameActual)

    cloud_vision_data <-
      cloud_vision_data %>%
      dplyr::mutate(
        dateCodeURL = dateTimeDocument,
        idDateTime = 1:n(),
        idVGKG = dateTimeDocument %>% paste0('-', idDateTime),
        dateTimeDocument = dateTimeDocument %>% lubridate::ymd_hms() %>% lubridate::with_tz(Sys.timezone()),
        dateDocument = dateTimeDocument %>% as.Date(),
        dimWidthHeight = dimWidthHeight %>% as.character() %>% parse_number()
      ) %>%
      dplyr::select(idVGKG, idDateTime, everything())

    if (remove_json_column) {
      cloud_vision_data <-
        cloud_vision_data %>%
        dplyr::select(-jsonCloudVision)
    }


    if (return_message) {
      "Downloaded, parsed and imported " %>%
        paste0(url) %>%
        cat(fill = T)

    }
    return(cloud_vision_data)
  }


.get_data_vgkg_day <-
  function(date_data = "2016-06-08",
           only_most_recent = F,
           include_translations = F,
           remove_json_column = T,
           return_message = T) {
    if (only_most_recent) {
      urls <-
        get_urls_vgkg_most_recent() %>%
        .$urlCloudVisionTags
    } else {
      if (!date_data %>% substr(5, 5) == "-") {
        stop("Sorry data must be in YMD format, ie, 2016-06-01")
      }

      date_data <-
        date_data %>%
        lubridate::ymd() %>%
        as.Date()

      if (date_data < "2016-02-22") {
        stop("Sorry data starts on February 22, 2016")
      }

      if (date_data > Sys.Date()) {
        stop("Sorry data can't go into the future")
      }

      if (!'cv_urls' %>% exists()) {
        cv_urls <-
          get_urls_vgkg()

        assign(x = 'get_urls_vgkg', eval(get_urls_vgkg), env = .GlobalEnv)
      }
      urls <-
        cv_urls %>%
        dplyr::filter(dateData == date_data) %>%
        .$urlCloudVisionTags

      if (include_translations) {
        urls <-
          c(urls,
            cv_urls %>%
              dplyr::filter(dateData == date_data) %>%
              .$urlTranslationTags)
      }
    }

    .get_data_vgkg_url_safe <-
      possibly(.get_data_vgkg_url, tibble())

    all_data <-
      urls %>%
      map_dfr(function(x) {
        .get_data_vgkg_url_safe(
          url = x,
          remove_json_column = remove_json_column,
          return_message = return_message
        )
      }) %>%
      distinct() %>%
      dplyr::select(idVGKG, idDateTime, dateTimeDocument, everything()) %>%
      suppressWarnings()

    if (return_message) {
      "You retrieved " %>%
        paste0(all_data %>% nrow,
               " cloud vision processed items for ",
               date_data) %>%
        cat(fill = T)
    }

    return(all_data)

  }

#' Gets CV Data for stated periods
#'
#' @param date_data
#' @param include_translations
#' @param file_directory
#' @param remove_files
#' @param empty_trash
#' @param return_message
#' @importFrom urltools domain
#' @importFrom httr url_ok
#' @import stringr dplyr purrr curl lubridate tidyr
#' @importFrom purrr flatten_chr
#' @importFrom curl curl_download
#' @importFrom readr read_tsv
#' @importFrom lubridate ymd_hms with_tz ymd
#' @return
#' @export
#'
#' @examples
get_data_vgkg_dates <-
  function(dates = c("2016-06-09", "2016-06-08"),
           include_translations = FALSE,
           only_most_recent = FALSE,
           remove_json_column = TRUE,
           return_message = TRUE) {
    if (only_most_recent) {
      dates <-
        Sys.Date()
    }

    .get_data_vgkg_day_safe <-
      possibly(.get_data_vgkg_day, tibble())

    all_data <-
      dates %>%
      map_dfr(
        function(x)
          .get_data_vgkg_day_safe(
            date_data = x,
            only_most_recent = only_most_recent,
            remove_json_column = remove_json_column,
            return_message = return_message
          )
      )

    all_data <-
      all_data %>%
      separate(idVGKG, into = c('VGKG', 'remove'), '\\-') %>%
      group_by(dateDocument) %>%
      mutate(count = 1:n()) %>%
      mutate(idVGKG = VGKG %>% paste0('-', count)) %>%
      dplyr::select(-c(count, idDateTime, remove, VGKG)) %>%
      dplyr::select(idVGKG, everything()) %>%
      ungroup()

    all_data <-
      all_data %>%
      mutate(domainSource = documentSource %>% urltools::domain()) %>%
      dplyr::select(idVGKG:dateTimeDocument, domainSource, everything())

    return(all_data)

  }

#' Parses Cloud Vision Lables
#'
#' @param gdelt_data
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#' @import purrr
#' @examples
parse_vgkg_labels <- function(gdelt_data,
                              filter_na = T,
                              return_wide = T) {
  parse_xml_labels_safe <-
    possibly(parse_xml_labels, tibble())

  allxmlLabels <-
    gdelt_data$idVGKG %>%
    map_dfr(function(x) {
      parse_xml_labels_safe(data = gdelt_data, id_vgkg = x)
    }) %>%
    suppressWarnings()

  allxmlLabels <-
    allxmlLabels %>%
    get_clean_count_vkg_data(count_col = 'idImageLabel', return_wide = return_wide)

  if (!return_wide) {
    allxmlLabels <-
      allxmlLabels %>%
      .resolve_long_names()
  }

  return(allxmlLabels)
}

#' Parses Cloud Vision Landmark
#'
#' @param gdelt_data
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
parse_vgkg_landmarks <- function(gdelt_data,
                                 filter_na = T,
                                 return_wide = T) {
  parse_xml_landmarks_safe <-
    possibly(parse_xml_landmarks, tibble())

  all_data <-
    gdelt_data$idVGKG %>%
    map_dfr(function(x) {
      parse_xml_landmarks_safe(data = gdelt_data, id_vgkg = x)
    }) %>%
    suppressWarnings()

  if (filter_na) {
    all_data <-
      all_data %>%
      dplyr::filter(!nameLandmark %>% is.na)
  }

  all_data <-
    all_data %>%
    get_clean_count_vkg_data(count_col = 'idLandmarkImage', return_wide = return_wide)

  if (!return_wide) {
    all_data <-
      all_data %>%
      .resolve_long_names()
  }
  return(all_data)
}



#' Parses Cloud Vision Logos
#'
#' @param gdelt_data
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
parse_vgkg_logos <- function(gdelt_data,
                             filter_na = T,
                             return_wide = T) {
  parse_xml_logos_safe <-
    possibly(parse_xml_logos,tibble())

  all_data <-
    gdelt_data$idVGKG %>%
    map_dfr(function(x) {
      parse_xml_logos_safe(data = gdelt_data, id_vgkg = x)
    }) %>%
    suppressWarnings()

  all_data <-
    all_data %>%
    mutate(midGoogle = ifelse(midGoogle == '', NA, midGoogle))

  if (filter_na) {
    all_data <-
      all_data %>%
      dplyr::filter(!nameLogo %>% is.na())
  }

  all_data <-
    all_data %>%
    get_clean_count_vkg_data(count_col = 'idLogoImage', return_wide = return_wide)

  if (!return_wide) {
    all_data <-
      all_data %>%
      .resolve_long_names() %>%
      suppressWarnings()
  }

  return(all_data)
}

#' Parses Safe Search
#'
#' @param gdelt_data
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
parse_vgkg_safe_search <- function(gdelt_data,
                                   filter_na = T,
                                   return_wide = T) {
  parse_xml_safe_search_safe <-
    possibly(parse_xml_safe_search, tibble())

  all_data <-
    gdelt_data$idVGKG %>%
    map_dfr(function(x) {
      parse_xml_safe_search_safe(data = gdelt_data, id_vgkg = x)
    }) %>%
    suppressWarnings()

  if (filter_na) {
    all_data <-
      all_data %>%
      dplyr::filter(!idSafeSearchImage %>% is.na)
  }

  if (!return_wide) {
    all_data <-
      all_data %>%
      .resolve_long_names()
  }

  return(all_data)
}

#' Parses Cloud Vision Faces
#'
#' @param gdelt_data
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
parse_vgkg_faces <- function(gdelt_data,
                             filter_na = T,
                             return_wide = T) {
  parse_xml_faces_search_safe <-
    possibly(parse_xml_faces, tibble())

  all_data <-
    gdelt_data$idVGKG %>%
    map_dfr(function(x) {
      parse_xml_faces_search_safe(data = gdelt_data, id_vgkg = x)
    }) %>%
    suppressWarnings()

  if (filter_na) {
    all_data <-
      all_data %>%
      dplyr::filter(!scoreDetectionConfidence %>% is.na())
  }

  all_data <-
    all_data %>%
    get_clean_count_vkg_data(count_col = 'idFace', return_wide = return_wide)

  if (!return_wide) {
    all_data <-
      all_data %>%
      .resolve_long_names()
  }

  return(all_data)
}

#' Parses Cloud Vision OCR
#'
#' @param gdelt_data
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
parse_vgkg_ocr <- function(gdelt_data,
                           filter_na = T,
                           return_wide = T) {
  parse_xml_ocr_safe <-
    possibly(parse_xml_ocr, tibble())

  all_data <-
    gdelt_data$idVGKG %>%
    map_dfr(function(x) {
      parse_xml_ocr_safe(data = gdelt_data, id_vgkg = x)
    }) %>%
    suppressWarnings()

  if (filter_na) {
    all_data <-
      all_data %>%
      dplyr::filter(!idItemOCR %>% is.na())
  }

  all_data <-
    all_data %>%
    get_clean_count_vkg_data(count_col = 'idItemOCR', return_wide = return_wide)

  if (!return_wide) {
    all_data <-
      all_data %>%
      .resolve_long_names()
  }

  return(all_data)
}

#' Parses Cloud Vision Languages
#'
#' @param gdelt_data
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
parse_vgkg_languages <- function(gdelt_data,
                                 filter_na = T,
                                 return_wide = T) {
  parse_language_types_safe <-
    possibly(parse_language_types, tibble())

  all_data <-
    gdelt_data$idVGKG %>%
    map_dfr(function(x) {
      parse_language_types_safe(data = gdelt_data, id_vgkg = x)
    }) %>%
    suppressWarnings()

  if (filter_na) {
    all_data <-
      all_data %>%
      dplyr::filter(!idItemLanguage %>% is.na())
  }

  all_data <-
    all_data %>%
    get_clean_count_vkg_data(count_col = 'idItemLanguage', return_wide = return_wide)

  if (!return_wide) {
    all_data <-
      all_data %>%
      .resolve_long_names()
  }

  return(all_data)
}

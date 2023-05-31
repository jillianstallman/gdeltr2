

.parse_xml_extra <-
  function(x =  "<PAGE_LINKS>http://therealdeal.com/2015/05/07/hfz-secures-1b-in-financing-for-high-line-site/;http://therealdeal.com/2015/09/10/uk-hedge-fund-loaning-850m-for-relateds-hudson-yards-resi-tower/;http://therealdeal.com/2016/02/05/hfz-seeks-250m-from-eb-5-investors-for-high-line-condos/;http://therealdeal.com/2016/07/28/macklowe-seeking-1b-loan-for-1-wall-street-conversion/;http://therealdeal.com/2016/08/18/first-look-floor-plans-at-hfzs-high-line-development/;http://therealdeal.com/2016/10/05/inside-gary-barnetts-game-of-real-estate-tetris/;http://therealdeal.com/2016/10/06/six-senses-to-open-hotel-at-hfzs-high-line-project/;http://therealdeal.com/issues_articles/whos-bankrolling-the-boom/;http://therealdeal.com/issues_articles/ziel-feldman-it-was-worth-every-penny/</PAGE_LINKS><PAGE_PRECISEPUBTIMESTAMP>20161025180000</PAGE_PRECISEPUBTIMESTAMP>") {
    safe_xml <-
      possibly(read_xml, otherwise = NULL)

    gdelt_xml <-
      list("<item>", x, "</item>") %>%
      invoke(paste0, .) %>%
      safe_xml()

    if (gdelt_xml %>% length > 0) {
      values <-
        gdelt_xml %>%
        xml_children() %>%
        html_text()

      items <-
        gdelt_xml %>%
        xml_children() %>%
        xml_name()

      xml_df <-
        tibble(item = items, value = values)

      xml_df <-
        xml_df$item %>%
        unique %>%
        map_dfr(function(x) {
          item_value <-
            xml_df %>%
            filter(item == x) %>%
            .$value %>%
            str_split('\\;') %>%
            flatten_chr()

          tibble(item = x, value = item_value) %>%
            group_by(item) %>%
            mutate(idXMLItem = seq_along(item)) %>%
            ungroup() %>%
            dplyr::select(idXMLItem, item, value)
        })
    } else {
      xml_df <-
        tibble()
    }
    return(xml_df)
  }


#' Parse XML Extra
#'
#' @param data
#' @param return_wide
#'
#' @return
#' @import xml2 purrr dplyr tidyr
#' @return
#' @export
#'
#' @examples
parse_xml_extras <-
  function(data, return_wide = FALSE) {
    xml_data <-
      data %>%
      filter(!xmlExtras %>% is.na()) %>%
      select(idGKG, xmlExtras)
    parse_xml_extra_safe <-
      possibly(.parse_xml_extra, NULL)

    xml_extra_df <-
      1:nrow(xml_data) %>%
      map_dfr(function(x) {
        row_data <-
          xml_data %>%
          slice(x)

        has_data <-
          row_data$xmlExtras %>%
          parse_xml_extra_safe() %>% nrow() > 0

        if (has_data) {
          xml_df <-
            row_data$xmlExtras %>%
            parse_xml_extra_safe() %>%
            mutate(idGKG = row_data$idGKG) %>%
            dplyr::select(idGKG, everything())
          return(xml_df)
        }
      })

    xml_extra_df <-
      xml_extra_df %>%
      dplyr::filter(!value %>% is.na()) %>%
      suppressWarnings()

    if (return_wide) {
      xml_extra_df <-
        xml_extra_df %>%
        mutate(idXMLItem = idXMLItem - 1,
               item = ifelse(idXMLItem > 0, paste0(item, idXMLItem), item)) %>%
        dplyr::select(-idXMLItem) %>%
        spread(item, value)
    }

    return(xml_extra_df)
  }

#' Parse XML Labels
#'
#' @param data
#' @param id_dateTime
#'
#' @return
#' @import dplyr
#' @import tidyr
#'
#' @examples

get_clean_count_vkg_data <-
  function(all_counts,
           extra_key = NA,
           count_col = 'idImageLabel',
           return_wide = T) {
    if (!extra_key %>% is.na()) {
      clean_data <-
        all_counts %>%
        dplyr::rename_(count = count_col,
                       ek = extra_key) %>%
        group_by(idVGKG) %>%
        mutate(count = 1:n()) %>%
        ungroup %>%
        mutate(count = count - 1) %>%
        gather(item, value, -c(idVGKG, count, ek), na.rm = T) %>%
        mutate(item = ifelse(count == 0, item, item %>% paste0(count)),
               value = value %>% str_trim) %>%
        separate(
          idVGKG,
          into = c('GKG', 'dateTime'),
          sep = '\\-',
          remove = F
        ) %>%
        mutate(dateTime = dateTime %>% as.numeric) %>%
        arrange(dateTime, count) %>%
        dplyr::select(-c(dateTime, GKG))
    } else {
      clean_data <-
        all_counts %>%
        dplyr::rename_(count = count_col) %>%
        group_by(idVGKG) %>%
        mutate(count = 1:n()) %>%
        ungroup %>%
        mutate(count = count - 1) %>%
        gather(item, value, -c(idVGKG, count), na.rm = T) %>%
        mutate(item = ifelse(count == 0, item, item %>% paste0(count)),
               value = value %>% str_trim) %>%
        separate(
          idVGKG,
          into = c('GKG', 'dateTime'),
          sep = '\\-',
          remove = F
        ) %>%
        mutate(dateTime = dateTime %>% as.numeric) %>%
        arrange(dateTime, count) %>%
        dplyr::select(-c(dateTime, GKG))

    }

    if (return_wide) {
      if (!extra_key %>% is.na()) {
        names_order <-
          c('idVGKG', 'ek',  clean_data$item %>% unique)
        clean_data <-
          clean_data %>%
          dplyr::select(-count)
      } else {
        names_order <-
          c('idVGKG',  clean_data$item %>% unique)
        clean_data <-
          clean_data %>%
          dplyr::select(-count) %>%
          spread(item, value)
      }
      keywords <-
        c(
          'amountValue',
          'latitude',
          'scoreGoldstein',
          'articleWordCount',
          'longitude',
          'countEvent',
          'idTypeLocation',
          'charLoc',
          'length',
          'count',
          'month',
          'day',
          'year',
          'score'
        )

      length_nums <-
        clean_data %>%
        dplyr::select(dplyr::matches(keywords %>% paste0(collapse = '|'))) %>%
        dplyr::select(-dplyr::matches('namePerson|idCountry')) %>%
        names %>% length

      if (length_nums > 0) {
        numeric_vars <-
          clean_data %>%
          dplyr::select(dplyr::matches(keywords %>% paste0(collapse = '|'))) %>%
          dplyr::select(-dplyr::matches('namePerson|idCountry')) %>%
          names
        clean_data <-
          clean_data %>%
          mutate_at(numeric_vars,
                    funs(as.numeric))

      }

      clean_data <-
        clean_data %>%
        dplyr::select_(.dots = names_order) %>%
        separate(
          idVGKG,
          into = c('GKG', 'dateTime'),
          sep = '\\-',
          remove = F
        ) %>%
        mutate(dateTime = dateTime %>% as.numeric) %>%
        arrange(dateTime) %>%
        dplyr::select(-c(GKG, dateTime))

      if (!extra_key %>% is.na()) {
        names(clean_data)[2] <-
          extra_key
      }
    } else {
      clean_data <-
        clean_data %>%
        dplyr::select(-count) %>%
        separate(
          idVGKG,
          into = c('GKG', 'dateTime'),
          sep = '\\-',
          remove = F
        ) %>%
        mutate(dateTime = dateTime %>% as.numeric) %>%
        arrange(dateTime) %>%
        dplyr::select(-c(GKG, dateTime))

    }

    return(clean_data)
  }

parse_xml_labels <-
  function(data, id_vgkg = 1)
  {
    xmlData <-
      data %>%
      dplyr::filter(idVGKG == id_vgkg) %>%
      .$xmlLabels

    if (xmlData %>% is.na()) {
      xml_df <-
        tibble(idVGKG = id_vgkg)
    } else {
      if (xmlData %>% str_detect('<RECORD>')) {
        xmlData <-
          xmlData %>%
          str_split('<RECORD>') %>%
          flatten_chr()
      }


      xml_df <-
        tibble(xmlData) %>%
        separate(
          xmlData,
          into = c('nameLabel', 'scoreConfidenceLabel', 'midGoogle'),
          sep = '<FIELD>'
        ) %>%
        mutate(
          idVGKG = id_vgkg,
          idImageLabel = 1:n(),
          scoreConfidenceLabel = scoreConfidenceLabel %>% as.numeric()
        ) %>%
        dplyr::select(idVGKG, idImageLabel, everything()) %>%
        suppressWarnings()
    }

    return(xml_df)

  }


#' Parses XML Landmarks
#'
#' @param data
#' @param id_vgkg
#'
#' @return
#'
#' @examples
parse_xml_landmarks <-
  function(data, id_vgkg = "20160610194500-2247") {
    xmlData <-
      data %>%
      dplyr::filter(idVGKG == id_vgkg) %>%
      .$xmlGeoLandmarks

    if (xmlData %>% is.na()) {
      xml_df <-
        tibble(idVGKG = id_vgkg)
    } else {
      if (xmlData %>% str_detect('<RECORD>')) {
        xmlData <-
          xmlData %>%
          str_split('<RECORD>') %>%
          flatten_chr()
      }


      xml_df <-
        tibble(xmlData) %>%
        separate(
          xmlData,
          into = c(
            'nameLandmark',
            'scoreConfidenceLandmark',
            'midGoogle',
            'latLonLandmark'
          ),
          sep = '<FIELD>'
        ) %>%
        separate(
          col = 'latLonLandmark',
          sep = '\\,',
          into = c('latitudeLandmark', 'longitudeLandmark')
        ) %>%
        mutate(
          idVGKG = id_vgkg,
          idLandmarkImage = 1:n(),
          scoreConfidenceLandmark = scoreConfidenceLandmark %>% as.numeric(),
          latitudeLandmark = latitudeLandmark %>% as.numeric,
          longitudeLandmark = longitudeLandmark %>% as.numeric
        ) %>%
        dplyr::select(idVGKG, idLandmarkImage, everything()) %>%
        suppressWarnings()
    }
    return(xml_df)

  }

#' Parses XML Logo
#'
#' @param data
#' @param id_vgkg
#'
#' @return
#'
#' @examples
parse_xml_logos <-
  function(data, id_vgkg = 1) {
    xmlData <-
      data %>%
      dplyr::filter(idVGKG == id_vgkg) %>%
      .$xmlLogos

    if (xmlData %>% is.na()) {
      xml_df <-
        tibble(idVGKG = id_vgkg)
    } else {
      xmlData <-
        xmlData %>%
        str_split('<RECORD>') %>%
        flatten_chr()

      xml_df <-
        tibble(xmlData) %>%
        separate(
          xmlData,
          into = c('nameLogo', 'scoreConfidenceLogo', 'midGoogle'),
          sep = '<FIELD>'
        ) %>%
        mutate(
          idVGKG = id_vgkg,
          idLogoImage = 1:n(),
          scoreConfidenceLogo = scoreConfidenceLogo %>% as.numeric()
        ) %>%
        dplyr::select(idVGKG, idLogoImage, everything()) %>%
        suppressWarnings()
    }

    return(xml_df)

  }


#' Parses Safe Search
#'
#' @param data
#' @param id_vgkg
#'
#' @return
#'
#' @examples
parse_xml_safe_search <-
  function(data, id_vgkg = 1) {
    xmlData <-
      data %>%
      dplyr::filter(idVGKG == id_vgkg) %>%
      .$xmlSafeSearch

    if (xmlData %>% is.na()) {
      xml_df <-
        tibble(idVGKG = id_vgkg)
    } else {
      xmlData <-
        xmlData %>%
        str_split('<RECORD>') %>%
        flatten_chr()

      xml_df <-
        tibble(xmlData) %>%
        separate(
          xmlData,
          into = c(
            'scoreViolenceLikelihood',
            'scoreMedicalLikelihood',
            'scoreSpoofLikelihood',
            'scoreAdultLikelihood'
          ),
          sep = '<FIELD>'
        )

      xml_df <-
        xml_df %>%
        mutate_at(xml_df %>% dplyr::select(dplyr::matches("score")) %>% names(),
                  funs(as.integer)) %>%
        mutate(idVGKG = id_vgkg,
               idSafeSearchImage = 1:n()) %>%
        dplyr::select(idVGKG, idSafeSearchImage, everything()) %>%
        suppressWarnings() %>%
        distinct()
    }

    return(xml_df)

  }

#' Parses XML Faces
#'
#' @param data
#' @param id_vgkg
#'
#' @return
#'
#' @examples
parse_xml_faces <-
  function(data, id_vgkg = 1) {
    xmlData <-
      data %>%
      dplyr::filter(idVGKG == id_vgkg) %>%
      .$xmlFaces

    if (xmlData %>% is.na()) {
      xml_df <-
        tibble(idVGKG = id_vgkg)
    } else {
      xmlData <-
        xmlData %>%
        str_split('<RECORD>') %>%
        flatten_chr()

      xml_df <-
        tibble(xmlData) %>%
        separate(
          xmlData,
          into = c(
            'scoreDetectionConfidence',
            'angleRoll',
            'anglePan',
            'angleTilt',
            'scoreLandmarkingConfidence',
            'boxBounding',
            'scoreEmotionSorrowLikelihood',
            'scoreEmotionAngerLikelihood',
            'scoreHeadwearLikelihood',
            'scoreEmotionJoyLikelihood',
            'scoreEmotionSurpriseLikelihood',
            'scoreUnderExposedLikelihood',
            'scoreBlurredLikelihood'
          ),
          sep = '<FIELD>'
        ) %>%
        separate('boxBounding', sep = ';', c('xy1', 'xy2', 'xy3', 'xy4')) %>%
        separate('xy1', sep = ',', c('faceX1', 'faceY1')) %>%
        separate('xy2', sep = ',', c('faceX2', 'faceY2')) %>%
        separate('xy3', sep = ',', c('faceX3', 'faceY3')) %>%
        separate('xy4', sep = ',', c('faceX4', 'faceY4'))

      xml_df <-
        xml_df %>%
        mutate_at(.vars =
                    xml_df %>% dplyr::select(dplyr::matches("score|face|angle")) %>% names(),
                  .funs = as.numeric) %>%
        mutate(idVGKG = id_vgkg,
               idFace = 1:n()) %>%
        dplyr::select(idVGKG, idFace, everything()) %>%
        suppressWarnings() %>%
        distinct()
    }

    return(xml_df)

  }

#' Parses XML OCR
#'
#' @param data
#' @param id_vgkg
#'
#' @return
#'
#' @examples
parse_xml_ocr <-
  function(data, id_vgkg = 1)  {
    xmlData <-
      data %>%
      dplyr::filter(idVGKG == id_vgkg) %>%
      .$xmlOCR

    if (xmlData %>% is.na()) {
      xml_df <-
        tibble(idVGKG = id_vgkg)
    } else {
      xmlData <-
        xmlData %>%
        str_split('<RECORD>') %>%
        flatten_chr() %>%
        gsub('\\n', '', .)

      xml_df <-
        tibble(textOCR = xmlData)

      xml_df <-
        xml_df %>%
        mutate(idVGKG = id_vgkg,
               idItemOCR = 1:n()) %>%
        dplyr::select(idVGKG, idItemOCR, everything()) %>%
        suppressWarnings() %>%
        distinct()
    }
    return(xml_df)

  }

#' Parses XML Language Type
#'
#' @param data
#' @param id_vgkg
#'
#' @return
#'
#' @examples
parse_language_types <-
  function(data, id_vgkg = 1)  {
    xmlData <-
      data %>%
      dplyr::filter(idVGKG == id_vgkg) %>%
      .$codesLanguages

    if (xmlData %>% is.na()) {
      xml_df <-
        tibble(idVGKG = id_vgkg)
    } else {
      xmlData <-
        xmlData %>%
        str_split('\\,') %>%
        flatten_chr()

      xml_df <-
        tibble(idLanguage = xmlData) %>%
        mutate(idVGKG = id_vgkg,
               idItemLanguage = 1:n()) %>%
        dplyr::select(idVGKG, idItemLanguage, everything()) %>%
        suppressWarnings()
    }

    return(xml_df)

  }

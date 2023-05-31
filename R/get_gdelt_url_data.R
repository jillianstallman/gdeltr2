#' Get GDELT url data
#'
#' @param url
#' @param file_directory
#' @param folder_name
#' @param remove_existing_folder
#' @param remove_files
#' @param empty_trash
#' @param return_message
#'
#' @return
#' @export
#' @import readr purrr curl dplyr lubridate tidyr stringr
#' @importFrom urltools domain
#' @examples
get_gdelt_url_data <-
  function(url = "http://data.gdeltproject.org/gdeltv2/20160531000000.gkg.csv.zip",
           file_directory = NULL,
           folder_name = 'gdelt_data',
           remove_existing_folder = T,
           remove_files = T,
           empty_trash = T,
           return_message = T) {

    use_tmp_file <-
      file_directory %>%
      is_null()

    if (use_tmp_file) {
      tmp <-
        tempfile()

      url %>%
        curl_download(url = ., tmp)

      con <-
        unzip(tmp)

      gdelt_cols <-
        con %>%
        read_tsv(col_names = F,
                 n_max = 1) %>% ncol() %>%
        suppressMessages() %>%
        suppressWarnings()

      if (gdelt_cols == 16) {
        gdelt_data <-
          con %>%
          read_tsv(col_names = F) %>%
          suppressWarnings() %>%
          suppressMessages()

        names(gdelt_data) <-
          get_schema_gkg_mentions() %>% .$nameActual

        gdelt_data <-
          gdelt_data %>%
          mutate(
            dateTimeEvent = dateEvent %>% ymd_hms() %>% lubridate::with_tz(Sys.timezone()),
            dateEvent = dateTimeEvent %>% as.Date(),
            dateTimeMention = dateMention %>% ymd_hms() %>% lubridate::with_tz(Sys.timezone()),
            dateMention = dateTimeMention %>% as.Date()
          ) %>%
          dplyr::select(idGlobalEvent,
                        dateTimeEvent,
                        dateTimeMention,
                        everything()) %>%
          dplyr::left_join(tibble(
            idMentionType = 1:6,
            typeMention = c('web', 'citation', 'core', 'dtic', 'jstor', 'nontext')
          )) %>%
          dplyr::select(idGlobalEvent:idMentionType,
                        typeMention,
                        everything()) %>%
          suppressMessages()

        gdelt_data <-
          gdelt_data %>%
          mutate_at(gdelt_data %>% dplyr::select(dplyr::matches("is")) %>% names(),
                    funs(. %>% as.logical()))

      }

      if (gdelt_cols == 15) {
        gdelt_data <-
          con %>%
          read_tsv(col_names = T) %>%
          suppressWarnings() %>%
          suppressMessages()

        names(gdelt_data) <-
          get_schema_gkg_counts() %>% .$nameActual

        gdelt_data <-
          gdelt_data %>%
          dplyr::left_join(tibble(
            idTypeLocation = 1:5,
            typeLocation = c(
              'country',
              'usState',
              'usCity',
              'worldCity',
              'worldState'
            )
          )) %>%
          suppressMessages() %>%
          dplyr::mutate(
            idRecord = 1:n(),
            idGKG = dateEvent %>% paste0('.', idRecord),
            urlSources = urlSources %>% str_replace_all("<UDIV>", ';')
          ) %>%
          dplyr::mutate(dateEvent = dateEvent %>% ymd()) %>%
          dplyr::select(dateEvent:idTypeLocation, typeLocation, everything()) %>%
          dplyr::select(idRecord, idGKG, everything())
      }

      if (gdelt_cols == 61) {
        gdelt_data <-
          con %>%
          read_tsv(col_names = F) %>%
          suppressWarnings() %>%
          suppressMessages()

        names(gdelt_data) <-
          get_schema_gdelt_events() %>% .$nameActual

        gdelt_data <-
          gdelt_data %>%
          dplyr::rename(dateTimeDocument = dateTimeDataAdded) %>%
          dplyr::mutate(
            dateEvent = dateEvent %>% lubridate::ymd,
            dateTimeDocument = dateTimeDocument %>% ymd_hms() %>% lubridate::with_tz(Sys.timezone()),
            nameSource = urlSource %>% urltools::domain() %>% str_replace_all("www.", '')
          )

        gdelt_data <-
          gdelt_data %>%
          mutate_at(gdelt_data %>% dplyr::select(dplyr::matches("is")) %>% names(),
                    funs(. %>% as.logical()))
        gdelt_data <-
          gdelt_data %>%
          dplyr::left_join(tibble(
            classQuad =  1:4,
            nameQuad =  c(
              'Verbal Cooperation',
              'Material Cooperation',
              'Verbal Conflict',
              'Material Conflict'
            )
          )) %>%
          suppressMessages()
      }

      if (gdelt_cols == 57) {
        gdelt_data <-
          con %>%
          read_tsv(col_names = F) %>%
          suppressWarnings() %>%
          suppressMessages()

        names(gdelt_data) <-
          c(
            "idGlobalEvent",
            "dateEvent",
            "monthYearEvent",
            "yearEvent",
            "dateFraction",
            "codeActor1",
            "nameActor1",
            "codeISOActor1",
            "codeCAMEOGroupActor1",
            "codeCAMEOEthnicityActor1",
            "codeCAMEOReligionActor1",
            "codeCAMEOReligion2Actor1",
            "codeCAMEOTypeActor1",
            "codeCAMEOType2Actor1",
            "codeCAMEOType3Actor1",
            "codeActor2",
            "nameActor2",
            "codeISOActor2",
            "codeCAMEOGroupActor2",
            "codeCAMEOEthnicityActor2",
            "codeCAMEOReligionActor2",
            "codeCAMEOReligion2Actor2",
            "codeCAMEOTypeActor2",
            "codeCAMEOType2Actor2",
            "codeCAMEOType3Actor.3",
            "isRootEvent",
            "idCAMEOEvent",
            "idCAMEOEventBase",
            "idCAMEOEventRoot",
            "classQuad",
            "scoreGoldstein",
            "countMentions",
            "countSources",
            "countArticles",
            "avgTone",
            "idTypeLocationActor1",
            "locationActor1",
            "idCountryActor1",
            "idADM1CodeActor1",
            "latitudeActor1",
            "longitudeActor1",
            "idFeatureActor1",
            "idTypeLocationActor2",
            "locationActor2",
            "idCountryActor2",
            "idADM1CodeActor2",
            "latitudeActor2",
            "longitudeActor2",
            "idFeatureActor2",
            "idTypeLocationAction",
            "locationAction",
            "idCountryAction",
            "idADM1CodeAction",
            "latitudeAction",
            "longitudeAction",
            "idFeatureAction",
            "dateAdded"
          )

        gdelt_data <-
          gdelt_data %>%
          dplyr::rename(dateDocument = dateAdded) %>%
          dplyr::mutate(
            dateEvent = dateEvent %>% lubridate::ymd,
            dateDocument = dateDocument %>% lubridate::ymd
          ) %>%
          suppressWarnings()

        gdelt_data <-
          gdelt_data %>%
          mutate_at(gdelt_data %>% dplyr::select(dplyr::matches("is")) %>% names(),
                    funs(. %>% as.logical()))

        gdelt_data <-
          gdelt_data %>%
          dplyr::left_join(tibble(
            classQuad =  1:4,
            nameQuad =  c(
              'Verbal Cooperation',
              'Material Cooperation',
              'Verbal Conflict',
              'Material Conflict'
            )
          )) %>%
          suppressMessages()
      }

      if (gdelt_cols == 58) {
        gdelt_data <-
          con %>%
          read_tsv(col_names = F) %>%
          suppressWarnings() %>%
          suppressMessages()

        names(gdelt_data) <-
          c(
            "idGlobalEvent",
            "dateEvent",
            "monthYearEvent",
            "yearEvent",
            "dateFraction",
            "codeActor1",
            "nameActor1",
            "codeISOActor1",
            "codeCAMEOGroupActor1",
            "codeCAMEOEthnicityActor1",
            "codeCAMEOReligionActor1",
            "codeCAMEOReligion2Actor1",
            "codeCAMEOTypeActor1",
            "codeCAMEOType2Actor1",
            "codeCAMEOType3Actor1",
            "codeActor2",
            "nameActor2",
            "codeISOActor2",
            "codeCAMEOGroupActor2",
            "codeCAMEOEthnicityActor2",
            "codeCAMEOReligionActor2",
            "codeCAMEOReligion2Actor2",
            "codeCAMEOTypeActor2",
            "codeCAMEOType2Actor2",
            "codeCAMEOType3Actor.3",
            "isRootEvent",
            "idCAMEOEvent",
            "idCAMEOEventBase",
            "idCAMEOEventRoot",
            "classQuad",
            "scoreGoldstein",
            "countMentions",
            "countSources",
            "countArticles",
            "avgTone",
            "idTypeLocationActor1",
            "locationActor1",
            "idCountryActor1",
            "idADM1CodeActor1",
            "latitudeActor1",
            "longitudeActor1",
            "idFeatureActor1",
            "idTypeLocationActor2",
            "locationActor2",
            "idCountryActor2",
            "idADM1CodeActor2",
            "latitudeActor2",
            "longitudeActor2",
            "idFeatureActor2",
            "idTypeLocationAction",
            "locationAction",
            "idCountryAction",
            "idADM1CodeAction",
            "latitudeAction",
            "longitudeAction",
            "idFeatureAction",
            "dateAdded",
            "urlSource"
          )

        gdelt_data <-
          gdelt_data %>%
          dplyr::rename(dateTimeDocument = dateAdded) %>%
          dplyr::mutate(
            dateEvent = dateEvent %>% lubridate::ymd(),
            nameSource = urlSource %>% urltools::domain() %>% str_replace_all("www.", '')
          ) %>%
          suppressWarnings()

        gdelt_data <-
          gdelt_data %>%
          dplyr::mutate_at(gdelt_data %>% dplyr::select(dplyr::matches("is")) %>% names(),
                           funs(. %>% as.logical()))

        gdelt_data <-
          gdelt_data %>%
          dplyr::left_join(tibble(
            classQuad =  1:4,
            nameQuad =  c(
              'Verbal Cooperation',
              'Material Cooperation',
              'Verbal Conflict',
              'Material Conflict.'
            )
          )) %>%
          suppressMessages()

      }

      if (gdelt_cols == 11) {
        gdelt_data <-
          con %>%
          read_tsv(col_names = T) %>%
          suppressWarnings() %>%
          suppressMessages()

        schema_df <-
          get_schema_gkg_general()

        names(gdelt_data) <-
          schema_df$nameActual[names(gdelt_data) %>% match(schema_df$nameGDELT)]

        names(gdelt_data)[1] <-
          c('date')

        gdelt_data <-
          gdelt_data %>%
          dplyr::mutate(
            idRecord = 1:n(),
            idGKG = date %>% paste0('.', idRecord),
            date = date %>% lubridate::ymd(),
            urlSources = urlSources %>% str_replace_all("<UDIV>", ';')
          ) %>%
          dplyr::select(idRecord, idGKG, everything())

      }

      if (gdelt_cols == 27) {
        gdelt_data <-
          con %>%
          read_tsv(col_names = F) %>%
          suppressWarnings() %>%
          suppressMessages()

        schema_df <-
          get_schema_gkg_general()

        names(gdelt_data) <-
          schema_df$nameActual[1:27]

        gdelt_data <-
          gdelt_data %>%
          dplyr::mutate(
            idSourceCollectionIdentifier = idSourceCollectionIdentifier %>% as.numeric(),
            isDocumentURL = ifelse(documentSource %>% str_detect('http'), T, F)
          ) %>%
          dplyr::select(idGKG:idSourceCollectionIdentifier,
                        isDocumentURL,
                        everything()) %>%
          dplyr::rename(dateTimeDocument = dateDocument) %>%
          dplyr::mutate(
            dateTimeDocument = dateTimeDocument %>% lubridate::ymd_hms() %>% lubridate::with_tz(Sys.timezone())
          ) %>%
          separate(
            idGKG,
            into = c('dateTime', 'idDateTimeArticle'),
            sep = '\\-',
            remove = F
          ) %>%
          dplyr::select(-c(dateTime, translationInfo)) %>%
          dplyr::mutate(
            idDateTimeArticle = idDateTimeArticle %>% as.numeric,
            domainSource = if_else(
              isDocumentURL == T,
              documentSource %>% urltools::domain(),
              nameSource
            )
          ) %>%
          dplyr::select(idGKG:documentSource, domainSource, everything()) %>%
          suppressMessages() %>%
          suppressWarnings()

      }
      con %>%
        unlink()
    }  else {
      only_folder <-
        !folder_name %>% is_null() &
        file_directory %>% is_null()
      if (only_folder) {
        file_directory <-
          getwd()
      }
      file_directory <-
        file_directory %>%
        paste0('/', folder_name)

      file <-
        url %>% basename()

      temp.dir <-
        file_directory

      file_path <-
        temp.dir %>% str_split('/') %>% flatten_chr() %>% .[seq_along(.)] %>% paste0(collapse = '/')
      if (remove_existing_folder) {
        if (dir.exists(paths = file_path)) {
          "rm -R " %>%
            paste0(temp.dir) %>%
            system()
          if (empty_trash) {
            system('rm -rf ~/.Trash/*')
          }
        }
      }

      if (!dir.exists(paths = file_path)) {
        dir.create(temp.dir)
      }

      file <-
        temp.dir %>%
        paste0('/', file)

      url %>%
        curl_download(url = ., destfile = file)

      file %>%
        unzip(exdir = paste0(temp.dir, '/'))

      dir_files <-
        temp.dir %>%
        list.files()

      file_name <-
        dir_files %>%
        str_detect('CSV|csv|TXT|txt|XLS|XLSX|xlsx|xls') %>%
        dir_files[.]

      csv_file_loc <-
        file_name[!file_name %>% str_detect("zip")] %>%
        paste0(file_directory, '/', .)

      gdelt_cols <-
        csv_file_loc %>%
        read_tsv(col_names = F,
                 n_max = 1) %>% ncol %>% suppressWarnings() %>%
        as.character() %>%
        parse_number() %>%
        suppressMessages()

      if (gdelt_cols == 16) {
        gdelt_data <-
          csv_file_loc %>%
          read_tsv(col_names = F) %>%
          suppressWarnings() %>%
          suppressMessages()

        names(gdelt_data) <-
          get_schema_gkg_mentions() %>% .$nameActual

        gdelt_data <-
          gdelt_data %>%
          mutate(
            dateTimeEvent = dateEvent %>% ymd_hms %>% with_tz(Sys.timezone()),
            dateEvent = dateTimeEvent %>% as.Date(),
            dateTimeMention = dateMention %>% ymd_hms %>% with_tz(Sys.timezone()),
            dateMention = dateTimeMention %>% as.Date()
          ) %>%
          dplyr::select(idGlobalEvent,
                        dateTimeEvent,
                        dateTimeMention,
                        everything()) %>%
          dplyr::left_join(tibble(
            idMentionType = 1:6,
            mention_type = c('web', 'citation', 'core', 'dtic', 'jstor', 'nontext')
          )) %>%
          dplyr::select(idGlobalEvent:idMentionType,
                        mention_type,
                        everything()) %>%
          suppressMessages()

        gdelt_data <-
          gdelt_data %>%
          mutate_at(.vars = gdelt_data %>% dplyr::select(dplyr::matches("is")) %>% names(),
                    .funs = as.logical)
      }

      if (gdelt_cols == 15) {
        gdelt_data <-
          csv_file_loc %>%
          read_tsv(col_names = T) %>%
          suppressWarnings() %>%
          suppressMessages()

        names(gdelt_data) <-
          get_schema_gkg_counts() %>% .$nameActual

        gdelt_data <-
          gdelt_data %>%
          dplyr::left_join(tibble(
            idTypeLocation = 1:5,
            typeLocation = c(
              'country',
              'usState',
              'usCity',
              'worldCity',
              'worldState'
            )
          )) %>%
          suppressMessages() %>%
          dplyr::mutate(
            idRecord = 1:n(),
            idGKG = dateEvent %>% paste0('.', idRecord),
            urlSources = urlSources %>% str_replace_all("<UDIV>", ';')
          ) %>%
          dplyr::mutate(dateEvent = dateEvent %>% ymd()) %>%
          dplyr::select(dateEvent:idTypeLocation, typeLocation, everything()) %>%
          dplyr::select(idRecord, idGKG, everything())
      }

      if (gdelt_cols == 61) {
        gdelt_data <-
          csv_file_loc %>%
          read_tsv(col_names = F) %>%
          suppressWarnings() %>%
          suppressMessages()

        names(gdelt_data) <-
          get_schema_gdelt_events() %>% .$nameActual

        gdelt_data <-
          gdelt_data %>%
          dplyr::rename(dateTimeDocument = dateTimeDataAdded) %>%
          dplyr::mutate(
            dateEvent = dateEvent %>% lubridate::ymd,
            dateTimeDocument = dateTimeDocument %>% ymd_hms() %>% with_tz(Sys.timezone()),
            nameSource = urlSource %>% domain() %>% str_replace_all("www.", '')
          )

        gdelt_data <-
          gdelt_data %>%
          dplyr::mutate_at(gdelt_data %>% dplyr::select(dplyr::matches("is")) %>% names(),
                           funs(. %>% as.logical()))

        gdelt_data <-
          gdelt_data %>%
          dplyr::left_join(tibble(
            classQuad =  1:4,
            nameQuad =  c(
              'Verbal Cooperation',
              'Material Cooperation',
              'Verbal Conflict',
              'Material Conflict'
            )
          )) %>%
          suppressMessages()
      }

      if (gdelt_cols == 57) {
        gdelt_data <-
          csv_file_loc %>%
          read_tsv(col_names = F) %>%
          suppressWarnings() %>%
          suppressMessages()

        names(gdelt_data) <-
          c(
            "idGlobalEvent",
            "dateEvent",
            "monthYearEvent",
            "yearEvent",
            "dateFraction",
            "codeActor1",
            "nameActor1",
            "codeISOActor1",
            "codeCAMEOGroupActor1",
            "codeCAMEOEthnicityActor1",
            "codeCAMEOReligionActor1",
            "codeCAMEOReligion2Actor1",
            "codeCAMEOTypeActor1",
            "codeCAMEOType2Actor1",
            "codeCAMEOType3Actor1",
            "codeActor2",
            "nameActor2",
            "codeISOActor2",
            "codeCAMEOGroupActor2",
            "codeCAMEOEthnicityActor2",
            "codeCAMEOReligionActor2",
            "codeCAMEOReligion2Actor2",
            "codeCAMEOTypeActor2",
            "codeCAMEOType2Actor2",
            "codeCAMEOType3Actor.3",
            "isRootEvent",
            "idCAMEOEvent",
            "idCAMEOEventBase",
            "idCAMEOEventRoot",
            "classQuad",
            "scoreGoldstein",
            "countMentions",
            "countSources",
            "countArticles",
            "avgTone",
            "idTypeLocationActor1",
            "locationActor1",
            "idCountryActor1",
            "idADM1CodeActor1",
            "latitudeActor1",
            "longitudeActor1",
            "idFeatureActor1",
            "idTypeLocationActor2",
            "locationActor2",
            "idCountryActor2",
            "idADM1CodeActor2",
            "latitudeActor2",
            "longitudeActor2",
            "idFeatureActor2",
            "idTypeLocationAction",
            "locationAction",
            "idCountryAction",
            "idADM1CodeAction",
            "latitudeAction",
            "longitudeAction",
            "idFeatureAction",
            "dateAdded"
          )

        gdelt_data <-
          gdelt_data %>%
          dplyr::rename(dateDocument = dateAdded) %>%
          dplyr::mutate(
            dateEvent = dateEvent %>% lubridate::ymd,
            dateDocument = dateDocument %>% lubridate::ymd
          ) %>%
          suppressWarnings()

        gdelt_data <-
          gdelt_data %>%
          dplyr::mutate_at(gdelt_data %>% dplyr::select(dplyr::matches("is")) %>% names(),
                           funs(. %>% as.logical()))

        gdelt_data <-
          gdelt_data %>%
          dplyr::left_join(tibble(
            classQuad =  1:4,
            nameQuad =  c(
              'Verbal Cooperation',
              'Material Cooperation',
              'Verbal Conflict',
              'Material Conflict'
            )
          )) %>%
          suppressMessages()
      }

      if (gdelt_cols == 58) {
        load_needed_packages(c('urltools'))
        gdelt_data <-
          csv_file_loc %>%
          read_tsv(col_names = F) %>%
          suppressWarnings() %>%
          suppressMessages()

        names(gdelt_data) <-
          c(
            "idGlobalEvent",
            "dateEvent",
            "monthYearEvent",
            "yearEvent",
            "dateFraction",
            "codeActor1",
            "nameActor1",
            "codeISOActor1",
            "codeCAMEOGroupActor1",
            "codeCAMEOEthnicityActor1",
            "codeCAMEOReligionActor1",
            "codeCAMEOReligion2Actor1",
            "codeCAMEOTypeActor1",
            "codeCAMEOType2Actor1",
            "codeCAMEOType3Actor1",
            "codeActor2",
            "nameActor2",
            "codeISOActor2",
            "codeCAMEOGroupActor2",
            "codeCAMEOEthnicityActor2",
            "codeCAMEOReligionActor2",
            "codeCAMEOReligion2Actor2",
            "codeCAMEOTypeActor2",
            "codeCAMEOType2Actor2",
            "codeCAMEOType3Actor.3",
            "isRootEvent",
            "idCAMEOEvent",
            "idCAMEOEventBase",
            "idCAMEOEventRoot",
            "classQuad",
            "scoreGoldstein",
            "countMentions",
            "countSources",
            "countArticles",
            "avgTone",
            "idTypeLocationActor1",
            "locationActor1",
            "idCountryActor1",
            "idADM1CodeActor1",
            "latitudeActor1",
            "longitudeActor1",
            "idFeatureActor1",
            "idTypeLocationActor2",
            "locationActor2",
            "idCountryActor2",
            "idADM1CodeActor2",
            "latitudeActor2",
            "longitudeActor2",
            "idFeatureActor2",
            "idTypeLocationAction",
            "locationAction",
            "idCountryAction",
            "idADM1CodeAction",
            "latitudeAction",
            "longitudeAction",
            "idFeatureAction",
            "dateAdded",
            "urlSource"
          )

        gdelt_data <-
          gdelt_data %>%
          dplyr::rename(dateTimeDocument = dateAdded) %>%
          dplyr::mutate(
            dateEvent = dateEvent %>% lubridate::ymd,
            dateTimeDocument %>% lubridate::ymd_hms() %>% with_tz(Sys.timezone()),
            dateDocument = dateTimeDocument %>% as.Date(),
            nameSource = urlSource %>% domain() %>% str_replace_all("www.", '')
          ) %>%
          suppressWarnings()

        gdelt_data <-
          gdelt_data %>%
          dplyr::mutate_at(gdelt_data %>% dplyr::select(dplyr::matches("is")) %>% names(),
                           funs(. %>% as.logical()))

        gdelt_data <-
          gdelt_data %>%
          dplyr::left_join(tibble(
            classQuad =  1:4,
            nameQuad =  c(
              'Verbal Cooperation',
              'Material Cooperation',
              'Verbal Conflict',
              'Material Conflict.'
            )
          )) %>%
          suppressMessages()

      }

      if (gdelt_cols == 11) {
        gdelt_data <-
          csv_file_loc %>%
          read_tsv(col_names = T) %>%
          suppressWarnings() %>%
          suppressMessages()

        schema_df <-
          get_schema_gkg_general()

        names(gdelt_data) <-
          schema_df$nameActual[names(gdelt_data) %>% match(schema_df$nameGDELT)]

        names(gdelt_data)[1] <-
          c('date')

        gdelt_data <-
          gdelt_data %>%
          dplyr::mutate(
            idRecord = 1:n(),
            idGKG = date %>% paste0('.', idRecord),
            date = date %>% lubridate::ymd(),
            urlSources = urlSources %>% str_replace_all("<UDIV>", ';')
          ) %>%
          dplyr::select(idRecord, idGKG, everything())

      }

      if (gdelt_cols == 27) {
        gdelt_data <-
          csv_file_loc %>%
          read_tsv(col_names = F) %>%
          suppressWarnings() %>%
          suppressMessages()

        schema_df <-
          get_schema_gkg_general()

        names(gdelt_data) <-
          schema_df$nameActual[1:27]

        gdelt_data <-
          gdelt_data %>%
          dplyr::mutate(
            idSourceCollectionIdentifier = idSourceCollectionIdentifier %>% as.numeric(),
            isDocumentURL = ifelse(documentSource %>% str_detect('http'), T, F)
          ) %>%
          dplyr::select(idGKG:idSourceCollectionIdentifier,
                        isDocumentURL,
                        everything()) %>%
          dplyr::rename(dateTimeDocument = dateDocument) %>%
          dplyr::mutate(dateTimeDocument = dateTimeDocument %>% lubridate::ymd_hms() %>% with_tz(Sys.timezone())) %>%
          separate(
            idGKG,
            into = c('dateTime', 'idDateTimeArticle'),
            sep = '\\-',
            remove = F
          ) %>%
          dplyr::select(-c(dateTime, translationInfo)) %>%
          dplyr::mutate(
            idDateTimeArticle = idDateTimeArticle %>% as.numeric,
            domainSource = if_else(
              isDocumentURL == T,
              documentSource %>% urltools::domain,
              nameSource
            )
          ) %>%
          dplyr::select(idGKG:documentSource, domainSource, everything()) %>%
          suppressMessages() %>%
          suppressWarnings()

      }

      if (remove_files) {
        "rm -R " %>%
          paste0(temp.dir) %>%
          system()
        if (empty_trash) {
          system('rm -rf ~/.Trash/*')
        }
      }


    }
    if ('idADM1CodeActor1' %in% names(gdelt_data)) {
      gdelt_data <-
        gdelt_data %>%
        mutate(idADM1CodeActor1 = idADM1CodeActor1 %>% as.character())

    }

    if ('idADM1CodeActor2' %in% names(gdelt_data)) {
      gdelt_data <-
        gdelt_data %>%
        mutate(idADM1CodeActor2 = idADM1CodeActor2 %>% as.character())

    }

    if ('idADM2CodeActor1' %in% names(gdelt_data)) {
      gdelt_data <-
        gdelt_data %>%
        mutate(idADM2CodeActor1 = idADM2CodeActor1 %>% as.character())

    }

    if ('idADM2CodeActor2' %in% names(gdelt_data)) {
      gdelt_data <-
        gdelt_data %>%
        mutate(idADM2CodeActor2 = idADM2CodeActor2 %>% as.character())

    }

    if ('idADM1CodeAction' %in% names(gdelt_data)) {
      gdelt_data <-
        gdelt_data %>%
        mutate(idADM1CodeAction = idADM1CodeAction %>% as.character())

    }

    if ('idADM2CodeAction' %in% names(gdelt_data)) {
      gdelt_data <-
        gdelt_data %>%
        mutate(idADM2CodeAction = idADM2CodeAction %>% as.character())

    }

    if ('idCAMEOEvent' %in% names(gdelt_data)) {
      gdelt_data <-
        gdelt_data %>%
        mutate_at(
          .vars = gdelt_data %>% dplyr::select(dplyr::matches('idCAMEOEvent')) %>% names(),
          funs(. %>% as.character() %>% as.numeric())
        )

    }

    if ('idFeatureActor' %in% names(gdelt_data)) {
      gdelt_data <-
        gdelt_data %>%
        mutate_at(.vars = gdelt_data %>% dplyr::select(dplyr::matches('idFeatureActor')) %>% names(),
                  .funs = as.character)

    }

    if (return_message) {
      "Downloaded, parsed and imported " %>%
        paste0(url) %>%
        cat(fill = T)
    }
    return(gdelt_data)
  }
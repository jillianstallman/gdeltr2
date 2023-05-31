# data_ac -----------------------------------------------------------------


.get_data_gkg_day_detailed <-
  function(date_data = "2016-06-01",
           table_name = "gkg",
           file_directory =  NULL,
           folder_name = 'gdelt_data',
           only_most_recent = F,
           remove_files = T,
           empty_trash = T,
           return_message = T) {
    if (only_most_recent) {
      date_data <-
        Sys.Date()
    }

    if (!date_data %>% substr(5, 5) == "-") {
      stop("Sorry data must be in YMD format, ie, 2016-06-01")
    }
    tables <-
      c('gkg', 'export', 'mentions')
    if (!table_name %in% tables) {
      stop("Sorry tables can only be:\n" %>% paste0(paste0(tables, collapse = '\n')))
    }

    date_data <-
      date_data %>%
      ymd() %>% as.Date()


    if (date_data < "2015-02-18") {
      stop("Sorry data starts on February 18th, 2015")
    }

    if (date_data > Sys.Date()) {
      stop("Sorry data can't go into the future")
    }

    if (only_most_recent) {
      gdelt_detailed_logs <-
        get_urls_gkg_most_recent_log()
      urls <-
        gdelt_detailed_logs %>%
        dplyr::filter(nameFile == table_name) %>%
        .$urlData
    } else {
      if (!'gdelt_detailed_logs' %>% exists()) {
        gdelt_detailed_logs <-
          get_urls_gkg_15_minute_log()

        assign(x = 'gdelt_detailed_logs',
               eval(gdelt_detailed_logs),
               env = .GlobalEnv)

      }

      urls <-
        gdelt_detailed_logs %>%
        dplyr::filter(dateData == date_data) %>%
        dplyr::filter(nameFile == table_name) %>%
        .$urlData
    }

    get_gdelt_url_data_safe <-
      possibly(get_gdelt_url_data, tibble())

    all_data <-
      urls %>%
      map_dfr(function(x) {
        data <-
          get_gdelt_url_data_safe(
            url = x,
            remove_files = remove_files,
            file_directory = file_directory,
            folder_name = folder_name,
            return_message = return_message,
            empty_trash = empty_trash
          )
        data %>%
          mutate_at(.vars = data %>% dplyr::select(dplyr::matches('idFeatureActor|idFeatureAction')) %>% names(),
                    .funs = as.character)
      }) %>%
      distinct() %>%
      suppressWarnings()

    if ('domainSource' %in% names(all_data)) {
      all_data <-
        all_data %>%
        mutate(domainSource = documentSource %>% urltools::domain())
    }

    if ('urlSource' %in% names(all_data)) {
      all_data <-
        all_data %>%
        mutate(domainSource = urlSource %>% urltools::domain())
    }

    if (return_message) {
      "You retrieved " %>%
        paste0(all_data %>% nrow, " gkg detailed events for ", date_data) %>%
        cat(fill = T)
    }

    return(all_data)
  }


#' Get dates detailed data from a specified table
#'
#' @param date_data must be a date in Year - Month - Day format
#' @param table_name the name of the table
#' options \code{c('gkg', 'export', 'mentions'))}
#' @param file_directory where are the files to be saved
#' @param remove_files Do you want to remove the files
#' \code{T, F}
#' @param empty_trash Do You want to empy the trash
#' \code{T, F}
#' @param return_message Do you want to return a message
#' \code{T, F}
#' @importFrom purrr flatten_chr
#' @importFrom readr parse_number
#' @importFrom purrr compact
#' @import dplyr utils dplyr purrr readr
#' @importFrom urltools domain
#' @importFrom curl curl_download curl
#' @importFrom urltools domain
#' @importFrom purrr map
#' @return
#' @export
#'
#' @examples

get_data_gkg_days_detailed <-
  function(dates = c("2016-07-19"),
           table_name = c("gkg"),
           file_directory = NULL,
           folder_name = 'gdelt_data',
           only_most_recent = F,
           remove_files = T,
           empty_trash = T,
           return_message = T) {
    .get_data_gkg_day_detailed_safe <-
      possibly(.get_data_gkg_day_detailed, tibble())

    var_matrix <-
      expand.grid(date = dates,
                  table_name = table_name,
                  stringsAsFactors = F) %>%
      as_tibble()

    all_data <-
      seq_len(var_matrix %>% nrow()) %>%
      map_dfr(
        function(x)
          .get_data_gkg_day_detailed_safe(
            date_data = var_matrix$date[x],
            table_name = var_matrix$table_name[x],
            only_most_recent = only_most_recent,
            file_directory = file_directory,
            folder_name = folder_name,
            remove_files = remove_files,
            empty_trash = empty_trash,
            return_message = return_message
          )
      ) %>%
      suppressWarnings()

    if ('idDateTime' %in% names(all_data)) {
      all_data <-
        all_data %>%
        dplyr::rename(idDateTime = idDateTimeArticle) %>%
        dplyr::mutate(idDateTime = 1:n()) %>%
        separate(idGKG, sep = '\\-', c('dateCode', 'remove')) %>%
        unite(idGKG,
              dateCode,
              idDateTime,
              sep = '-',
              remove = F) %>%
        dplyr::select(-c(dateCode, remove))
    }

    return(all_data)
  }
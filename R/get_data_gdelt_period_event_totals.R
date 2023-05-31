

#' Retrieves GDELT event summary by period
#'
#' @param period can be \code{c("yearly", "daily", "monthly")}
#' @param by_country is data by country
#' can be \code{c(TRUE, FALSE)}
#' @param return_message returns message
#' @importFrom readr read_csv
#' @return
#' @export
#'
#' @examples
#' get_data_gdelt_period_event_totals(period = 'monthly', by_country = T)

get_data_gdelt_period_event_totals <-
  function(period = 'yearly',
           by_country = T,
           return_message = T) {
    periods <-
      c('daily', 'monthly', 'yearly')
    if (!period %in% periods) {
      "Sorry periods can only be:\n" %>%
        stop(paste0(paste0(periods, collapse = '\n')))
    }

    if (by_country) {
      period_slug <-
        period %>%
        paste0('_country.csv')
    } else {
      period_slug <-
        period %>%
        paste0('.csv')
    }
    base <-
      'http://data.gdeltproject.org/normfiles/'

    url_data <-
      base %>%
      paste0(period_slug)

    period_data <-
      url_data %>%
      read_csv(col_names = F) %>%
      suppressWarnings() %>%
      suppressMessages()

    if (by_country) {
      names(period_data) <-
        c('idDate', 'idCountry', 'countEvents')
    } else {
      names(period_data) <-
        c('idDate', 'countEvents')
    }

    period_data <-
      period_data %>%
      mutate(periodData = period, isByCountry = by_country)

    if (period == 'daily') {
      period_data <-
        period_data %>%
        mutate(dateData = idDate %>% lubridate::ymd() %>% as.Date()) %>%
        dplyr::select(periodData, isByCountry, dateData, everything())
    }

    if (period == 'monthly') {
      period_data <-
        period_data %>%
        mutate(yearMonth = idDate) %>%
        dplyr::select(periodData, isByCountry, yearMonth, everything()) %>%
        mutate(
          yearData = yearMonth %>% substr(1, 4) %>% as.numeric(),
          monthData = yearMonth %>% substr(5, 6) %>% as.numeric()
        ) %>%
        select(periodData, isByCountry, yearData, monthData, everything())
    }

    if (period == 'yearly') {
      period_data <-
        period_data %>%
        mutate(yearData = idDate) %>%
        dplyr::select(periodData, isByCountry, everything())
    }

    if (return_message)  {
      from_date <-
        period_data$idDate %>% min()

      to_date <-
        period_data$idDate %>% max()

      total_events <-
        period_data$countEvents %>% sum() / 1000000
      events_slug <-
        total_events %>% paste0(" million GDELT events from ")
      "There have been " %>%
        paste0(events_slug, from_date,
               ' to ', to_date) %>%
        message
    }
    return(period_data)
  }
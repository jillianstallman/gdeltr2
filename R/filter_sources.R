#' Filters a gdelt data frame to specified sources
#'
#' @param data gkg or vgkg data frame
#' @param sources vector of sources
#'
#' @return
#' @export
#'
#' @examples
filter_sources <-
  function(data,
           sources = c(
             'netsdaily',
             'realdeal',
             'curbed',
             'law360',
             'dailymail',
             'wsj.com',
             'law360',
             'pehub',
             'techcrunch',
             'washingtonpost',
             'bloomberg',
             'archdaily',
             'espn.com',
             'venturebeat'
           )) {
    sources <-
      sources %>%
      str_to_lower() %>%
      paste0(collapse = '|')
    if (!'documentSource' %in% names(data)) {
      stop("Data must count the documentSource colum")
    }
    data <-
      data %>%
      filter(documentSource %>% str_detect(sources))

    return(data)
  }
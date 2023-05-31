#' Retrives most recent GDELT Global Content Analysis Measures (GCAM) code book
#'
#'
#' @return
#' @export
#' @importFrom magrittr %>%
#' @importFrom readr read_tsv
#' @examples
#' get_codes_gcam()
get_codes_gcam <- function() {
  url <-
    'http://data.gdeltproject.org/documentation/GCAM-MASTER-CODEBOOK.TXT'
  gcam_data <-
    url %>%
    read_tsv() %>%
    suppressWarnings() %>%
    suppressMessages()

  names(gcam_data) <-
    c(
      'idGCAM',
      'idDictionary',
      'idDimension',
      'typeDictionary',
      'codeLanguage',
      'dictionaryHumanName',
      'dimensionHumanName',
      'dictionaryCitation'
    )

  gcam_data
}


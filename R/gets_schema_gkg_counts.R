#' Gets gkg count schema
#'
#' @return
#'
#' @examples
#' get_schema_gkg_counts()

get_schema_gkg_counts <- function() {
  counts_schema <-
    tibble(
      nameGDELT = c(
        "DATE",
        "NUMARTS",
        "COUNTTYPE",
        "NUMBER",
        "OBJECTTYPE",
        "GEO_TYPE",
        "GEO_FULLNAME",
        "GEO_COUNTRYCODE",
        "GEO_ADM1CODE",
        "GEO_LAT",
        "GEO_LONG",
        "GEO_FEATUREID",
        "CAMEOEVENTIDS",
        "SOURCES",
        "SOURCEURLS"
      ),
      nameActual = c(
        "dateEvent",
        "countArticles",
        "typeEvent",
        "countObject",
        "typeObject",
        "idTypeLocation",
        "location",
        "idCountry",
        "idADM1CodeAction",
        "latitude",
        "longitude",
        "idFeature",
        "idCAMEOEvents",
        "sources",
        "urlSources"
      )
    )
  return(counts_schema)
}
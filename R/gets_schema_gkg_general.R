#' Gets gkg general schema
#'
#' @return
#'
#' @examples
#' get_schema_gkg_general()
get_schema_gkg_general <- function() {
  schema_df <-
    tibble(
      nameGDELT = c(
        "GKGRECORDID",
        "DATE",
        "SourceCollectionIdentifier",
        "SourceCommonName",
        "DocumentIdentifier",
        "Counts",
        "V2Counts",
        "Themes",
        "V2Themes",
        "Locations",
        "V2Locations",
        "Persons",
        "V2Persons",
        "Organizations",
        "V2Organizations",
        "V2Tone",
        "Dates",
        "GCAM",
        "SharingImage",
        "RelatedImages",
        "SocialImageEmbeds",
        "SocialVideoEmbeds",
        "Quotations",
        "AllNames",
        "Amounts",
        "TranslationInfo",
        "Extras",
        "NUMARTS",
        "COUNTS",
        "THEMES",
        "LOCATIONS",
        "PERSONS",
        "ORGANIZATIONS",
        "TONE",
        "CAMEOEVENTIDS",
        "SOURCES",
        "SOURCEURLS"

      ),
      nameActual = c(
        "idGKG",
        "dateDocument",
        "idSourceCollectionIdentifier",
        "nameSource",
        "documentSource",
        "counts",
        "countsCharLoc",
        "themes",
        "themesCharLoc",
        "locations",
        "locationsCharLoc",
        "persons",
        "personsCharLoc",
        "organizations",
        "organizationsCharLoc",
        "tone",
        "dates",
        "gcam",
        "urlImage",
        "urlImageRelated",
        "urlSocialMediaImageEmbeds",
        "urlSocialMediaVideoEmbeds",
        "quotations",
        "mentionedNamesCounts",
        "mentionedNumericsCounts",
        "translationInfo",
        "xmlExtras",
        "countArticles",
        "counts",
        "themes",
        "locations",
        "persons",
        "organizations",
        "tone",
        "idCAMEOEvents",
        "sources",
        "urlSources"
      )
    )
  return(schema_df)
}
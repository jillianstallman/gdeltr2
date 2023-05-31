#' Gets gkg mention schema
#'
#' @return
#'
#' @examples
#' get_schema_gkg_mentions()
get_schema_gkg_mentions <- function() {
  mentions_schema <-
    tibble(
      nameGDELT =
        c(
          "GLOBALEVENTID",
          "EventTimeDate",
          "MentionTimeDate",
          "MentionType",
          "MentionSourceName",
          "MentionIdentifier",
          "SentenceID",
          "Actor1CharOffset",
          "Actor2CharOffset",
          "ActionCharOffset",
          "InRawText",
          "Confidence",
          "MentionDocLen",
          "MentionDocTone",
          "MentionDocTranslationInfo",
          "Extras"
        ),
      nameActual =
        c(
          "idGlobalEvent",
          "dateEvent",
          "dateMention",
          "idMentionType",
          "nameSource",
          "documentSource",
          "idSentence",
          "charLocActor1",
          "charLocActor2",
          "charLocAction",
          "isRawText",
          "scoreGoldsteinConfidence",
          "lengthMentionedDocument",
          "toneMentionedDocument",
          "translationMentionedDocument",
          "extra"
        )
    )

  return(mentions_schema)
}
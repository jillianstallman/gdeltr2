#' Retrieves GKG theme code book
#'
#' @return
#' @export
#' @import dplyr readr stringr purrr tidyr
#' @examples
#' get_codes_gkg_themes()

get_codes_gkg_themes <- function() {
  url <-
    'http://data.gdeltproject.org/api/v2/guides/LOOKUP-GKGTHEMES.TXT'

  code_df <-
    url %>%
    read_tsv(col_names = F) %>%
    setNames(c("codeGKGTheme", "countEvents"))

  code_df <-
    code_df %>%
    mutate(
      code2 = codeGKGTheme %>% str_to_lower(),
      isWBCode = ifelse(code2 %>% str_detect("wb_"), T, F),
      isEconomicEvent = ifelse(code2 %>% str_detect("econ_"), T, F),
      isSocialEvent = ifelse(code2 %>% str_detect("soc"), T, F),
      isTaxEvent = ifelse(code2 %>% str_detect("tax_"), T, F),
      isSocialEvent = ifelse(code2 %>% str_detect("soc_"), T, F),
      isMilitaryEvent = ifelse(code2 %>% str_detect("military|mil_"), T, F),
      isGovernmentEvent = ifelse(code2 %>% str_detect("gov_|government"), T, F),
      isMedicalEvent = ifelse(code2 %>% str_detect("med_|medical"), T, F),
      isAgressionAct = ifelse(code2 %>% str_detect("act_"), T, F),
      isMediaEvent = ifelse(code2 %>% str_detect("media_|_media"), T, F),
      isEmergencyEvent = ifelse(code2 %>% str_detect("emerg_"), T, F),
      isMovement = ifelse(code2 %>% str_detect("movement_"), T, F),
      isCriminalEvent = ifelse(code2 %>% str_detect("crime|crm_"), T, F)
    ) %>%
    dplyr::select(-code2)

  wb_codes <-
    code_df %>%
    dplyr::filter(isWBCode)

  wb_codes <-
    wb_codes %>%
    mutate(codeGKGTheme = codeGKGTheme %>% sub('\\_', '\\.', .)) %>%
    separate(
      codeGKGTheme,
      into = c('idDictionary', 'nameWBCode'),
      remove = F,
      sep = '\\.'
    ) %>%
    mutate(nameWBCode = nameWBCode %>% sub('\\_', '\\.', .)) %>%
    separate(
      nameWBCode,
      into = c('idWBCode', 'nameWBCode'),
      remove = T,
      sep = '\\.'
    ) %>%
    mutate(
      idWBCode = idWBCode %>% as.numeric,
      nameWBCode = nameWBCode %>% str_replace_all('\\_', ' ') %>% str_to_lower
    ) %>%
    dplyr::select(-idDictionary)

  non_wb <-
    code_df %>%
    dplyr::filter(!isWBCode)

  code_df <-
    non_wb %>%
    bind_rows(wb_codes)

  code_df <-
    code_df %>%
    mutate(codeGKGTheme = codeGKGTheme %>% str_replace_all('WB.', '\\WB_')) %>%
    dplyr::select(isWBCode, codeGKGTheme, idWBCode, nameWBCode, everything())

  code_df

}
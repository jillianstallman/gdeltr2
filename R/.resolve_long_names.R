.resolve_long_names <-
  function(data) {
    data <-
      data %>%
      mutate(
        numberItem = ifelse(
          item %>% str_detect("idADM"),
          item %>% str_replace_all("idADM1", '') %>% as.character() %>% parse_number(),
          item %>%
            as.character() %>%
            parse_number()
        ),
        numberItem = ifelse(numberItem %>% is.na(), 0 , numberItem),
        item = item %>% str_replace_all("^\\d+|\\d+$", '')
      ) %>%
      distinct() %>%
      suppressWarnings()

    data <-
      data %>%
      spread(item, value) %>%
      suppressWarnings()

    data <-
      data %>%
      mutate_at(data %>% select(
        dplyr::matches(
          "^score|^count|^amount|^value|^face|^angle|^latitude|^longitude|^day|^month|^year|^idTypeLocation"
        )
      ) %>% names,
      funs(. %>% as.character() %>% parse_number()))

    return(data)
  }
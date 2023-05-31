#' Loads needed packages
#'
#' @param required_packages vector of pckages to lad
#'
#' @return invisible
#' @export
#'
#' @examples
#' load_neeeded_packages(c('magrittr', 'dplyr))
#'
load_needed_packages <- function(required_packages = c('dplyr')) {
  loaded_packages <- gsub("package:", "", search())
  package_to_load <- required_packages[!required_packages %in%
                                         loaded_packages]
  if (length(package_to_load) > 0) {
    lapply(package_to_load, library, character.only = T)
  }
}

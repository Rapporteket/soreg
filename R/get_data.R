#' Get SoReg data
#'
#' Functions used to get data from SoReg database
#'
#' @param registry_name Character string providing the registry name
#'
#' @return A data frame of registry data
#' @name get_data
#' @aliases get_allevarnum


#' @rdname get_data
#' @export
get_allevarnum <- function(registry_name) {

  query <- "
SELECT
  *
FROM
  AlleVarNum;"

  rapbase::loadRegData(registry_name, query)
}

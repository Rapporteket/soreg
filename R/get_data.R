#' Get SoReg data
#'
#' Functions used to get data from SoReg database
#'
#' @param registry_name Character string providing the registry name
#' @param tabs Character vector where each element represents a database
#' table
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

#' @rdname get_data
#' @export
describe_db <- function(registry_name, tabs = c()) {

  query_tab <- "SHOW TABLES;"
  query_desc <- "DESCRIBE "

  desc <- list()

  if (length(tabs) == 0) {
    tabs <- rapbase::loadRegData(registryName = registry_name,
                                 query = query_tab)[[1]]
  }

  for (tab in tabs) {
    query <- paste0(query_desc, tab, ";")
    desc[[tab]] <- rapbase::loadRegData(registryName = registry_name,
                                        query)
  }

  desc
}

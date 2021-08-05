#' Provide dataframe of registry data
#'
#' Provides a dataframe containing data from a registry
#'
#' @param registryName String providing the current registryName
#' @param tableName String providing a table name
#' @param fromDate String providing start date
#' @param toDate String provideing end date
#' @param ... Optional arguments to be passed to the function
#' @name getRegData
#' @aliases getRegDataWhat getDataDump lagDDump
NULL

#' @rdname getRegData
#' @export
getRegDataWhat <- function(registryName) {

  # nocov start
  dbType <- "mysql"

  query <- "
SELECT
  AvdRESH AS Avdeling,
  COUNT(*) AS n
FROM
  AlleVarNum
GROUP BY
  AvdRESH;
"

  regData <- rapbase::LoadRegData(registryName, query, dbType)

  return(regData)
  # nocov end
  }

#' @rdname getRegData
#' @export
lagDDump <- function(registryName, tableName, fromDate, toDate, ...) {

  # dummy query returning empty data set
  query <- "SELECT * FROM AlleVar WHERE 1=0;"

  if (tableName %in% c("friendlynamestable", "change_log_variables",
                       "avdelingsoversikt", "Brukerliste")) {
    query <- paste0("
SELECT
  *
FROM
  ", tableName, ";
  ")
  }

  if (tableName %in% c("SkjemaOversikt", "AlleVar",
                       "AlleVarNum",
                       "Arskontrollar",
                       "Arskontrollar1", "Arskontrollar2",
                       "Arskontrollar5", "Arskontrollar10",
                       "Arskontrollar1Num", "Arskontrollar2Num",
                       "Arskontrollar5Num", "Arskontrollar10Num")) {
    query <- paste0("
SELECT
  fo.HovedDato,
  d.*
FROM
  ", tableName, " AS d
LEFT JOIN
  ForlopsOversikt fo
ON
  d.ForlopsID = fo.ForlopsID
WHERE
  fo.HovedDato BETWEEN
    CAST('", fromDate, "' AS DATE) AND
    CAST('", toDate, "' AS DATE);
")
  }

  if (tableName %in% c("ForlopsOversikt")) {
    query <- paste0("
SELECT
  *
FROM
  ", tableName, "
WHERE
  HovedDato BETWEEN
    CAST('", fromDate, "' AS DATE) AND
    CAST('", toDate, "' AS DATE);
")
  }

  if (tableName %in% c("","","")) {
    query <- paste0("
SELECT
  *
FROM
  ", tableName, "
WHERE
  HovedDato BETWEEN
    CAST('", fromDate, "' AS DATE) AND
    CAST('", toDate, "' AS DATE);
")
  }

  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]],
                       msg = paste("Soreg data dump:\n", query))
  }
  rapbase::loadRegData(registryName, query)
}

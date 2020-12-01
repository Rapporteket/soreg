#' Pick particular hospitals and years from a data frame
#'
#' @name snitt
#' @param df Data frame holding SoReg data
#' @param sh hospital(s)
#' @param yr year(s)
#'
#' @return A data frame for choice of hospitals and years
#'
#' @export
snitt <- function(df, sh, yr) {df %>%
  dplyr::filter(.data$OperererendeSykehus %in% .env$sh, .data$op_aar %in% .env$yr)}

#' slingringsmonn for aarskontrollar, minus
#'
#' +-90
#' @name nitti_m
#' @param yr years
#' @param dag operationday
#' @param l number of deviating controldays
#' @return beginning day of 'normtid'
#' @export
                                             #  slingringsmonn
nitti_m <- function(yr = 1, dag = lubridate::today(), l = 90) {
    dag - lubridate::ddays(l) + lubridate::years(yr)
}

#' slingringsmonn for aarskontrollar, plus
#'
#' @name nitti_p
#' @param yr years
#' @param dag operationday
#' @param l number of deviating controldays
#' @return end day of 'normtid'
#' @export

  nitti_p <- function(yr = 1, dag = lubridate::today(), l = 90) {
    dag + lubridate::ddays(l) + lubridate::years(yr)
  }

#' slingringsmonn for aarskontrollar, minus
#'
#' @name nitti
#' @param yr years
#' @param dag operationday
#' @param l number of deviating controldays
#' @return interval of 'normtid'
#' @export

nitti <- function(yr = 1, dag = lubridate::today(), l = 90) {
  c(dag - lubridate::ddays(l) + lubridate::years(yr),
    dag + lubridate::ddays(l) + lubridate::years(yr))
  }







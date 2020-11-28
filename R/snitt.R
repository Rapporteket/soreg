#' Pick particular hospitals and years from a data frame
#'
#' @param df Data frame holding SoReg data
#' @param OpererendeSykehus operating hospital
#' @param op_aar operasjonsår
#' @param sh hospital(s)
#' @param yr year(s)
#'
#' @return A data frame for choice of hospitals and years
#'
#' @export

snitt <- function(df, sh, yr) {df %>%
  dplyr::filter(OperererendeSykehus %in% sh, op_aar %in% yr)}

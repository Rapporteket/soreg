#' Pick particular hospitals and years from a data frame
#'
#' @param df Data frame holding SoReg data
#' @param sh hospital(s)
#' @param yr year(s)
#'
#' @return A data frame for choice of hospitals and years
#'
#' @export

snitt <- function(df, sh, yr) {df %>%
  dplyr::filter(.data$OperererendeSykehus %in% .env$sh, .data$op_aar %in% .env$yr)}

# OperererendeSykehus <- op_aar <- NULL



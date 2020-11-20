#' Calculate indicator
#'
#' @param df Data frame holding SoReg data
#' @param indicator Character string defining the indicator to be returned.
#' One of "liggetid", "kompl" or "dag30"
#'
#' @return A tibble providing the numerator (cases), denominator (sample size)
#' and the indicator
#' @export

ki <- function(df, indicator) {

  stopifnot(indicator %in% c("liggetid", "kompl", "dag30"))

  numerator <- switch (indicator,
    liggetid = sum(df$LiggeDogn <= 3),
    kompl = sum(df$`6U_KomplAlvorGrad` >= 4, na.rm=TRUE),
    dag30 = sum(df$`6U_Behandling30Dager` == 1)
  )

  denominator <- nrow(df)
  indicator <- numerator / denominator

  tibble::tibble(numerator, denominator, indicator)
}

#' Calculate indicator
#'
#' @param df Data frame holding SoReg data
#' @param indicator Character string defining the indicator to be returned.
#' One of "liggetid", "kompl", "dag30" or "TWL20"
#'
#' @return A tibble providing the numerator (cases), denominator (sample size)
#' and the indicator
#' @export

ki <- function(df, indicator) {

  stopifnot(indicator %in% c("liggetid", "kompl", "dag30", "K1", "K2", "TWL20"))

  numerator <- switch(indicator,
    liggetid = sum(df$LiggeDogn <= 3, na.rm = TRUE),
    kompl = sum(df$u6_KomplAlvorGrad >= 4, na.rm = TRUE),
    dag30 = sum(df$u6_Behandling30Dager == 1, na.rm = TRUE),
    K1 = sum(df$et_nt, na.rm = TRUE),
    K2 = sum(df$to_nt, na.rm = TRUE),
    TWL20 = sum(df$del20, na.rm = TRUE)
  )

  denominator <- nrow(df)
  indicator <- round(100 * numerator / denominator, 1)

  tibble::tibble(numerator, denominator, indicator)
}

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

#' slingringsmonn for aarskontrollar, minus
#'
#' +-90
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
#' @param yr years
#' @param dag operationday
#' @param l number of deviating controldays
#' @return end day of 'normtid'
#' @export

  nitti_p <- function(yr = 1, dag = lubridate::today(), l = 90) {
    dag + lubridate::ddays(l) + lubridate::years(yr)
  }

#' slingringsmonn for aarskontrollar, interval
#' @param yr years
#' @param dag operationday
#' @param l number of deviating controldays
#' @return interval of 'normtid'
#' @export

  nitti <- function(yr = 1, dag = lubridate::today(), l = 90) {
    c(dag - lubridate::ddays(l) + lubridate::years(yr),
      dag + lubridate::ddays(l) + lubridate::years(yr))
  }

#' lage aarskontrolltabell
#' @param df data frame
#' @param k which year control
#' @return df data frame grouped by year and hospital
#' @export

aar_ktr_tb <- function(df, k = 2){

last_opday <- max(df$Operasjonsdato)

   switch(k,
           "1" = {last_op =  last_opday - months(15) #?lubridate::month
                     nt = .data$et_nt},
           "2" = {last_op =  last_opday - months(27)
                     nt = .data$to_nt}
)

df <- df %>%
dplyr::mutate(et_nor_m = nitti_m(dag = .data$Operasjonsdato),
		 	        et_nor_p = nitti_p(dag = .data$Operasjonsdato),
              to_nor_m = nitti_m(yr = 2, dag = .data$Operasjonsdato),
			        to_nor_p = nitti_p(yr = 2, dag = .data$Operasjonsdato),
                  pTWL = 100*(.data$BR_Vekt - .data$`ToAar_Vekt`)/.data$BR_Vekt )

df <- df %>% dplyr::mutate(
 et_nt = .data$EttAar_Oppfolgingsdato %within% lubridate::interval( .data$et_nor_m, .data$et_nor_p ),
 to_nt = .data$ToAar_Oppfolgingsdato %within% lubridate::interval( .data$to_nor_m, .data$to_nor_p ),
 et_b4 = .data$EttAar_Oppfolgingsdato %within%
         lubridate::interval( .data$Operasjonsdato+1, .data$et_nor_m-1 ),
 et_lt = .data$EttAar_Oppfolgingsdato > .data$et_nor_p,
 to_b4 = .data$ToAar_Oppfolgingsdato %within%
         lubridate::interval( .data$Operasjonsdato+1, .data$to_nor_m-1 ),
 to_lt = .data$ToAar_Oppfolgingsdato > .data$to_nor_p)

df <- df %>% dplyr::select(
 c("PasientID", "OperererendeSykehus", "Operasjonsdato", "op_aar",
 "Operasjonsmetode", "Opmetode_GBP", "et_b4", "et_nt", "et_lt",
 "to_b4",  "to_nt", "to_lt", "pTWL"))


df %>%
 dplyr::filter(.data$Operasjonsdato < last_op) %>%
 dplyr::group_by(.data$OperererendeSykehus, .data$op_aar) %>%
 dplyr::mutate(ops = dplyr::n()) %>%
 dplyr::summarise(ktrl = sum(nt, na.rm = T), oprs = .data$ops[1],
                   ktl = sum(nt, na.rm = T)/ .data$ops[1])

}



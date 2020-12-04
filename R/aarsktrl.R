#' Pick particular hospitals and years from a data frame
#'
#' @param df Data frame holding full SoReg data
#' @importFrom lubridate %within%
#' @return k1nt A data frame for choice of hospitals and years
#' @return k2nt A data frame for choice of hospitals and years
#'
#' @export

aars_ktrl <- function(df){
df <- df %>%  dplyr::mutate(et_nor_m = nitti_m(dag = .data$Operasjonsdato),
                           et_nor_p = nitti_p(dag = .data$Operasjonsdato),
                           to_nor_m = nitti_m(yr = 2, dag = .data$Operasjonsdato),
			   to_nor_p = nitti_p(yr = 2, dag = .data$Operasjonsdato),
                           pTWL = 100*(.data$BR_Vekt - .data$`ToAar_Vekt`)/.data$BR_Vekt )
df <- df %>% dplyr::mutate(
 et_nt = .data$EttAar_Oppfolgingsdato %within%
         lubridate::interval( .data$et_nor_m, .data$et_nor_p ),
 to_nt = .data$ToAar_Oppfolgingsdato %within%
         lubridate::interval( .data$to_nor_m, .data$to_nor_p ),
 et_b4 = .data$EttAar_Oppfolgingsdato %within%
         lubridate::interval( .data$Operasjonsdato+1, .data$et_nor_m-1 ),
 et_lt = .data$EttAar_Oppfolgingsdato > .data$et_nor_p,
 to_b4 = .data$ToAar_Oppfolgingsdato %within%
         lubridate::interval( .data$Operasjonsdato+1, .data$to_nor_m-1 ),
 to_lt = .data$ToAar_Oppfolgingsdato > .data$to_nor_p) %>% dplyr::select(
 c("PasientID", "OperererendeSykehus", "Operasjonsdato", "op_aar",
 "Operasjonsmetode", "Opmetode_GBP", "et_b4", "et_nt", "et_lt",
 "to_b4",  "to_nt", "to_lt", "pTWL"))

last_opday <- max(df$Operasjonsdato)
k1_last <- last_opday -  months(15) # slingringsmonn
k2_last <- last_opday -  months(27) #

k1nt  <-  df %>%
 dplyr::filter(.data$Operasjonsdato < k1_last) %>%
 dplyr::group_by(.data$OperererendeSykehus, .data$op_aar) %>% dplyr::mutate(ops = dplyr::n()) %>%
 dplyr::summarise(ktr1 = sum(.data$et_nt, na.rm = T), oprs = .data$ops[1],
           kt1 = sum(.data$et_nt, na.rm = T)/ .data$ops[1])

k2nt  <-  df %>%
 dplyr::filter(.data$Operasjonsdato < k2_last) %>%
 dplyr::group_by(.data$OperererendeSykehus, .data$op_aar) %>% dplyr::mutate(ops = dplyr::n()) %>%
 dplyr::summarise(ktr2 = sum(.data$to_nt, na.rm = T),oprs = .data$ops[1],
           kt2 = sum(.data$to_nt, na.rm = T)/ .data$ops[1])
}
#--------------------------------------------------------------------------- 80
#' Pick particular hospitals and years from a data frame
#'
#' @param sh hospital
#' @param yr year(s)
#'
#' @return k1 A data frame for choice of hospitals and years
#'
#' @export

k1 <- function(sh, yr) {.data$k1nt %>%
dplyr::filter(.data$OperererendeSykehus %in% .data$sh, .data$op_aar %in% .data$yr)}

#' Pick particular hospitals and years from a data frame
#'
#' @param sh hospital
#' @param yr year(s)
#'
#' @return k2 A data frame for choice of hospitals and years
#'
#' @export

k2 <- function(sh, yr) {.data$k2nt %>%
dplyr::filter(.data$OperererendeSykehus %in% .data$sh, .data$op_aar %in% .data$yr)}

#--------------------------------------------------------------------------- 80



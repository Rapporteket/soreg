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
  
#' slingringsmonn for aarskontrollar
#' ###################################################
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
  
#' @param yr years
#' @param dag operationday
#' @param l number of deviating controldays
#' @return end day of 'normtid'
#' @export
  
  nitti_p <- function(yr = 1, dag = lubridate::today(), l = 90) {
    dag + lubridate::ddays(l) + lubridate::years(yr)
  }

#' @param yr years
#' @param dag operationday
#' @param l number of deviating controldays
#' @return interval of 'normtid'
#' @export

  nitti <- function(yr = 1, dag = lubridate::today(), l = 90) {
    c(dag - lubridate::ddays(l) + years(yr),
      dag + lubridate::ddays(l) + lubridate::years(yr))
  }
  
#' @param df data frame
#' @param k which year control
#' @return df data frame grouped by year and hospital
#' @export
  
aar_ktr_tb <- function(df, k = 2){
 
last_opday <- max(df$Operasjonsdato)
   switch{k, 
           1 = {last_op =  last_opday - lubridate::months(15)
                     nt = et_nt},
           2 = {last_op =  last_opday - lubridate::months(27)
                     nt = to_nt}
}

df <- df %>%  
dplyr::mutate(et_nor_m = nitti_m(dag = .data$Operasjonsdato),                  					   
		 	  et_nor_p = nitti_p(dag = .data$Operasjonsdato),
              to_nor_m = nitti_m(yr = 2, dag = .data$Operasjonsdato), 
			  to_nor_p = nitti_p(yr = 2, dag = .data$Operasjonsdato),
                  pTWL = 100*(.data$BR_Vekt - .data$`ToAar_Vekt`)/.data$BR_Vekt )

df <- df %>% dplyr::mutate(
 et_nt = .data$EttAar_Oppfolgingsdato %within% interval( et_nor_m, et_nor_p ),
 to_nt = .data$ToAar_Oppfolgingsdato %within% interval( to_nor_m, to_nor_p ),
 et_b4 = .data$EttAar_Oppfolgingsdato %within% 
         interval( Operasjonsdato+1, et_nor_m-1 ),
 et_lt = .data$EttAar_Oppfolgingsdato > et_nor_p,
 to_b4 = .data$ToAar_Oppfolgingsdato %within% 
         interval( Operasjonsdato+1, to_nor_m-1 ),
 to_lt = .data$ToAar_Oppfolgingsdato > to_nor_p) %>% dplyr::select(  
 c("PasientID", "OperererendeSykehus", "Operasjonsdato", "op_aar", 
 "Operasjonsmetode", "Opmetode_GBP", "et_b4", "et_nt", "et_lt",
 "to_b4",  "to_nt", "to_lt", "pTWL"))


df %>% 
 dplyr::filter(.data$Operasjonsdato < last_op) %>% 
 dplyr::group_by(.data$OperererendeSykehus, op_aar) %>%
 dplyr::mutate(ops = n()) %>% 
 dplyr::summarise(ktrl = sum(nt, na.rm = T), oprs = ops[1],
                   kt1 = sum(nt, na.rm = T)/ ops[1]) 	
 	
}

###
# # switch  only 15/27  and et_nt/to_nt ?

#####
last_opday <- max(dt$Operasjonsdato)
k1_last <- last_opday - lubridate::months(15) # slingringsmonn
k2_last <- last_opday - lubridate::months(27)

k1nt  <-  df %>% 
 dplyr::filter(Operasjonsdato < k1_last) %>% 
 dplyr::group_by(OperererendeSykehus,op_aar) %>% dplyr::mutate(ops = n()) %>% 
 dplyr::summarise(ktr1 = sum(et_nt, na.rm = T), oprs = ops[1],
           kt1 = sum(et_nt, na.rm = T)/ ops[1]) 

k2nt  <-  df %>% 
 dplyr::filter(Operasjonsdato < k2_last) %>% 
 dplyr::group_by(OperererendeSykehus,op_aar) %>% dplyr::mutate(ops = n()) %>% 
 dplyr::summarise(ktr2 = sum(to_nt, na.rm = T),oprs = ops[1],
		    kt2 = sum(to_nt, na.rm = T)/ ops[1]) 



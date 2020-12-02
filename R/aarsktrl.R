#' Pick particular hospitals and years from a data frame
#'
#' @names aars_ktrl function for ca
#' @param df Data frame holding full SoReg data
#'
#' @return k1nt A data frame for choice of hospitals and years
#' @return k2nt A data frame for choice of hospitals and years
#'
#' @export

aars_ktrl <- function(df){
df <- df %>%  dplyr::mutate(et_nor_m = nitti_m(dag = Operasjonsdato), 
                           et_nor_p = nitti_p(dag = Operasjonsdato),
                           to_nor_m = nitti_m(yr = 2, dag = Operasjonsdato), 
						   to_nor_p = nitti_p(yr = 2, dag = Operasjonsdato),
                           pTWL = 100*(BR_Vekt - `ToAar_Vekt`)/BR_Vekt )
df <- df %>% dplyr::mutate(
 et_nt = EttAar_Oppfolgingsdato %within% interval( et_nor_m, et_nor_p ),
 to_nt = ToAar_Oppfolgingsdato %within% interval( to_nor_m, to_nor_p ),
 et_b4 = EttAar_Oppfolgingsdato %within% 
         interval( Operasjonsdato+1, et_nor_m-1 ),
 et_lt = EttAar_Oppfolgingsdato > et_nor_p,
 to_b4 = ToAar_Oppfolgingsdato %within% 
         interval( Operasjonsdato+1, to_nor_m-1 ),
 to_lt = ToAar_Oppfolgingsdato > to_nor_p) %>% dplyr::select(  
 c("PasientID", "OperererendeSykehus", "Operasjonsdato", "op_aar", 
 "Operasjonsmetode", "Opmetode_GBP", "et_b4", "et_nt", "et_lt",
 "to_b4",  "to_nt", "to_lt", "pTWL"))

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
}					   
#--------------------------------------------------------------------------- 80
#' Pick particular hospitals and years from a data frame
#'
#' @names k1 function for ca
#' @param sh hospital
#' @param yr year(s)
#'
#' @return k1 A data frame for choice of hospitals and years
#'
#' @export

k1 <- function(sh, yr) {k1nt %>% 
        filter(OperererendeSykehus %in% sh, op_aar %in% yr)}
		
#' Pick particular hospitals and years from a data frame
#'
#' @names k2 function for ca
#' @param sh hospital
#' @param yr year(s)
#'
#' @return k2 A data frame for choice of hospitals and years
#'
#' @export
		
k2 <- function(sh, yr) {k2nt %>% 
        filter(OperererendeSykehus %in% sh, op_aar %in% yr)}
}
#--------------------------------------------------------------------------- 80


 
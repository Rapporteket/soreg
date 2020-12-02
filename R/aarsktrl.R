#' Pick particular hospitals and years from a data frame
#'
#' @names aars_ktrl function for ca
#' @param df Data frame holding full SoReg data
#'
#' @return k1 A data frame for choice of hospitals and years
#' @return k2 A data frame for choice of hospitals and years
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

k1nt  <-  dt %>% 
filter(Operasjonsdato < k1_last) %>% 
group_by(OperererendeSykehus,op_aar) %>% mutate(ops = n()) %>% 
summarise(ktr1 = sum(et_nt, na.rm = T), oprs = ops[1],
           kt1 = sum(et_nt, na.rm = T)/ ops[1]) 

k2nt  <-  dt %>% 
 filter(Operasjonsdato < k2_last) %>% 
 group_by(OperererendeSykehus,op_aar) %>% mutate(ops = n()) %>% 
 summarise(ktr2 = sum(to_nt, na.rm = T),oprs = ops[1],
		    kt2 = sum(to_nt, na.rm = T)/ ops[1]) 
						   
#--------------------------------------------------------------------------- 80
k1 <- function(sh, yr) {k1nt %>% 
        filter(OperererendeSykehus %in% sh, op_aar %in% yr)}
k2 <- function(sh, yr) {k2nt %>% 
        filter(OperererendeSykehus %in% sh, op_aar %in% yr)}
}
#--------------------------------------------------------------------------- 80


# dmx <- d_full   # alle operasjoner for kontrollene
# dm  <- dmx   %>%  mutate( et_nor_m = nitti_m(dag= Operasjonsdato), et_nor_p = nitti_p(dag= Operasjonsdato),
                          # to_nor_m = nitti_m(yr=2, dag= Operasjonsdato), to_nor_p = nitti_p(yr=2, dag= Operasjonsdato),
                          # pTWL = 100*(BR_Vekt - `ToAar_Vekt`)/BR_Vekt )

# dn <-  dm %>% mutate(et_nt= EttAar_Oppfolgingsdato %within% interval( et_nor_m, et_nor_p ),
                     # to_nt= ToAar_Oppfolgingsdato %within% interval( to_nor_m, to_nor_p ),
                     # et_b4= EttAar_Oppfolgingsdato %within% interval( Operasjonsdato+1, et_nor_m-1 ),
                     # et_lt= EttAar_Oppfolgingsdato > et_nor_p,
                     # to_b4= ToAar_Oppfolgingsdato %within% interval( Operasjonsdato+1, to_nor_m-1 ),
                     # to_lt= ToAar_Oppfolgingsdato > to_nor_p)
#                     fs1_gj_kt = `1Aar_OppfolgingsType` == 4 ,
#                     fs1_u_kt  = `1Aar_OppfolgingsType` == 5 ,
#                     fs2_gj_kt = `2Aar_OppfolgingsType` == 4 , 
#                     fs2_u_kt  =  `2Aar_OppfolgingsType` == 5  )

# dt    <-  dn %>% select(  c("PasientID", "OperererendeSykehus", "Operasjonsdato", "op_aar", "Operasjonsmetode", "Opmetode_GBP",
                           # "et_b4", "et_nt", "et_lt", "to_b4",  "to_nt", "to_lt", "pTWL"))
#                           "fs1_gj_kt", "fs1_u_kt", "fs2_gj_kt", "fs2_u_kt", "pTWL"))

# fedmerelatert
# du    <-  dm %>% select("BR_SovnApne" ,"BR_Hypertoni" ,"BR_Diabetes","BR_Dyslipidemi",
                        # "BR_Dyspepsi" ,"BR_Depresjon","BR_MuskelSkjelettsmerter")
# fr <-  colSums(du,na.rm = TRUE) /nrow(du)
# fr <- sort(fr,decr=T)
# last_opday <- max(dt$Operasjonsdato)
# k1_last <- last_opday - months(15)
# k2_last <-  last_opday - months(27)

# k1_dt <- dt %>% filter(Operasjonsdato < k1_last) %>% group_by(OperererendeSykehus) %>% mutate(ops = n())
# k2_dt <- dt %>% filter(Operasjonsdato < k2_last) %>% group_by(OperererendeSykehus) %>% mutate(ops = n())

# k1nt  <-  dt %>% filter(Operasjonsdato < k1_last) %>% 
                 # group_by(OperererendeSykehus,op_aar) %>% 
                 # mutate(ops = n()) %>% 
                 # summarise(ktr1 = sum(et_nt, na.rm = T),oprs = ops[1],kt1 = sum(et_nt, na.rm = T)/ ops[1]) 

# k2nt  <-  dt %>% filter(Operasjonsdato < k2_last) %>% 
                 # group_by(OperererendeSykehus,op_aar) %>% 
                 # mutate(ops = n()) %>% 
                 # summarise(ktr2 = sum(to_nt, na.rm = T),oprs = ops[1],kt2 = sum(to_nt, na.rm = T)/ ops[1]) 

# k1_sh <- function(sh) {k1nt %>% filter(OperererendeSykehus %in% sh)}
# k1_yr <- function(yr) {k1nt %>% filter(op_aar %in% yr)}
# k2_sh <- function(sh) {k2nt %>% filter(OperererendeSykehus %in% sh)}
# k2_yr <- function(yr) {k2nt %>% filter(op_aar %in% yr)}

# k1 <- function(sh,yr) {k1nt %>% filter(OperererendeSykehus %in% sh, op_aar %in% yr)}
# k2 <- function(sh,yr) {k2nt %>% filter(OperererendeSykehus %in% sh, op_aar %in% yr)}

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

#' lage ligged?gntabell
#' @param df data frame
#' @return df data frame grouped by year and hospital
#' @export

lgg_tb <- function(df) { df %>%
 dplyr::group_by(.data$OperererendeSykehus, .data$op_aar) %>%
 soreg::ki(across(), "liggetid") %>%          # sjekk ki(.,)
 dplyr::arrange(dplyr::desc(indicator)) %>% dplyr::ungroup()  # ungroup er bra?
}

  # # KI1 LIGGEDØGN  ---#----- Kor mange låg mindre enn fire døgn per sjukehus
  # d_kortligg_sjuk <- d_prim_6v %>%
  #   dplyr::group_by(OperererendeSykehus, op_aar) %>%
  #   dplyr::do(soreg::ki_liggetid(.)) %>%
  #   dplyr::arrange(desc(ind)) %>% dplyr::ungroup()
  # #----------------------------------------------------------------------------80
  # kortligg    <- function(sh, yr){
  #   d_kortligg_sjuk %>%
  #     dplyr::filter(OperererendeSykehus %in% sh, op_aar %in% yr)}
  # # kortligg <- snitt(d_kortligg_sjuk, sh, yr)


#' lage reinnleggningtabell
#' @param df data frame
#' @return df data frame grouped by year and hospital
#' @export

reinn_tb <- function(df)  {df <- df %>%
  dplyr::filter(((.data$`6U_KontrollType` %in% 1:3) |
                   (.data$`6U_Behandling30Dager` == 1)) &
                  (.data$`6U_Behandling30Dager` != 2))
df %>%   dplyr::group_by(.data$OperererendeSykehus, .data$op_aar) %>%
  soreg::ki(dplyr::across(), "dag30") %>%
  dplyr::arrange(dplyr::desc(indicator))
}

# KI2 REINNLEGGELSE
# I analysar for reinnlegging ser me berre på dei som har 6-vekesoppfølging
# registrert eller som er registrert som reinnlagd til trass for at dei
# ikkje har 6-vekesoppfølging (jf. forklaringstekst for reinnleggings-
# indikatoren i årsrapporten). Hentar ut eiga datasett for desse. Men det
# viser seg at det òg er mogleg å svara «Vet ikke» (verdi 2) på om pasienten
# vart reinnlagd. Desse gjev ingen informasjon, og vert derfor òg fjerna.
# d_reinn <- d_prim %>% dplyr::filter(((`6U_KontrollType` %in% 1:3) |
#                                        (`6U_Behandling30Dager` == 1)) &
#                                       (`6U_Behandling30Dager` != 2))
#
#----------------------------------------------------------------------------80
# andel pasienter som får innleggelse innen 30 dager etter operasjon
# per sjukehus
# d_innlegg30 <- d_reinn %>%
#   dplyr::group_by(OperererendeSykehus, op_aar) %>%
#   dplyr::do(soreg::ki_30dager(.)) %>%
#   dplyr::arrange(desc(ind))
#
# innl30 <- function(sh,yr){d_innlegg30 %>%
#     dplyr::filter(OperererendeSykehus %in% sh, op_aar %in% yr)}
# innl30 <- snitt(d_innlegg30, sh, yr)


#' lage komplikasjontabell
#' @param df data frame
#' @return df data frame grouped by year and hospital
#' @export

kompl_tb <- function(df) {df <- df %>%
 dplyr::filter((.data$`6U_KontrollType` %in% 1:3) |
                 (!is.na(.data$`6U_KomplAlvorGrad`)))
df  %>%
 dplyr::group_by(.data$OperererendeSykehus, .data$op_aar) %>%
 soreg::ki(dplyr::across(), "kompl")) %>%
 dplyr::arrange(dplyr::desc(indicator))
}


# # KI3 KOMPLIKASJONAR
# # Tilsvarande for alvorlege komplikasjonar
# d_kompl <- d_prim %>%
#   dplyr::filter((`6U_KontrollType` %in% 1:3) | (!is.na(`6U_KomplAlvorGrad`)))
#
# # andel pasienter som får en alvorlig komplikasjon per sjukehus
# d_kompl_alv_sjukehus <- d_kompl %>%
#   dplyr::group_by(OperererendeSykehus, op_aar) %>%
#   dplyr::do(soreg::ki_kompl_alv(.)) %>%
#   dplyr::arrange(desc(ind))
#
# kompl <- function(sh, yr){d_kompl_alv_sjukehus %>%
#     dplyr::filter(OperererendeSykehus %in% sh, op_aar %in% yr)}
# # kompl <- snitt(d_kompl_alv_sjukehus, sh, yr)


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
#----------------------------------------------------------------------------80
#' lage vekttaptabell
#' @param df data frame
#' @return df data frame grouped by year and hospital
#' @export

TWL_tb <- function(df){  # d_full
 d_TWL  <- df %>%
  dplyr::filter(!is.na(.data$`ToAar_Vekt`)) %>%
  dplyr::mutate(
    pTWL = 100*(.data$BR_Vekt - .data$`ToAar_Vekt`)/.data$BR_Vekt ) %>%
  dplyr::mutate(del20 = .data$pTWL >= 20.0)
 # pTWL at 2 year must exist!
 d_slv  <- d_TWL %>% dplyr::filter(.data$Operasjonsmetode == 6)
 d_gbp  <- d_TWL %>% dplyr::filter(.data$Operasjonsmetode == 1,
                                   .data$Opmetode_GBP == 1)
 d_oa   <- d_TWL %>% dplyr::filter(.data$Operasjonsmetode == 1,
                                   .data$Opmetode_GBP == 2)
 slv20   <-    detail(d_slv)
 gbp20   <-    detail(d_gbp)
 oa20   <-    detail(d_oa)
 slv20  # output only sleeve first
}

#' lage vekttapdetaljer
#' @param dm data frame
#' @return df data frame grouped by year and hospital
#' @export

detail <- function(dm){ dm %>%
 dplyr::group_by(.data$OperererendeSykehus, .data$op_aar) %>%
 dplyr::summarise("tyve"= sum(.data$del20, na.rm = TRUE),  "ops" = dplyr::n(),
                  "minst20" = mean(.data$del20, na.rm = TRUE))
}

# d_WL <- d_full %>%  dplyr::filter(!is.na(d_full$`ToAar_Vekt`)) %>%
#   dplyr::mutate(pTWL = 100*(d_WL$BR_Vekt - d_WL$`ToAar_Vekt`)/d_WL$BR_Vekt )
# d_TWL  <- d_WL %>% dplyr::mutate(del20 = d_WL$pTWL >= 20.0)
#
# d_slv  <- d_TWL %>% dplyr::filter(d_TWL$Operasjonsmetode == 6)
# d_gbp  <- d_TWL %>% dplyr::filter(d_TWL$Operasjonsmetode == 1,
#                                   d_TWL$Opmetode_GBP == 1)
# d_oa   <- d_TWL %>% dplyr::filter(d_TWL$Operasjonsmetode == 1,
#                                   d_TWL$Opmetode_GBP == 2)



# detail    <-  function(dmx) {dmx %>%
#     dplyr::group_by(.data$OperererendeSykehus, .data$op_aar)}
# # shw       <-  function(dmx) {dmx %>% .env$tbl_df %>% rmarkdown::paged_table()}
#
# slv20   <-    detail(d_slv) %>%
#   dplyr::summarise("tyve"= sum(.env$del20, na.rm = TRUE),  "ops" = dplyr::n(),
#                    "minst20" = mean(.env$del20, na.rm = TRUE))
# gbp20   <-    detail(d_gbp) %>%
#   dplyr::summarise("tyve"= sum(.env$del20, na.rm = TRUE),  "ops" = dplyr::n(),
#                    "minst20" = mean(.env$del20, na.rm = TRUE))
# oa20   <-    detail(d_oa)  %>%
#   dplyr::summarise("tyve"= sum(.env$del20, na.rm = TRUE),  "ops" = dplyr::n(),
#                    "minst20" = mean(.env$del20, na.rm = TRUE))

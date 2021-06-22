#' Pick particular hospitals and years from a data frame
#'
#' @param df Data frame holding SoReg data
#' @param sh hospital(s)
#' @param yr year(s)
#'
#' @return A data frame for choice of hospitals and years
#'
#' @export

snitt <- function(df, sh, yr) {
  df %>%
  dplyr::filter(.data$OperererendeSykehus %in% .env$sh,
                .data$op_aar %in% .env$yr)
  }

#' Pick particular hospitals and years from a data frame
#'
#' @param df Data frame holding SoReg data
#' @param sh hospital(s)
#' @param yr year(s)
#' @param prm primary operation
#' @param opr operation type
#'
#' @return A data frame for choice of hospitals, years and opr.type
#'
#' @export

slice <- function(df, sh, yr, prm, opr) {
  df %>%
    dplyr::filter(.data$OperererendeSykehus %in% .env$sh,
                  .data$op_aar %in% .env$yr,
                  .data$op_primar %in% .env$prm,
                  .data$Operasjonsmetode %in% .env$opr )
}



#' slingringsmonn for aarskontrollar, minus
#'
#' +-90
#' @param yr years
#' @param dag operationday
#' @param l number of deviating controldays
#' @return beginning day of 'normtid'
#' @export

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

lgg_tb <- function(df) {
  df %>%
 dplyr::group_by(.data$OperererendeSykehus, .data$op_aar) %>%
 dplyr::summarise(soreg::ki(dplyr::across(), "liggetid")) %>%
 dplyr::arrange(dplyr::desc(.data$indicator)) %>%
 dplyr::ungroup()
}

#' lage liggedogngraf
#' @param df data frame
#' @return df data frame grouped by year and hospital
#' @export

lgg_gr <- function(df){
  maksdogn_vis = 14
  LiggeDogn <- liggedogn_trunk <- liggedogn_lenge <- NULL
  # r-bloggers.com/2019/08/no-visible-binding-for-global-variable
  df %<>%
    dplyr::mutate(
      liggedogn_lenge = LiggeDogn > maksdogn_vis,
      liggedogn_trunk = pmin(LiggeDogn, maksdogn_vis + 1))

      # n_liggedogn_lenge = sum(df$LiggeDogn_lenge, na.rm = TRUE)
      liggedogn_maks = max(df$LiggeDogn, na.rm = TRUE)

      liggedogn_breaks = seq(
        pmin(1, min(df$LiggeDogn, na.rm = TRUE)), maksdogn_vis + 1)
      liggedogn_tekst = liggedogn_breaks
      liggedogn_tekst[length(liggedogn_tekst)] = paste0("\u2265", maksdogn_vis + 1 )


   df %>% dplyr::filter(!is.na(LiggeDogn) & !(LiggeDogn<0)) %>%
    ggplot2::ggplot( ggplot2::aes(x = liggedogn_trunk, fill = liggedogn_lenge)) +
     ggplot2::geom_bar(stat="count", show.legend = FALSE)+
     ggplot2::scale_fill_manual(values = c("FALSE"= "blue","TRUE"= "red"))+
     ggplot2::scale_x_continuous(breaks = liggedogn_breaks, labels = liggedogn_tekst, expand = c(0, .6))+
     ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.0, .05), add = 0) )+
     ggplot2::xlab("Liggedogn") + ggplot2::ylab("Talet paa pasienter")
}


#' lage reinnleggningtabell
#' @param df data frame
#' @return df data frame grouped by year and hospital
#' @export

reinn_tb <- function(df)  {
  df <- df %>%
  dplyr::filter(((.data$`6U_KontrollType` %in% 1:3) |
                   (.data$`6U_Behandling30Dager` == 1)) &
                  (.data$`6U_Behandling30Dager` != 2))
df %>%
  dplyr::group_by(.data$OperererendeSykehus, .data$op_aar) %>%
  dplyr::summarise(soreg::ki(dplyr::across(), "dag30")) %>%
  dplyr::arrange(dplyr::desc(.data$indicator))
}

#' lage komplikasjontabell
#' @param df data frame
#' @return df data frame grouped by year and hospital
#' @export

kompl_tb <- function(df) {
  df <- df %>%
 dplyr::filter((.data$`6U_KontrollType` %in% 1:3) |
                 (!is.na(.data$`6U_KomplAlvorGrad`)))
df  %>%
 dplyr::group_by(.data$OperererendeSykehus, .data$op_aar) %>%
  dplyr::summarise(soreg::ki(dplyr::across(), "kompl")) %>%
 dplyr::arrange(dplyr::desc(.data$indicator))
}

#' lage aarskontrolltabell
#' @param df data frame
#' @param k which year control
#' @return df data frame grouped by year and hospital
#' @export

aar_ktr_tb <- function(df, k = 2) {

last_opday <- max(df$Operasjonsdato)

   switch(k,
           "1" <- {
             last_op <-  last_opday - months(15)
                  nt <- .data$et_nt},
           "2" <- {
             last_op <-  last_opday - months(27)
                  nt <- .data$to_nt
                  }
)

df <- df %>%
dplyr::mutate(et_nor_m = nitti_m(dag = .data$Operasjonsdato),
et_nor_p = nitti_p(dag = .data$Operasjonsdato),
to_nor_m = nitti_m(yr = 2, dag = .data$Operasjonsdato),
to_nor_p = nitti_p(yr = 2, dag = .data$Operasjonsdato),
pTWL = 100 * (.data$BR_Vekt - .data$`ToAar_Vekt`) / .data$BR_Vekt)

df <- df %>%
  dplyr::mutate(
 et_nt <- .data$EttAar_Oppfolgingsdato %within%
   lubridate::interval(.data$et_nor_m, .data$et_nor_p),
 to_nt <- .data$ToAar_Oppfolgingsdato %within%
   lubridate::interval(.data$to_nor_m, .data$to_nor_p),
 et_b4 <- .data$EttAar_Oppfolgingsdato %within%
   lubridate::interval(.data$Operasjonsdato + 1, .data$et_nor_m - 1),
 et_lt <- .data$EttAar_Oppfolgingsdato > .data$et_nor_p,
 to_b4 <- .data$ToAar_Oppfolgingsdato %within%
         lubridate::interval(.data$Operasjonsdato + 1, .data$to_nor_m - 1),
 to_lt <- .data$ToAar_Oppfolgingsdato > .data$to_nor_p)

df <- df %>% dplyr::select(
 c("PasientID", "OperererendeSykehus", "Operasjonsdato", "op_aar",
 "Operasjonsmetode", "Opmetode_GBP", "et_b4", "et_nt", "et_lt",
 "to_b4",  "to_nt", "to_lt", "pTWL"))


df %>%
 dplyr::filter(.data$Operasjonsdato < last_op) %>%
 dplyr::group_by(.data$OperererendeSykehus, .data$op_aar) %>%
 dplyr::mutate(ops = dplyr::n()) %>%
 dplyr::summarise(ktrl = sum(nt, na.rm = T), oprs = .data$ops[1],
                   ktl = sum(nt, na.rm = T) / .data$ops[1])

}
#----------------------------------------------------------------------------80
#' lage vekttaptabell
#' @param df data frame full data
#' @param opr_tp operasjonstype sleeve, bypass, oagb
#' @return df data frame grouped by year and hospital
#' @export

TWL_tb <- function(df, opr_tp) {
 d_TWL  <- df %>%
  dplyr::filter(!is.na(.data$`ToAar_Vekt`)) %>%
  dplyr::mutate(
    pTWL = 100 * (.data$BR_Vekt - .data$`ToAar_Vekt`) / .data$BR_Vekt) %>%
  dplyr::mutate(del20 = .data$pTWL >= 20.0)
 # pTWL at 2 year must exist!
 d_slv  <- d_TWL %>% dplyr::filter(.data$Operasjonsmetode == 6)
 d_gbp  <- d_TWL %>% dplyr::filter(.data$Operasjonsmetode == 1,
                                   .data$Opmetode_GBP == 1)
 d_oa   <- d_TWL %>% dplyr::filter(.data$Operasjonsmetode == 1,
                                   .data$Opmetode_GBP == 2)
 switch(opr_tp,
 "slv"  = {
   detail(d_slv)},
 "gbp"  = {
   detail(d_gbp)},
 "oa"   = {
   detail(d_oa)})
}

#' lage vekttapdetaljer
#' @param dm data frame
#' @return df data frame grouped by year and hospital
#' @export

detail <- function(dm) {
  dm %>%
 dplyr::group_by(.data$OperererendeSykehus, .data$op_aar) %>%
 dplyr::summarise("tyve" = sum(.data$del20, na.rm = TRUE),
                  "ops" = dplyr::n(),
                  "minst20" = mean(.data$del20, na.rm = TRUE))
}

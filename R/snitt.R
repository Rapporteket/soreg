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
                  .data$Operasjonsmetode %in% .env$opr)
              #    .data$Opmetode_GBP %in% .env$oa)
}

#' Pick particular hospitals and years from a data frame
#'
#' @param df Data frame holding SoReg data
#' @param sh hospital(s)
#' @param yr year(s)
#' @param prm primary operation
#' @param opr operasjonstype
#' @param oa one anastomosis GB
#' @param dayIv dato interval
#' @param opr operation type
#'
#' @return A data frame for choice of hospitals, years and opr.type
#'
#' @export


siivu <- function(df, sh, yr, prm, opr, oa, dayIv) {
  # if dato_iv exists()
  # df %>%
  #   dplyr::filter(.data$OperererendeSykehus %in% .env$sh,
  #                 # .data$op_aar %in% .env$yr,
  #                 .data$op_primar %in% .env$prm,
  #                 .data$Operasjonsmetode %in% .env$opr,
  #                 .data$Opmetode_GBP %in% .env$oa,
  #                 .data$dato_iv %in% .env$dayIv)
  # else
    df %>%
    dplyr::filter(.data$OperererendeSykehus %in% .env$sh,
                  .data$op_aar %in% .env$yr,
                  .data$op_primar %in% .env$prm,
                  .data$Operasjonsmetode %in% .env$opr,
                  .data$Opmetode_GBP %in% .env$oa,
                  .data$dato_iv %in% .env$dayIv)
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

lgg_gr <- function(df) {
  maksdogn_vis <- 14
  liggeDogn <- liggedognTrunk <- liggedognLenge <- NULL
  # r-bloggers.com/2019/08/no-visible-binding-for-global-variable
df %<>%
  dplyr::mutate(
    liggedognLenge = liggeDogn > maksdogn_vis,
    liggedognTrunk = pmin(liggeDogn, maksdogn_vis + 1))

    liggedognMaks <- max(df$liggeDogn, na.rm = TRUE)
    liggedognBreaks <- seq(
        pmin(1, min(df$liggeDogn, na.rm = TRUE)),
        maksdogn_vis + 1)
    liggedognTekst <- liggedognBreaks
    liggedognTekst[length(liggedognTekst)] <-
      paste0("\u2265", maksdogn_vis + 1)


df %>%
  dplyr::filter(!is.na(liggeDogn) & !(liggeDogn < 0)) %>%
  ggplot2::ggplot(ggplot2::aes(x = liggedognTrunk,
                               fill = liggedognLenge)) +
  ggplot2::geom_bar(stat = "count",
                    show.legend = FALSE) +
  ggplot2::scale_fill_manual(values = c("FALSE" = "blue",
                                        "TRUE" = "red")) +
  ggplot2::scale_x_continuous(breaks = liggedognBreaks,
                              labels = liggedognTekst,
                              expand = c(0, .6)) +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.0, .05),
                                                          add = 0)) +
  ggplot2::xlab("Ligged\u00F8gn") +
  ggplot2::ylab("Talet p\u00E5 pasienter")
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

#' lage komplikasjonsgraf
#' @param df data frame
#' @return df data frame grouped by year and hospital
#' @export

kompl_gr <- function(df) {
  `6U_KomplAlvorGrad` <- kompl_grad_tekst <- n <- NULL
  fjern_x <- ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                            panel.grid.minor.x = ggplot2::element_blank())
  fjern_y <- ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                            panel.grid.minor.y = ggplot2::element_blank())
  fjern_x_ticks <- ggplot2::theme(axis.ticks.x = ggplot2::element_blank())
  fjern_y_ticks <- ggplot2::theme(axis.ticks.y = ggplot2::element_blank())
  colPrim <- c("#000059", "#084594", "#2171b5", "#4292c6", "#6baed6",
               "#c6dbef")

  d_kompl_graf <- df %>%
    dplyr::filter(!is.na(`6U_KomplAlvorGrad`)) %>%
    dplyr::count(`6U_KomplAlvorGrad`) %>%
    dplyr::mutate(kompl_grad_tekst =
                    factor(`6U_KomplAlvorGrad`,
                           levels = rev(c(1:4, 6:7, 5)),
                           labels = rev(c("Grad I: Ingen tiltak",
                                          "Grad II: Farmakologiske tiltak",
                                      "Grad IIIa: Intervensjon uten narkose",
                                      "Grad IIIb: Intervensjon i narkose",
          "Grad IVa: Intensivbehandling med eitt sviktande organ",
          "Grad IVb: Intensivbehandling med meir enn eitt sviktande organ",
                                          "Grad V: D\u00F8d"))))

ggplot2::ggplot(d_kompl_graf, ggplot2::aes(x = kompl_grad_tekst, y = n)) +
  ggplot2::geom_bar(stat = "identity", fill = colPrim[3], width = 2 / 3) +
  ggplot2::scale_y_continuous(breaks = 5,
                              expand = ggplot2::expansion(mult = c(0.0, .05),
                                                          add = 0)) +
  ggplot2::scale_x_discrete(drop = FALSE) +
  ggplot2::xlab(NULL) + ggplot2::ylab("Talet p\u00E5 pasienter") +
  ggplot2::coord_flip() + fjern_y +  fjern_y_ticks +
  ggplot2::theme(panel.grid.minor.x =  ggplot2::element_blank())
}

#' lage aarskontrolltabell
#' @param df data frame
#' @param k which year control
#' @return df data frame grouped by year and hospital
#' @export

aarKtrl <- function(df, k) {
  nt <- et_nt <- to_nt <- NULL
  last_opday <- max(df$Operasjonsdato)
  switch(k,
  "1" = {
    last_op <-  last_opday - months(15)},
  "2" = {
    last_op <-  last_opday - months(27)})
  df <- df %>%
    dplyr::mutate(
      et_nor_m = nitti_m(dag = .data$Operasjonsdato),
      et_nor_p = nitti_p(dag = .data$Operasjonsdato),
      to_nor_m = nitti_m(yr = 2, dag = .data$Operasjonsdato),
      to_nor_p = nitti_p(yr = 2, dag = .data$Operasjonsdato),
      pTWL = 100 * (.data$BR_Vekt - .data$`ToAar_Vekt`) / .data$BR_Vekt)

df <- df %>%
    dplyr::mutate(
      et_nt = .data$EttAar_Oppfolgingsdato %within%
        lubridate::interval(.data$et_nor_m, .data$et_nor_p),
      to_nt = .data$ToAar_Oppfolgingsdato %within%
        lubridate::interval(.data$to_nor_m, .data$to_nor_p),
      et_b4 = .data$EttAar_Oppfolgingsdato %within%
        lubridate::interval(.data$Operasjonsdato + 1, .data$et_nor_m - 1),
      et_lt = .data$EttAar_Oppfolgingsdato > .data$et_nor_p,
      to_b4 = .data$ToAar_Oppfolgingsdato %within%
        lubridate::interval(.data$Operasjonsdato + 1, .data$to_nor_m - 1),
      to_lt = .data$ToAar_Oppfolgingsdato > .data$to_nor_p)

df <- df %>% dplyr::select(
    c("PasientID", "OperererendeSykehus", "Operasjonsdato", "op_aar",
      "Operasjonsmetode", "Opmetode_GBP", "et_b4", "et_nt", "et_lt",
      "to_b4",  "to_nt", "to_lt", "pTWL"))

switch(k,
"1" = {
  df %>%
    dplyr::filter(.data$Operasjonsdato < last_op) %>%
    dplyr::group_by(.data$OperererendeSykehus, .data$op_aar) %>%
    dplyr::mutate(ops = dplyr::n()) %>%
    dplyr::summarise(ktrl = sum(et_nt, na.rm = T), oprs = .data$ops[1],
                     ktl = sum(et_nt, na.rm = T) / .data$ops[1])},
"2" = {
  df %>%
    dplyr::filter(.data$Operasjonsdato < last_op) %>%
    dplyr::group_by(.data$OperererendeSykehus, .data$op_aar) %>%
    dplyr::mutate(ops = dplyr::n()) %>%
    dplyr::summarise(ktrl = sum(to_nt, na.rm = T), oprs = .data$ops[1],
                     ktl = sum(to_nt, na.rm = T) / .data$ops[1])
  }
)
}



#----------------------------------------------------------------------------80
#' lage vekttaptabell
#' @param df data frame full data
#' @param opr_tp operasjonstype sleeve, bypass
#' @param opr_oa oagb
#' @return df data frame grouped by year and hospital
#' @export

twlTb <- function(df, opr_tp, opr_oa = 2) {

 dTwl  <- df %>%
  dplyr::filter(!is.na(.data$`ToAar_Vekt`)) %>%
  dplyr::mutate(
    pTWL = 100 * (.data$BR_Vekt - .data$`ToAar_Vekt`) / .data$BR_Vekt) %>%
  dplyr::mutate(del20 = .data$pTWL >= 20.0)
 # pTWL at 2 year must exist!
 d_slv  <- dTwl %>% dplyr::filter(.data$Operasjonsmetode == 6)
 d_gbp  <- dTwl %>% dplyr::filter(.data$Operasjonsmetode == 1,
                                   .data$Opmetode_GBP == 1)
 d_oa   <- dTwl %>% dplyr::filter(.data$Operasjonsmetode == 1,
                                   .data$Opmetode_GBP == 2)
 switch(opr_tp,
 "6"  = {
   detail(d_slv)},
 "1"  = {
   switch(opr_oa,
    "1" = detail(d_oa),
    "2" = detail(d_gbp)
    )}
 )
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

#----------------------------------------------------------------------------80
#' lage vekttaptabell
#' @param df data frame full data
#' @param opr_tp operasjonstype sleeve, bypass,
#' @param opr_oa oagb
#' @return df data frame grouped by year and hospital
#' @export

twlGr <- function(df, opr_tp, opr_oa = 2) {

  ggplot2::ggplot(data = df, ggplot2::aes(stat = "identity",
                                   x = .data$pTWL,
                                   color = .data$OperererendeSykehus)) +
  ggplot2::geom_density() + ggplot2::geom_vline(xintercept = 20,
                                                  linetype = "dashed",
                                                  color = "red")
}

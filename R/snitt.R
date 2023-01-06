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
  dplyr::filter(.data$OperererendeSykehus %in% sh,
                .data$op_aar %in% yr)
  } # as.character(.data$OpererendeRESH)

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
    dplyr::filter(.data$OperererendeSykehus %in% sh,
                  .data$op_aar %in% yr,
                  .data$op_primar %in% prm,
                  .data$Operasjonsmetode %in% opr)
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


siivu <- function(df, sh, yr, prm, opr, oa) {
  # if dato_iv exists()
  df %>%
    dplyr::filter(.data$OperererendeSykehus %in% sh,
                  .data$op_aar %in% yr,
                  .data$op_primar %in% prm,
                  .data$Operasjonsmetode %in% opr,
                  .data$Opmetode_GBP %in% oa)
}

#' Lag RESH tabellen
#'
#' @param registryName streng
#' @return dataframe tabell
#' @export

RESH_table <- function(registryName){
  query <- paste("select AvdRESH, SykehusNavn",
                 "from ForlopsOversikt",
                 "group by AvdRESH, SykehusNavn")
  rapbase::loadRegData(registryName, query)
}

#' Fra RESH til Sykehusnavn
#'
#' @param df data.table
#' @param RESHId num
#' @return dataframe tabell
#' @export

RESH_to_sh <- function(df, RESHId){
   df$SykehusNavn[df$AvdRESH == RESHId]
}

#' Fra RESH til Sykehus
#'
#' @param ct named list
#' @param RESHId num
#' @return dataframe tabell
#' @export

RESH_sh <- function(ct, RESHId){
  ct[[as.character(RESHId)]]
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

#' lage liggedogntabell
#' @param df data frame
#' @return df data frame grouped by year and hospital
#' @export

lgg_tb <- function(df) {
  res <- df %>%
 dplyr::group_by(.data$OperererendeSykehus, .data$op_aar) %>%
 dplyr::summarise(soreg::ki(dplyr::across(), "liggetid")) %>%
 dplyr::arrange(dplyr::desc(.data$indicator)) %>%
 dplyr::ungroup()
  res$op_aar <- format(res$op_aar, digits = 4)
  names(res) <- c("Sjukehus", "År", "teljare", "nemnare", "%")
  res
}

#' lage liggedogngraf
#' @param df data frame
#' @return df data frame grouped by year and hospital
#' @export

lgg_gr <- function(df) {
  maksdogn_vis <- 14
  LiggeDogn <- liggedognTrunk <- liggedognLenge <- NULL
  # r-bloggers.com/2019/08/no-visible-binding-for-global-variable
df %<>%
  dplyr::mutate(
    liggedognLenge = LiggeDogn > maksdogn_vis,
    liggedognTrunk = pmin(LiggeDogn, maksdogn_vis + 1))
    # liggedognMaks <- max(df$LiggeDogn, na.rm = TRUE)
    # liggedognBreaks <- seq(
    #     pmin(1, min(df$LiggeDogn, na.rm = TRUE)),
    #     maksdogn_vis + 1)
liggedognMaks <- max( LiggeDogn, na.rm = T)
liggedognBreaks <- 0:15
    liggedognTekst <- liggedognBreaks
    liggedognTekst[length(liggedognTekst)] <-
      paste0("\u2265", maksdogn_vis + 1)

df %>%
  dplyr::filter(!is.na(LiggeDogn) & !(LiggeDogn < 0)) %>%
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
  ggplot2::ylab("Talet p\u00E5 pasienter")+
  ggplot2::theme_minimal()
}


#' lage reinnleggningtabell
#' @param df data frame
#' @return df data frame grouped by year and hospital
#' @export

reinn_tb <- function(df)  {
  df <- df %>%
    dplyr::filter(((.data$u6_KontrollType %in% 1:3) |
                     (.data$u6_Behandling30Dager == 1)) &
                    (.data$u6_Behandling30Dager != 2))
  res <- df %>%
    dplyr::group_by(.data$OperererendeSykehus, .data$op_aar) %>%
    dplyr::summarise(soreg::ki(dplyr::across(), "dag30")) %>%
    dplyr::arrange(dplyr::desc(.data$indicator))
  res$op_aar <- format(res$op_aar, digits = 4)
  names(res) <- c("Sjukehus", "Opr.år", "teljare", "operasjonar", "%")
  res
}

#' lage reinnleggninggraf
#' @param df data frame
#' @return df data frame grouped by year and hospital
#' @export

reinn_gr <- function(df) {
 # df <-
  df %>%
    dplyr::filter(((.data$u6_KontrollType %in% 1:3) |
                     (.data$u6_Behandling30Dager == 1)) &
                    (.data$u6_Behandling30Dager != 2))  %>%
#  res <- df %>%
    dplyr::group_by(.data$OperererendeSykehus, .data$op_aar) %>%
    dplyr::summarise(soreg::ki(dplyr::across(), "dag30")) %>%
    dplyr::arrange(dplyr::desc(.data$indicator))  %>%

#  d_reinn_graf <- res %>%
    ggplot2::ggplot() +
    ggplot2::aes(x=op_aar, y = indicator,
                 group = OperererendeSykehus, color = OperererendeSykehus) +
    ggplot2::geom_line() +
    ggplot2::theme_minimal()
#  d_reinn_graf
}

#' lage komplikasjontabell
#' @param df data frame
#' @return df data frame grouped by year and hospital
#' @export

kompl_tb <- function(df) {
  df <- df %>%
 dplyr::filter((.data$u6_KontrollType %in% 1:3) |
                 (!is.na(.data$u6_KomplAlvorGrad)))
res <- df  %>%
 dplyr::group_by(.data$OperererendeSykehus, .data$op_aar) %>%
  dplyr::summarise(soreg::ki(dplyr::across(), "kompl")) %>%
 dplyr::arrange(dplyr::desc(.data$indicator))
res$op_aar <- format(res$op_aar, digits = 4)
names(res) <- c("Sjukehus", "År", "teljare", "nemnare", "%")
res
}

#' lage komplikasjonsgraf
#' @param df data frame
#' @return df data frame grouped by year and hospital
#' @export

kompl_gr <- function(df) {
  u6_KomplAlvorGrad <- kompl_grad_tekst <- n <- NULL
  fjern_x <- ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                            panel.grid.minor.x = ggplot2::element_blank())
  fjern_y <- ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                            panel.grid.minor.y = ggplot2::element_blank())
  fjern_x_ticks <- ggplot2::theme(axis.ticks.x = ggplot2::element_blank())
  fjern_y_ticks <- ggplot2::theme(axis.ticks.y = ggplot2::element_blank())
  colPrim <- c("#000059", "#084594", "#2171b5", "#4292c6", "#6baed6",
               "#c6dbef")

  d_kompl_graf <- df %>%
    dplyr::filter(!is.na(u6_KomplAlvorGrad)) %>%
    dplyr::count(u6_KomplAlvorGrad) %>%
    dplyr::mutate(kompl_grad_tekst =
                    factor(u6_KomplAlvorGrad,
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
  ggplot2::theme(panel.grid.minor.x =  ggplot2::element_blank()) +
  ggplot2::theme_minimal()
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
      pTWL = 100 * (.data$BR_Vekt - .data$`a2_Vekt`) / .data$BR_Vekt)

df <- df %>%
    dplyr::mutate(
      et_nt = .data$a1_KontrollDato %within%
        lubridate::interval(.data$et_nor_m, .data$et_nor_p),
      to_nt = .data$a2_KontrollDato %within%
        lubridate::interval(.data$to_nor_m, .data$to_nor_p),
      et_b4 = .data$a1_KontrollDato %within%
        lubridate::interval(.data$Operasjonsdato + 1, .data$et_nor_m - 1),
      et_lt = .data$a1_KontrollDato > .data$et_nor_p,
      to_b4 = .data$a2_KontrollDato %within%
        lubridate::interval(.data$Operasjonsdato + 1, .data$to_nor_m - 1),
      to_lt = .data$a2_KontrollDato > .data$to_nor_p)

df <- df %>% dplyr::select(
    c("PasientID", "OperererendeSykehus", "Operasjonsdato", "op_aar",
      "Operasjonsmetode", "Opmetode_GBP", "et_b4", "et_nt", "et_lt",
      "to_b4",  "to_nt", "to_lt", "pTWL"))
df$op_aar <- format(df$op_aar, digits = 4)

switch(k,
"1" = {
  df %>%
    dplyr::filter(.data$Operasjonsdato < last_op) %>%
    dplyr::group_by(.data$OperererendeSykehus, .data$op_aar) %>%
    dplyr::mutate(ops = dplyr::n()) %>%
    dplyr::summarise(ktrl = sum(et_nt, na.rm = T), oprs = .data$ops[1],
                     ktl = sum(et_nt, na.rm = T) / .data$ops[1]) %>%
    dplyr::arrange(dplyr::desc(.data$ktl))},
"2" = {
  df %>%
    dplyr::filter(.data$Operasjonsdato < last_op) %>%
    dplyr::group_by(.data$OperererendeSykehus, .data$op_aar) %>%
    dplyr::mutate(ops = dplyr::n()) %>%
    dplyr::summarise(ktrl = sum(to_nt, na.rm = T), oprs = .data$ops[1],
                     ktl = sum(to_nt, na.rm = T) / .data$ops[1]) %>%
    dplyr::arrange(dplyr::desc(.data$ktl))}
)
}



#' lage aarskontrollfigur
#' @param df data frame
#' @param k which year control
#' @return df data frame grouped by year and hospital
#' @export


aar_ktr_gr <- function(df, k) {
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
      pTWL = 100 * (.data$BR_Vekt - .data$`a2_Vekt`) / .data$BR_Vekt)

  df <- df %>%
    dplyr::mutate(
      et_nt = .data$a1_KontrollDato %within%
        lubridate::interval(.data$et_nor_m, .data$et_nor_p),
      to_nt = .data$a2_KontrollDato %within%
        lubridate::interval(.data$to_nor_m, .data$to_nor_p),
      et_b4 = .data$a1_KontrollDato %within%
        lubridate::interval(.data$Operasjonsdato + 1, .data$et_nor_m - 1),
      et_lt = .data$a1_KontrollDato > .data$et_nor_p,
      to_b4 = .data$a2_KontrollDato %within%
        lubridate::interval(.data$Operasjonsdato + 1, .data$to_nor_m - 1),
      to_lt = .data$a2_KontrollDato > .data$to_nor_p)

  df <- df %>% dplyr::select(
    c("PasientID", "OperererendeSykehus", "Operasjonsdato", "op_aar",
      "Operasjonsmetode", "Opmetode_GBP", "et_b4", "et_nt", "et_lt",
      "to_b4",  "to_nt", "to_lt", "pTWL"))
  df$op_aar <- format(df$op_aar, digits = 4)

  switch(k,
         "1" = {
           df %>%
             dplyr::filter(.data$Operasjonsdato < last_op) %>%
             dplyr::group_by(.data$OperererendeSykehus, .data$op_aar) %>%
             dplyr::mutate(ops = dplyr::n()) %>%
             dplyr::summarise(ktrl = sum(et_nt, na.rm = T), oprs = .data$ops[1],
                              ktl = sum(et_nt, na.rm = T) / .data$ops[1]) %>%
             ggplot2::ggplot(ggplot2::aes(x = op_aar , y = ktl,
                                          group = OperererendeSykehus,
                                          color = OperererendeSykehus)  ) +
             ggplot2::geom_line()+
             ggplot2::theme_minimal()
               },
         "2" = {
           df %>%
             dplyr::filter(.data$Operasjonsdato < last_op) %>%
             dplyr::group_by(.data$OperererendeSykehus, .data$op_aar) %>%
             dplyr::mutate(ops = dplyr::n()) %>%
             dplyr::summarise(ktrl = sum(to_nt, na.rm = T), oprs = .data$ops[1],
                              ktl = sum(to_nt, na.rm = T) / .data$ops[1]) %>%
             ggplot2::ggplot(ggplot2::aes(x = op_aar , y = ktl,
                                          group = OperererendeSykehus,
                                          color = OperererendeSykehus)  ) +
             ggplot2::geom_line()+
             ggplot2::theme_minimal()
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

# twlTb <- function(df, opr_tp, opr_oa) {
#
#  dTwl  <- df %>%
#   dplyr::filter(!is.na(.data$a2_Vekt))
# # %>%
# #  dplyr::mutate(
# #    pTWL = 100 * (.data$BR_Vekt - .data$a2_Vekt) / .data$BR_Vekt) %>%
# #  dplyr::mutate(del20 = .data$pTWL >= 20.0)
#  # pTWL at 2 year must exist!
#  d_slv  <- dTwl %>% dplyr::filter(.data$Operasjonsmetode == 6)
#  d_gbp  <- dTwl %>% dplyr::filter(.data$Operasjonsmetode == 1,
#                                    .data$Opmetode_GBP == 1)
#  d_oa   <- dTwl %>% dplyr::filter(.data$Operasjonsmetode == 1,
#                                    .data$Opmetode_GBP == 2)
#  switch(opr_tp,
#  "6"  = {
#    detail(d_slv)},
#  "1"  = {
#    switch(opr_oa,
#     "1" = detail(d_gbp),
#     "2" = detail(d_oa)
#     )}
#  )
# }


twlTb <- function(){detail(slc())}


#' lage vekttapdetaljer
#' @param dm data frame
#' @return df data frame grouped by year and hospital
#' @export

detail <- function(dm) {
  res <- dm %>%
    dplyr::group_by(.data$OperererendeSykehus, .data$op_aar) %>%
    dplyr::summarise("tyve" = sum(.data$del20, na.rm = TRUE),
                     "ops" = dplyr::n(),
                     "minst20" = 100 * mean(.data$del20, na.rm = TRUE))
  res$op_aar <- format(res$op_aar, digits = 4)
 # res$minst20 <- 100*res$minst20
  names(res) <- c("Sjukehus", "År", "Vekttap ≥ 20%", "Operasjonar", "%")
  res %>%  dplyr::arrange(dplyr::desc(.data$`%`))
}

#----------------------------------------------------------------------------80
#' lage vekttaptabell
#' @param df data frame full data
#' @param opr_tp operasjonstype sleeve, bypass,
#' @param opr_oa oagb
#' @return df data frame grouped by year and hospital
#' @export

twlGr <- function(df, opr_tp, opr_oa) {
  dTwl  <- df %>%
    dplyr::filter(!is.na(.data$a2_Vekt)) %>%
    dplyr::mutate(
      pTWL = 100 * (.data$BR_Vekt - .data$a2_Vekt) / .data$BR_Vekt) %>%
    dplyr::mutate(del20 = .data$pTWL >= 20.0)
  # pTWL at 2 year must exist!
  d_slv  <- dTwl %>% dplyr::filter(.data$Operasjonsmetode == 6)
  d_gbp  <- dTwl %>% dplyr::filter(.data$Operasjonsmetode == 1,
                                   .data$Opmetode_GBP == 1)
  d_oa   <- dTwl %>% dplyr::filter(.data$Operasjonsmetode == 1,
                                   .data$Opmetode_GBP == 2)
   dr = switch(opr_tp,
         "6"  = {
          detail( d_slv)},
         "1"  = {
           switch(opr_oa,
                  "1" = detail(d_gbp),
                  "2" = detail(d_oa)
           )}
  )

ggplot2::ggplot(data = dr,
                ggplot2::aes(x=År , y=`%`, color=Sjukehus, group=Sjukehus)) +
  ggplot2::geom_line() +
  ggplot2::theme_minimal()
  # ggplot2::ggplot(data = d_fr) +
  # ggplot2::geom_density(    ggplot2::aes(stat = "identity",
  #                                        x = pTWL,
  #                                        color = OperererendeSykehus)
  # ) +
  #   ggplot2::geom_vline(xintercept = 20,
  #                       linetype = "dashed",
  #                       color = "red") +
  # ggplot2::theme_minimal()
}

#' lage vekttapgraf
#' @param df data frame sliced
#' @export

wlGr <- function(df){
  df %>% ggplot2::ggplot(ggplot2::aes(x = År, y = `%`, color=Sjukehus, group=Sjukehus)) +
    ggplot2::geom_line() +
    ggplot2::theme_minimal()
}

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
  dplyr::filter( OperererendeSykehus %in% sh,
                 op_aar %in% yr)
  } # as.character( OpererendeRESH)

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
    dplyr::filter( OperererendeSykehus %in% sh,
                   op_aar %in% yr,
                   op_primar %in% prm,
                   Operasjonsmetode %in% opr)
}

#' Pick particular hospitals and years from a data frame
#'
#' @param df Data frame holding SoReg data
#' @param sh hospital(s)
#' @param yr year(s)
#' @param prm primary operation
#' @param opr operasjonstype
#' @param oa one anastomosis GB
#'
#' @return A data frame for choice of hospitals, years and opr.type
#'
#' @export


siivu <- function(df, sh, yr, prm, opr, oa) {
  # if dato_iv exists()
  df %>%
    dplyr::filter( OperererendeSykehus %in% sh,
                   op_aar %in% yr,
                   op_primar %in% prm,
                   Operasjonsmetode %in% opr,
                   Opmetode_GBP %in% oa)
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

  nitti_m <- function(yr, dag, l) {
    dag - lubridate::ddays(l) + lubridate::years(yr)
  }

#' slingringsmonn for aarskontrollar, plus
#' @param yr years
#' @param dag operationday
#' @param l number of deviating controldays
#' @return end day of 'normtid'
#' @export

  nitti_p <- function(yr, dag, l) {
    dag + lubridate::ddays(l) + lubridate::years(yr)
  }

#' slingringsmonn for aarskontrollar, interval
#' @param yr years
#' @param dag operationday
#' @param l number of deviating controldays
#' @return interval of 'normtid'
#' @export

  nitti <- function(yr, dag = lubridate::today(), l = 90) {
    c(dag - lubridate::ddays(l) + lubridate::years(yr),
      dag + lubridate::ddays(l) + lubridate::years(yr))
  }

#' lage liggedogntabell
#' @param df data frame
#' @param agg lgl
#' @return df data frame grouped by year and hospital
#' @export

lgg_tb <- function(df, agg) {
  if (!agg) {
    res <- df %>%
      dplyr::group_by( OperererendeSykehus) %>%
      dplyr::summarise(soreg::ki(dplyr::across(), "liggetid")) %>%
      dplyr::arrange(dplyr::desc( indicator))
    names(res) <- c("Sjukehus", "Liggedøgn ≤4", "Operasjonar", "%")
    res
  } else
  {  res <- df %>%
    dplyr::group_by( OperererendeSykehus,  op_aar) %>%
    dplyr::summarise(soreg::ki(dplyr::across(), "liggetid")) %>%
    dplyr::arrange(dplyr::desc( indicator))
  res$op_aar <- format(res$op_aar, digits = 4)
  names(res) <- c("Sjukehus", "År", "Liggedøgn ≤4", "Operasjonar", "%")
  res}
}

#' lage liggedogngraf
#' @param df data frame
#' @return df data frame grouped by year and hospital
#' @export

lgg_gr <- function(df) {
  maksdogn_vis <- 14
   df %<>%
    dplyr::mutate(
      liggedognLenge = df$LiggeDogn > maksdogn_vis,
      liggedognTrunk = pmin(df$LiggeDogn, maksdogn_vis + 1))

  # liggedognMaks <- max( df$LiggeDogn, na.rm = T)
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
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(size = 20),
      axis.title.y = ggplot2::element_text(size = 20),
      axis.text = ggplot2::element_text(size = 14)
    )
}


#' lage reinnleggningtabell
#' @param df data frame
#' @param agg lgl
#' @return df data frame grouped by year and hospital
#' @export

reinn_tb <- function(df, agg)  {
  df <- df %>%
    dplyr::filter((( u6_KontrollType %in% 1:3) |
                     ( u6_Behandling30Dager == 1)) &
                    ( u6_Behandling30Dager != 2))
  if (!agg) {
    res <- df %>%
      dplyr::group_by( OperererendeSykehus ) %>%
      dplyr::summarise(soreg::ki(dplyr::across(), "dag30")) %>%
      dplyr::arrange(dplyr::desc( indicator))
    names(res) <- c("Sjukehus",   "Reinnlagt", "Operasjonar", "%")
    res
  } else {
  res <- df %>%
    dplyr::group_by( OperererendeSykehus,  op_aar) %>%
    dplyr::summarise(soreg::ki(dplyr::across(), "dag30")) %>%
    dplyr::arrange(dplyr::desc( indicator))
  res$op_aar <- format(res$op_aar, digits = 4)
  names(res) <- c("Sjukehus", "Opr.år", "Reinnlagt", "Operasjonar", "%")
  res}
}

#' lage reinnleggninggraf
#' @param df data frame
#' @param agg lgl
#' @return df data frame grouped by year and hospital
#' @export

reinn_gr <- function(df, agg)  {
  p_sz = 3
  lw = 2
  tl_sz = 18  # rel()
  tc_sz = 16
  ang = 90

  df <- df %>%
    dplyr::filter((( u6_KontrollType %in% 1:3) |
                     ( u6_Behandling30Dager == 1)) &
                    ( u6_Behandling30Dager != 2))
  if (!agg) {
    res <- df %>%
      dplyr::group_by( OperererendeSykehus ) %>%
      dplyr::summarise(soreg::ki(dplyr::across(), "dag30")) %>%
      dplyr::arrange(dplyr::desc( indicator))
    names(res) <- c("Sjukehus",   "Reinnlagt", "Operasjonar", "%")
   p <- res %>%  dplyr::mutate(Sjukehus = forcats::fct_reorder(Sjukehus, dplyr::desc(`%`))) %>%
      ggplot2::ggplot(ggplot2::aes(x = Sjukehus,  y = `%`,  group = Sjukehus, fill = Sjukehus)) +
      ggplot2::geom_bar( stat = "identity" ) +  # histogram
      ggplot2::scale_x_discrete( "Sjukehus")+
      ggplot2::scale_y_continuous("Reinnlagt, %", labels = scales::label_number())+
      ggplot2::theme_minimal()
    p +  ggplot2::theme(axis.text.x = ggplot2::element_text(size = tc_sz,  angle = ang),
                        axis.title.y = ggplot2::element_text(size = tc_sz),
                         axis.text = ggplot2::element_text(size = tl_sz),
                        legend.position = "none"
                        )
  } else
  {   res <- df %>%
    dplyr::group_by( OperererendeSykehus,  op_aar) %>%
    dplyr::summarise(soreg::ki(dplyr::across(), "dag30")) %>%
    dplyr::arrange(dplyr::desc( indicator))
  res$op_aar <- format(res$op_aar, digits = 4)
  names(res) <- c("Sjukehus", "Opr.år", "Reinnlagt", "Operasjonar", "%")

  if (length(unique(df$op_aar))>1)
  {
    p <-res %>% ggplot2::ggplot( ) +
      ggplot2::aes(x = Opr.år, y = `%`, group = Sjukehus, color = Sjukehus )+
      ggplot2::geom_line(linewidth = lw) +
      ggplot2::scale_x_discrete( "Operasjonsår")+
      ggplot2::scale_y_continuous("Reinnlagt, %", labels = scales::label_number())+
      ggplot2::theme_minimal()
  } else if  (length(unique(df$op_aar))<1) {message("tomt valg!")}
  else {
    p <-res %>% ggplot2::ggplot( ) +
      ggplot2::aes(x = Opr.år, y = `%`, group = Sjukehus, color = Sjukehus )+
      ggplot2::geom_point(size=p_sz) +
      ggplot2::scale_x_discrete( "Operasjonsår")+
      ggplot2::scale_y_continuous("Reinnlagt, %", labels = scales::label_number())+
      ggplot2::theme_minimal()
  }

  p +  ggplot2::theme(
    axis.title.x = ggplot2::element_text(size = tl_sz),
    axis.title.y = ggplot2::element_text(size = tl_sz),
    axis.text.x  = ggplot2::element_text(size = tl_sz))
  }
}
#' lage komplikasjontabell
#' @param df data frame
#' @param agg lgl
#' @return df data frame grouped by year and hospital
#' @export

kompl_tb <- function(df, agg) {
  df <- df %>%
    dplyr::filter(( u6_KontrollType %in% 1:3) |
                    (!is.na( u6_KomplAlvorGrad)))
  if (!agg) {res <- df  %>%
    dplyr::group_by( OperererendeSykehus) %>%
    dplyr::summarise(soreg::ki(dplyr::across(), "kompl")) %>%
    dplyr::arrange(dplyr::desc( indicator))
  names(res) <- c("Sjukehus",  "Komplikasjonar", "Operasjonar", "%")
  res} else {
    res <- df  %>%
      dplyr::group_by( OperererendeSykehus,  op_aar) %>%
      dplyr::summarise(soreg::ki(dplyr::across(), "kompl")) %>%
      dplyr::arrange(dplyr::desc( indicator))
    res$op_aar <- format(res$op_aar, digits = 4)
    names(res) <- c("Sjukehus", "År", "Komplikasjonar", "Operasjonar", "%")
    res}
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
                           labels = rev(c("Grad I: ",
                                          "Grad II: ",
                                      "Grad IIIa:  ",
                                      "Grad IIIb:  ",
          "Grad IVa:  ",
          "Grad IVb: ",
                                          "Grad V: "))))  # D\u00F8d

ggplot2::ggplot(d_kompl_graf, ggplot2::aes(x = kompl_grad_tekst, y = n)) +
  ggplot2::geom_bar(stat = "identity", fill = colPrim[3], width = 2 / 3) +
  ggplot2::scale_y_continuous(breaks =scales::breaks_width (5),
                              expand = ggplot2::expansion(mult = c(0, .05))) +
  ggplot2::scale_x_discrete(drop = FALSE) +
  ggplot2::xlab(NULL) + ggplot2::ylab("Talet p\u00E5 pasienter") +
  ggplot2::coord_flip() + fjern_y +  fjern_y_ticks +
  ggplot2::theme(panel.grid.minor.x =  ggplot2::element_blank()) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(size = 14),
    axis.text.x = ggplot2::element_text(size = 12, angle = 90 ),
   # axis.title.y = ggplot2::element_text(size =16),
    axis.text.y = ggplot2::element_text(size = 14 ))
}


#' lage aarskontrolltabell
#' @param df data frame
#' @param k which year control
#' @param agg lgl resultater aggregert
#' @return df data frame grouped by year and hospital
#' @export

aarKtrl <- function(df, k, agg){
  if (!agg) {
    switch(k,
           "1" = {res <- df %>%
             dplyr::group_by( OperererendeSykehus) %>%
             dplyr::summarise(soreg::ki(dplyr::across(), "K1")) %>%
             dplyr::arrange(dplyr::desc( indicator))
           names(res) <- c("Sjukehus",   "Kontroll i normtid", "Operasjonar", "%")
           res},
           "2" =  {res <- df %>%
             dplyr::group_by( OperererendeSykehus ) %>%
             dplyr::summarise(soreg::ki(dplyr::across(), "K2")) %>%
             dplyr::arrange(dplyr::desc( indicator))
           names(res) <- c("Sjukehus",   "Kontroll i normtid", "Operasjonar", "%")
           res})
  } else {
    switch(k,
           "1" = {res <- df %>%
             dplyr::group_by( OperererendeSykehus,  op_aar) %>%
             dplyr::summarise(soreg::ki(dplyr::across(), "K1")) %>%
             dplyr::arrange(dplyr::desc( indicator))
           res$op_aar <- format(res$op_aar, digits = 4)
           names(res) <- c("Sjukehus", "År", "Kontroll i normtid", "Operasjonar", "%")
           res},
           "2" =  {res <- df %>%
             dplyr::group_by( OperererendeSykehus,  op_aar) %>%
             dplyr::summarise(soreg::ki(dplyr::across(), "K2")) %>%
             dplyr::arrange(dplyr::desc( indicator))
           res$op_aar <- format(res$op_aar, digits = 4)
           names(res) <- c("Sjukehus", "År", "Kontroll i normtid", "Operasjonar", "%")
           res}
    )}
}

#' lage aarskontrollfigur
#' @param df data frame
#' @param k which year control
#' @return df data frame grouped by year and hospital
#' @export


aar_ktr_gr <- function(df, k) {
  # # ------- grafikk-parameters
  p_sz = 3
  lw = 2

  df <- df %>% dplyr::select(
    c("PasientID", "OperererendeSykehus", "Operasjonsdato", "op_aar",
      "Operasjonsmetode", "Opmetode_GBP", "et_nt", "to_nt",  "pTWL"))
  df$op_aar <- format(df$op_aar, digits = 4)

  switch(k,
         "1" =   {
           if   ( length(unique(df$op_aar)) >1) {
             p <- df %>%
               dplyr::group_by( OperererendeSykehus,  op_aar) %>%
               dplyr::summarise(ktrl = sum(et_nt, na.rm = T), oprs = dplyr::n(),
                                ktl = sum(et_nt, na.rm = T) /dplyr::n()) %>%
               ggplot2::ggplot(ggplot2::aes(x = op_aar , y = ktl,
                                            group = OperererendeSykehus,
                                            color = OperererendeSykehus)  ) +
               ggplot2::geom_line(linewidth=lw)+
               ggplot2::scale_x_discrete( "Operasjonsår")+
               ggplot2::scale_y_continuous("Kontroll i normtid, %", labels = scales::percent)+
               ggplot2::theme_minimal() } else {p <-df %>%
                 dplyr::group_by( OperererendeSykehus ) %>%
                 dplyr::summarise(ktrl = sum(et_nt, na.rm = T), oprs = dplyr::n(),
                                  ktl = sum(et_nt, na.rm = T) /dplyr::n()) %>%
                 ggplot2::ggplot(ggplot2::aes(x = unique(df$op_aar),  y = ktl,
                                              group = OperererendeSykehus,
                                              color = OperererendeSykehus)  ) +
                 ggplot2::geom_point(size=p_sz)+
                 ggplot2::scale_x_discrete( "Operasjonsår")+
                 ggplot2::scale_y_continuous("Kontroll i normtid, %", labels = scales::percent )+
                 ggplot2::theme_minimal()    }
         },
         "2" = {
           if   ( length(unique(df$op_aar)) >1) {
             p<- df %>%
               dplyr::group_by( OperererendeSykehus,  op_aar) %>%
               dplyr::summarise(ktrl = sum(to_nt, na.rm = T), oprs = dplyr::n(),
                                ktl = sum(to_nt, na.rm = T) / dplyr::n()) %>%
               ggplot2::ggplot(ggplot2::aes(x = op_aar , y = ktl,
                                            group = OperererendeSykehus,
                                            color = OperererendeSykehus)  ) +
               ggplot2::geom_line(linewidth=lw)+
               ggplot2::scale_x_discrete( "Operasjonsår")+
               ggplot2::scale_y_continuous("Kontroll i normtid, %", labels = scales::percent)+
               ggplot2::theme_minimal()  } else {p<- df %>%
                 dplyr::group_by( OperererendeSykehus ) %>%
                 dplyr::summarise(ktrl = sum(to_nt, na.rm = T), oprs = dplyr::n(),
                                  ktl = sum(to_nt, na.rm = T) /dplyr::n()) %>%
                 ggplot2::ggplot(ggplot2::aes(x = unique(df$op_aar),  y = ktl,
                                              group = OperererendeSykehus,
                                              color = OperererendeSykehus)  ) +
                 ggplot2::geom_point(size=p_sz)+
                 ggplot2::scale_x_discrete( "Operasjonsår")+
                 ggplot2::scale_y_continuous("Kontroll i normtid, %", labels = scales::percent)+
                 ggplot2::theme_minimal()  }
         }
  )
  p  +    ggplot2::theme(
    axis.title.x = ggplot2::element_text(size = 20),
    axis.title.y = ggplot2::element_text(size = 20),
    axis.text = ggplot2::element_text(size = 14))
}


#' lage vekttapdetaljer
#' @param dm data frame
#' @param agg lgl resulater aggregert
#' @return df data frame grouped by year and hospital
#' @export

detail <- function(dm, agg) {
  if (!agg) {
    res <- dm %>%
      dplyr::group_by( OperererendeSykehus) %>%
      dplyr::summarise("tyve" = sum( del20, na.rm = TRUE),
                       "ops" = dplyr::n(),
                       "minst20" =tyve/ops )
    names(res) <- c("Sjukehus", "Vekttap ≥ 20%", "Operasjonar", "%")
    res %>%  dplyr::arrange(dplyr::desc( `%`))
  } else {
   res <- dm %>%
      dplyr::group_by( OperererendeSykehus,  op_aar) %>%
      dplyr::summarise("tyve" = sum( del20, na.rm = TRUE),
                       "ops" = dplyr::n(),
                       "minst20" = tyve/ops )
    res$op_aar <- format(res$op_aar, digits = 4)
    names(res) <- c("Sjukehus", "År", "Vekttap ≥ 20%", "Operasjonar", "%")
    res %>%  dplyr::arrange(dplyr::desc( `%`))}}


#' lage vekttapgraf
#' @param df data frame sliced
#' @param agg lgl detail toggle
#' @export

wlGr <- function(df, agg){
  # # ------- grafikk-parameters
  p_sz = 3
  lw = 2
  tl_sz = 18
  tc_sz = 16
  ang = 90
  if (agg)
  {  if (length(unique(df$`År`))>1) { # length(unique(df$`År`))
   p <- df %>% ggplot2::ggplot(ggplot2::aes(x = `År`, y = `%`, color=Sjukehus, group=Sjukehus)) +
      ggplot2::geom_line(linewidth=lw) +
    # ggplot2::labs(x = "Operasjonsår", y="Prosent toårs vekttap ≥ 20%") +
      ggplot2::scale_x_discrete( "Operasjonsår")+
      ggplot2::scale_y_continuous("Prosent to års vekttap ≥ 20%", labels = scales::percent)+
      ggplot2::theme_minimal()
   } else
      { # ett år
        p<-   df %>% ggplot2::ggplot(ggplot2::aes(x = År, y = `%`, color=Sjukehus, group=Sjukehus)) +
          ggplot2::geom_point(size = p_sz) +
       #   ggplot2::labs(x = "Operasjonsår", y="Prosent toårs vekttap ≥ 20%") +
          ggplot2::scale_x_discrete( "Operasjonsår")+
          ggplot2::scale_y_continuous("Prosent to års vekttap ≥ 20%", labels = scales::percent)+
          ggplot2::theme_minimal()
      }
    p+  ggplot2::theme(
      axis.title.x = ggplot2::element_text(size = tl_sz),
      axis.title.y = ggplot2::element_text(size = tl_sz ),
      axis.text = ggplot2::element_text(size = 14))
  } else
  { # histogram
    p <-  df %>%  dplyr::mutate(Sjukehus = forcats::fct_reorder(Sjukehus, dplyr::desc(`%`))) %>%
      ggplot2::ggplot(ggplot2::aes(x = Sjukehus,  y = `%`,  group = Sjukehus, fill = Sjukehus)) +
      ggplot2::geom_bar( stat = "identity" ) +
      ggplot2::scale_x_discrete( "Sjukehus") +
      ggplot2::scale_y_continuous("Prosent toårs vekttap ≥ 20%", labels = scales::percent,
                                  expand = ggplot2::expansion(mult = c(0.0, .5),
                                                              add = 0)) +
      ggplot2::theme_minimal()
    p +        ggplot2::theme(
        axis.title.x = ggplot2::element_text(size = tl_sz, angle = 0   ),
        axis.title.y = ggplot2::element_text(size = tl_sz, angle = ang),
        axis.text = ggplot2::element_text(size = tc_sz ),
        axis.text.x = ggplot2::element_text(angle = ang),
        legend.position = "none")
  }
}

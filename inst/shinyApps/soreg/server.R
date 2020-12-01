# library(shiny)
# library(shinyalert)
# library(shinyWidgets)
library(magrittr)
# library(soreg)
library(lubridate)
# library(tibble)
# library(DT)
# library(dplyr)
# library(rapbase)

server <- function(input, output, session) {
#----------------------------------------------------------------------------80
  # Faste verdier for applikasjonen
  registry_name <- "soreg"
  # Last inn data
  # Legg til info om operasjonsår og primæroperasjon
  d_full <- soreg::get_arsrp("soreg")
  d_full %<>% dplyr::mutate(
    op_aar = lubridate::year(Operasjonsdato),
    op_primar = (TidlFedmeOp == 0))
  d_prim <- d_full %>% dplyr::filter(op_primar)
  # I nokre analysar ser me berre på dei som har 6-vekesoppfølging
  # registrert. Hentar ut eiga datasett for desse
  d_prim_6v <- d_prim %>% dplyr::filter(`6U_KontrollType` %in% 1:3)

# LIGGEDØGN  ---#-------------- Kor mange låg mindre enn fire døgn per sjukehus
  d_kortligg_sjuk <- d_prim_6v %>%
    dplyr::group_by(OperererendeSykehus, op_aar) %>%
    dplyr::do(soreg::ki_liggetid(.)) %>%
    dplyr::arrange(desc(ind)) %>% dplyr::ungroup()
#----------------------------------------------------------------------------80
   kortligg    <- function(sh, yr){
    d_kortligg_sjuk %>% dplyr::filter(
      OperererendeSykehus %in% sh,
      op_aar %in% yr)}
# kortligg <- snitt(d_kortligg_sjuk, sh, yr)

  # REINNLEGGELSE
  # I analysar for reinnlegging ser me berre på dei som har 6-vekesoppfølging
  # registrert eller som er registrert som reinnlagd til trass for at dei
  # ikkje har 6-vekesoppfølging (jf. forklaringstekst for reinnleggings-
  # indikatoren i årsrapporten). Hentar ut eiga datasett for desse. Men det
  # viser seg at det òg er mogleg å svara «Vet ikke» (verdi 2) på om pasienten
  # vart reinnlagd. Desse gjev ingen informasjon, og vert derfor òg fjerna.
  d_reinn <- d_prim %>% dplyr::filter(((`6U_KontrollType` %in% 1:3) |
                                       (`6U_Behandling30Dager` == 1)) &
                                       (`6U_Behandling30Dager` != 2))

  # Tilsvarande for alvorlege komplikasjonar
  d_kompl <- d_prim %>%
    dplyr::filter((`6U_KontrollType` %in% 1:3) | (!is.na(`6U_KomplAlvorGrad`)))

  #----------------------------------------------------------------------------80
  # andel pasienter som får innleggelse innen 30 dager etter operasjon
  # per sjukehus
  d_innlegg30 <- d_reinn %>%
    dplyr::group_by(OperererendeSykehus, op_aar) %>%
    dplyr::do(soreg::ki_30dager(.)) %>%
    dplyr::arrange(desc(ind))
  # innl30_sh <- function(sh){d_innlegg30 %>%
  #     filter(OperererendeSykehus %in% sh)}
  # innl30_yr <- function(yr){d_innlegg30 %>%
  #     filter(op_aar %in% yr)}
 innl30 <- function(sh,yr){d_innlegg30 %>%
      filter(OperererendeSykehus %in% sh, op_aar %in% yr)}
# innl30 <- snitt(d_innlegg30, sh, yr)

  # KOMPLIKASJONAR

  # andel pasienter som får en alvorlig komplikasjon per sjukehus
  d_kompl_alv_sjukehus <- d_kompl %>%
    dplyr::group_by(OperererendeSykehus, op_aar) %>%
    dplyr::do(soreg::ki_kompl_alv(.)) %>%
    dplyr::arrange(desc(ind))

  # kompl_sh <- function(sh){d_kompl_alv_sjukehus %>%
  #     filter(OperererendeSykehus %in% sh)}
  # kompl_yr <- function(yr){d_kompl_alv_sjukehus %>%
  #     filter(op_aar %in% yr)}
  kompl <- function(sh, yr){d_kompl_alv_sjukehus %>%
      filter(OperererendeSykehus %in% sh, op_aar %in% yr)}
# kompl <- snitt(d_kompl_alv_sjukehus, sh, yr)

  # aarskontrollar
  ####################################################
  # +-90                                             #  slingringsmonn
  nitti_m <- function(yr = 1, dag = lubridate::today(), l = 90) {
    dag - lubridate::ddays(l) + lubridate::years(yr)
  }
  nitti_p <- function(yr = 1, dag = lubridate::today(), l = 90) {
    dag + lubridate::ddays(l) + lubridate::years(yr)
  }
  nitti <- function(yr = 1, dag = lubridate::today(), l = 90) {
    c(dag - lubridate::ddays(l) + years(yr),
      dag + lubridate::ddays(l) + lubridate::years(yr))
  }

  dmx <- d_full   # alle operasjoner for kontrollene

  dm <- dmx %>%  dplyr::mutate(et_nor_m = nitti_m(dag = Operasjonsdato),
                               et_nor_p = nitti_p(dag = Operasjonsdato),
                               to_nor_m = nitti_m(yr = 2, dag= Operasjonsdato),
                               to_nor_p = nitti_p(yr = 2, dag= Operasjonsdato),
                               pTWL = 100 * (BR_Vekt - `ToAar_Vekt`)/BR_Vekt )

  dn <- dm %>%
    dplyr::mutate(
      et_nt = EttAar_Oppfolgingsdato %within% interval(et_nor_m, et_nor_p),
      to_nt = ToAar_Oppfolgingsdato %within% interval(to_nor_m, to_nor_p),
      et_b4 = EttAar_Oppfolgingsdato %within%
        interval( Operasjonsdato+1, et_nor_m-1 ),
      et_lt = EttAar_Oppfolgingsdato > et_nor_p,
      to_b4 = ToAar_Oppfolgingsdato %within%
        interval( Operasjonsdato+1, to_nor_m-1 ),
      to_lt = ToAar_Oppfolgingsdato > to_nor_p)
  #----------------------------------------------------------------------------80
  dt  <-  dn %>%
    dplyr::select(
      c("PasientID", "OperererendeSykehus", "Operasjonsdato", "op_aar",
        "Operasjonsmetode", "Opmetode_GBP",
        "et_b4", "et_nt", "et_lt", "to_b4",  "to_nt", "to_lt", "pTWL"))

  ##-----------#---------------------------------------------------------------80

  # Gjenbrukbar funksjon for å bearbeide Rmd til html
  htmlRenderRmd <- function(srcFile, params = list()) {
    # set param needed for report meta processing
    # params <- list(tableFormat="html")
    system.file(srcFile, package="soreg") %>%
      knitr::knit() %>%
      markdown::markdownToHTML(.,
                               options = c(" fragment_only" ,
                                           " base64_images" ,
                                           " highlight_code" ),
                               encoding = "utf-8") %>%
      shiny::HTML()
  }

  # widget
  output$appUserName <- renderText(rapbase::getUserFullName(session))
  output$appOrgName <- renderText(rapbase::getUserReshId(session))


  # Brukerinformasjon
  userInfo <- rapbase::howWeDealWithPersonalData(session)
  observeEvent(input$userInfo, {
    shinyalert("Dette vet Rapporteket om deg:", userInfo,
               type = "", imageUrl = "rap/logo.svg",
               closeOnEsc = TRUE, closeOnClickOutside = TRUE,
               html = TRUE,
               confirmButtonText = rapbase::noOptOutOk())
  })

  # Veiledning
  output$veiledning <- renderUI({
    htmlRenderRmd("veiledning.Rmd")
  })

#-----------------------------------------------------# years in data -------80
output$uc_sh <- shiny::renderUI({
  shinyWidgets::pickerInput(
    inputId = "sh",
    label = "velg sjukehus",
    choices = (unique(d_full$OperererendeSykehus)),
    selected = "Testsjukhus Norge",
    multiple = TRUE,
    options = shinyWidgets::pickerOptions(actionsBox = TRUE,
                                  title = "Please select a hospital",
                                  header = "This is a list of hospitals"))
  })

  output$uc_years <- renderUI({
    ## years available, hardcoded if outside known context
    if (rapbase::isRapContext()) {
      years <- soreg::data_years(registry_name)
      # remove NAs if they exist (bad registry)
      years <- years[!is.na(years)]
    } else {
      years <- c("2016", "2017", "2018", "2019", "2020")
    }
    shiny::checkboxGroupInput(
      inputId = "aar",
      label ="År:",
      choices = years,
      selected = 2017:2018)
  })

  output$uc_prim <- renderUI({
     shiny::checkboxGroupInput(
      inputId = "prim",
      label = "Primæroperasjon ?",
      choices = unique(d_full$op_primar),
      selected = TRUE)
  })

output$uc_opr <- renderUI({
shiny::checkboxGroupInput(
      inputId = "op_tech",
      label = "Operasjonsteknikk",
      choices =   unique(d_full$Operasjonsmetode),  #c(1,6),
      selected = 6) # 6 = sleeve
    # conditional buttons?       this should appear iff op_tech == 1
})

output$uc_dates <- renderUI({
shiny::dateRangeInput(
     inputId = "dato_iv",
     label = "Operasjonsinterval?",
     start = min(d_full$Operasjonsdato), end = max(d_full$Operasjonsdato)),
})

  # lgdgn stats::
  # Viss nokon har *veldig* mange liggedøgn, vert
  # grafen uoversiktleg. Avgrensa derfor talet på
  # liggedøgn me viser grafisk.
  maksdogn_vis = 14
  d_prim_6v %<>%
    dplyr::mutate(
      liggedogn_lenge = LiggeDogn > maksdogn_vis,
      liggedogn_trunk = pmin(LiggeDogn, maksdogn_vis + 1))
  n_liggedogn_lenge = sum(d_prim_6v$liggedogn_lenge, na.rm=TRUE)
  liggedogn_maks = max(d_prim_6v$LiggeDogn, na.rm=TRUE)

  #---------------------------- KI1 figur og tabell----------------------------80

  liggedogn_breaks = seq(
    pmin(1,  min(d_prim_6v$LiggeDogn, na.rm = TRUE)), maksdogn_vis + 1)
  liggedogn_tekst = liggedogn_breaks
  liggedogn_tekst[length(liggedogn_tekst)] = paste0("\u2265", maksdogn_vis + 1)
  ####### ---------------------------------------------------------------------80
  KIi  <- shiny::reactive({ input$KIix })
  output$QI <- shiny::renderText({ KIi() })

  # output$lggpl <- renderPlot({
  #   d_prim_6v <- dplyr::filter(d_prim_6v, Operasjonsmetode == input$op_tech)
  #
  #   ggplot2::ggplot(
  #     dplyr::filter(d_prim_6v, LiggeDogn >=0), #   !is.na(LiggeDogn)),
  #     # ?? LiggeDogn[11] = -1455
  #     ggplot2::aes(x = liggedogn_trunk, fill = liggedogn_lenge)) +
  #     ggplot2::geom_bar(stat="count", show.legend = FALSE)
  # })
  #
  # output$reinnpl <- renderPlot({
  #   d_prim_6v <- dplyr::filter(d_prim_6v, Operasjonsmetode == input$op_tech)
  #
  #   ggplot2::ggplot(
  #     data = d_prim_6v,   #   !is.na(LiggeDogn)),
  #     ggplot2::aes(x = liggedogn_trunk, fill = liggedogn_lenge)) +
  #     ggplot2::geom_bar(stat="count", show.legend = FALSE)
  # })

  # KI1 - KI6--------- # which KI: f() --------------- # KI user controls------80
  KI <- reactive({
    switch(input$KIix,
           "KI1" =  snitt(d_kortligg_sjuk, input$sh, input$aar), #  1 LiggeDogn
           "KI2" =  snitt(d_innlegg30, input$sh, input$aar),     #  2 REINNLEGGELSE
           "KI3" =  snitt(d_kompl_alv_sjukehus, input$sh, input$aar),    #  3 komplikasjonar
           "KI4" =  runif,        #  4  1 årskontrollar i normtid
           "KI5" =  rexp,         #  5  2 årskontrollar i normtid
           "KI6" =  rnorm)        #  6   del %TWL >= 20
    # ds(input$n)       # tal trukket fra fordelingen  #  year, hospital

  })

  output$DT <-  renderTable({ KI() })
  #---------------------------- KI1 figur og tabell----------------------------80

  pl <- reactive({
    switch( input$KIix,
            "KI1" = {
              d_prim_6v <- dplyr::filter(d_prim_6v, Operasjonsmetode == input$op_tech)

              ggplot2::ggplot(data = dplyr::filter(d_prim_6v, LiggeDogn >=0), #
                              # ?? LiggeDogn[11] = -1455
              ggplot2::aes(x = liggedogn_trunk, fill = liggedogn_lenge)) +
              ggplot2::geom_bar(stat="count", show.legend = FALSE)
            }  ,       #  1 LiggeDogn  output$lggpl,
            "KI2" = {
              d_prim_6v <- dplyr::filter(d_prim_6v, Operasjonsmetode == input$op_tech)
              ggplot2::ggplot( data =  dplyr::filter(d_prim_6v, LiggeDogn < 3),   #   !is.na(LiggeDogn)),
                               ggplot2::aes(x = liggedogn_trunk, fill = liggedogn_lenge)) +
                ggplot2::geom_bar(stat="count", show.legend = FALSE)
            } ,        #  2 REINNLEGGELSE    output$reinnpl
            "KI3" =  rexp ,         #  3 komplikasjonar
# d_kompl_graf = d_kompl %>%
  # filter(!is.na(`6U_KomplAlvorGrad`)) %>%
  # count(`6U_KomplAlvorGrad`) %>%
  # mutate(kompl_grad_tekst = factor(`6U_KomplAlvorGrad`,
                                   # levels=rev(c(1:4,6:7,5)),
                                   # labels=rev(c("Grad I: Ingen tiltak",
                                            # "Grad II: Farmakologiske tiltak",
                                            # "Grad IIIa: Intervensjon uten narkose",
                                            # "Grad IIIb: Intervensjon i narkose",
                                            # "Grad IVa: Intensivbehandling med eitt sviktande organ",
                                            # "Grad IVb: Intensivbehandling med meir enn eitt sviktande organ",
                                            # "Grad V: Død"))))

# ggplot(d_kompl_graf, aes(x=kompl_grad_tekst, y=n))+
  # geom_bar(stat="identity", fill=colPrim[3], width = 2/3) +
  # scale_y_continuous(breaks=sett_avkutningspunkt_bredde(5),
                     # expand = expansion(mult = c(0, .05))) +
  # scale_x_discrete(drop = FALSE) +
  # xlab(NULL) + ylab("Talet på pasienter") +
  # coord_flip() + fjern_y + fjern_y_ticks +
  # theme(panel.grid.minor.x = element_blank())

            "KI4" =  runif,        #  4  1 årskontrollar i normtid
            "KI5" =  rexp,         #  5  2 årskontrollar i normtid
            "KI6" =  rnorm)        #  6   del %TWL >= 20
  })

  output$graf  <- renderPlot( pl())



  ##-----------#---------------------------------------------------------------80

  ## Figur
  output$PlotKI1 <- renderPlot({
    soreg::makeHist(df = d_full, var = input$vrb, bins = input$bn)
  })

  output$PlotKI2 <- renderPlot({
    soreg::makeHist(df = d_full, var = input$vrb, bins = input$bn)
  })
  output$PlotKI3 <- renderPlot({
    soreg::makeHist(df = d_full, var = input$vrb, bins = input$bn)
  })

  ## Tabell
  output$TableKI1 <- renderTable({
    soreg::makeHist(
      df = d_full,
      var = input$vrb,
      bins = input$bn,
      makeTable = TRUE)
  })

  lgg <- reactive({kortligg(input$sh, input$lggar)})
  output$liggdogn <- DT::renderDataTable({ lgg() })

  innl <- reactive({innl30(input$sh, input$lggar)})
  output$reinnl <- DT::renderDataTable({ innl() })

  kmpl <- reactive({kompl(input$sh, input$lggar)})
  output$kompl <- DT::renderDataTable({ kmpl() })

  ## DT::dataTableOutput(" ligge" )
  #----------------------------------------------------------------------------80
  # Samlerapport
  ## vis
  output$samlerapport <- renderUI({
    htmlRenderRmd(srcFile = "samlerapport.Rmd",
                  params = list(var = input$varS, bins = input$binsS))
  })

  ## last ned
  output$downloadSamlerapport <- downloadHandler(
    filename = function() {"samlerapport.html"},
    content = function(file) {
      srcFile <- normalizePath(system.file("samlerapport.Rmd",
                                           package = "soreg"))
      tmpFile <- "tmpSamlerapport.Rmd"
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(srcFile, tmpFile, overwrite = TRUE)
      out <- rmarkdown::render(tmpFile,
                               output_format = rmarkdown::html_document(),
                               params = list(var = input$varS,
                                             bins = input$binsS),
                               output_dir = tempdir())
      file.rename(out, file)
    }
  )
  #----------------------------------------------------------------------------80
  # Abonnement
  ## rekative verdier for å holde rede på endringer som skjer mens
  ## applikasjonen kjører
  rv <- reactiveValues(
    subscriptionTab = rapbase::makeUserSubscriptionTab(session))

  ## lag tabell over gjeldende status for abonnement
  output$activeSubscriptions <- DT::renderDataTable(
    rv$subscriptionTab, server = FALSE, escape = FALSE, selection = "none" ,
    options = list(dom = "tp" , ordning = FALSE), rownames = FALSE )

  ## lag side som viser status for abonnement, også når det ikke finnes noen
  output$subscriptionContent <- renderUI({
    userFullName <- rapbase::getUserFullName(session)
    userEmail <- rapbase::getUserEmail(session)
    if (length(rv$subscriptionTab) == 0) {
      p(paste("Ingen aktive abonnement for", userFullName))
    } else {
      tagList(
        p(paste0("Aktive abonnement som sendes per epost til ",
                 userFullName,
                 "(",userEmail, "):")),
        DT::dataTableOutput("activeSubscriptions")
      )
    }
  })

  ## nye abonnement
  observeEvent (input$subscribe, {
    package <- "soreg"
    owner <- rapbase::getUserName(session)
    interval <- strsplit(input$subscriptionFreq, "-")[[1]][2]
    intervalName <- strsplit(input$subscriptionFreq, "-")[[1]][1]
    runDayOfYear <- rapbase::makeRunDayOfYearSequence(
      interval = interval)
    email <- rapbase::getUserEmail(session)
    organization <- rapbase::getUserReshId(session)

    if (input$subscriptionRep == "Samlerapport1") {
      synopsis <- "Automatisk samlerapport1"
      fun <- "samlerapport1Fun"
      paramNames <- c("p1", "p2")
      paramValues <- c("Alder", 1)
    }
    if (input$subscriptionRep == "Samlerapport2") {
      synopsis <- "Automatisk samlerapport2"
      fun <- "samlerapport2Fun"
      paramNames <- c("p1", "p2")
      paramValues <- c("BMI", 2)
    }
    rapbase::createAutoReport(synopsis = synopsis, package = package,
                              fun = fun, paramNames = paramNames,
                              paramValues = paramValues, owner = owner,
                              email = email, organization = organization,
                              runDayOfYear = runDayOfYear,
                              interval = interval, intervalName = intervalName)
    rv$subscriptionTab <- rapbase::makeUserSubscriptionTab(session)
  })
  #----------------------------------------------------------------------------80
  ## slett eksisterende abonnement
  observeEvent(input$del_button, {
    selectedRepId <- strsplit(input$del_button, "_")[[1]][2]
    rapbase::deleteAutoReport(selectedRepId)
    rv$subscriptionTab <- rapbase::makeUserSubscriptionTab(session)
  })

  # Metadata
  meta <- reactive({
    soreg::describe_db(registry_name)
  })

  output$meta_control <- renderUI({
    tabs <- names(meta())
    selectInput("meta_tab", "Velg tabell:", tabs)
  })

  output$meta_table <- DT::renderDataTable(
    meta()[[input$meta_tab]], rownames = FALSE,
    options = list(lengthMenu=c(25, 50, 100, 200, 400))
  )

  output$meta_data <- renderUI({
    DT::dataTableOutput("meta_table")
  })
}

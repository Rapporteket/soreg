library(shiny)
library(shinyalert)
library(shinyWidgets)
library(magrittr)
library(soreg)
library(lubridate)
library(tidyverse)

server <- function(input, output, session) {

  # Faste verdier for applikasjonen
  registry_name <- "soreg"

  # Last inn data
  # Legg til info om operasjonsår og primæroperasjon
   d_full <- soreg::get_arsrp("soreg")
    d_full %<>% dplyr::mutate(op_aar = lubridate::year(Operasjonsdato), op_primar = (TidlFedmeOp==0))
  # Rekn ut BMI for alle tidspunkta
   d_full <- d_full %>%
    dplyr::mutate(bmi_baseline = BR_Vekt/(BR_Hoyde/100)^2,
                  bmi_op = OperasjonVekt/(BR_Hoyde/100)^2,
                  bmi_6v = `6U_Vekt`/(`6U_Hoyde`/100)^2,
                  bmi_1a = `1Aar_Vekt`/(`1Aar_Hoyde`/100)^2,
                  bmi_2a = `ToAar_Vekt`/(`ToAar_Hoyde`/100)^2)
  # d_prim = d %>% filter(op_primar)
   d_prim <- d_full %>% dplyr::filter(op_primar)
  # I nokre analysar ser me berre på dei som har 6-vekesoppfølging
  # registrert. Hentar ut eiga datasett for desse
   d_prim_6v <- d_prim %>% dplyr::filter(`6U_KontrollType` %in% 1:3)
  # tidsinterval 
  min_dato <-min(d_full$Operasjonsdato)
  max_dato <-max(d_full$Operasjonsdato)
  fyrstAar<-year(min_dato)
  sistAar<-year(max_dato)

 # funksjon for å regne ut kvalitetsindikatoren
  # definert for liggetid (andel pasienter med 3 dager eller færre)
  ki_liggetid = function(df){

    teljar = sum(df$LiggeDogn <= 3)
    nemnar = nrow(df)

    ind = teljar/nemnar

    res = tibble::tibble(# forklaring="Del pasienter med 3 eller færre liggedøgn",
      # ltx_tekst = "Tre/færre liggedøgn",
      teljar, nemnar, ind = ind) # trenger ikke å definere at
    # variabler som er med i en group_by
    # skal være med, de kommer uansett
    res
  }

  # Kor mange låg mindre enn fire døgn
  p_kortligg = ki_liggetid(d_prim_6v)$ind

  # Kor mange låg mindre enn fire døgn per sjukehus
  d_kortligg_sjuk = d_prim_6v %>%
    dplyr::group_by(OperererendeSykehus, op_aar) %>%
    dplyr::do(ki_liggetid(.)) %>%
    dplyr::arrange(desc(ind))

  kortligg_sh <- function(sh) {d_kortligg_sjuk %>% dplyr::filter(OperererendeSykehus %in% sh)}
  kortligg_yr <- function(yr) {d_kortligg_sjuk %>% dplyr::filter(op_aar %in% yr)}
  kortligg    <- function(sh,yr){d_kortligg_sjuk %>% dplyr::filter(OperererendeSykehus %in% sh, op_aar %in% yr)}



 
  # rapporteringsaar <- 2019
  # dato_opptilrapaar <- lubridate::as_date(paste0(rapporteringsaar, "-12-31")) # For ev. seinare bruk i teksten
  # d_opptilrapaar <- d_full %>%
    # filter(Operasjonsdato < (dato_opptilrapaar + 1))

  # # Data for alle år opptil (og inkludert) to år før rapporteringsåret,
  # # dvs. alle data me i teorien burde ha toårs oppfølgingsdata på
  # dato_toaarsdata <- lubridate::as_date(paste0(rapporteringsaar - 2, "-12-31")) # For ev. seinare bruk i teksten
  # d_toaarsdata <- d_full %>%
    # dplyr::filter(Operasjonsdato < (dato_toaarsdata + 1))

  # # Og tilsvarande for eittårsdata
  # dato_eittaarsdata <- lubridate::as_date(paste0(rapporteringsaar - 1, "-12-31")) # For ev. seinare bruk i teksten
  # d_eittaarsdata <- d_full %>%
    # dplyr::filter(Operasjonsdato < (dato_eittaarsdata + 1))

  # # Dei som vart operert det aktuelle året
  # d <- d_full %>%  dplyr::filter(op_aar == rapporteringsaar)

  # # Talet på operasjonsmåtar
  # n_op <- nrow(d_full)
  # n_pas <- dplyr::n_distinct(d$PasientID)

  # d_toaarsdata_prim = d_toaarsdata %>% dplyr::filter(op_primar)
  # d_eittaarsdata_prim <- d_eittaarsdata %>% dplyr::filter(op_primar)
  # maksdogn_vis <- 14

  # # Talet på primæroperasjonar og reoperasjonar
  # n_prim = nrow(d_prim)
  # n_reop = n_op - n_prim
  # # Kva var den vanlegaste talet døgn å ligga etter operasjon
  # liggedogn_typetal = d_prim_6v %>%
    # dplyr::count(LiggeDogn) %>%
    # dplyr::arrange(desc(n)) %$%
    # LiggeDogn %>%
    # dplyr::first()

 
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
  dm <- dmx %>%
    dplyr::mutate(et_nor_m = nitti_m(dag = Operasjonsdato),
                  et_nor_p = nitti_p(dag = Operasjonsdato),
                  to_nor_m = nitti_m(yr = 2, dag= Operasjonsdato),
                  to_nor_p = nitti_p(yr = 2, dag= Operasjonsdato),
                  pTWL = 100 * (BR_Vekt - `ToAar_Vekt`)/BR_Vekt )

  dn <- dm %>%
    dplyr::mutate(et_nt= EttAar_Oppfolgingsdato %within% interval( et_nor_m, et_nor_p ),
                       to_nt= ToAar_Oppfolgingsdato %within% interval( to_nor_m, to_nor_p ),
                       et_b4= EttAar_Oppfolgingsdato %within% interval( Operasjonsdato+1, et_nor_m-1 ),
                       et_lt= EttAar_Oppfolgingsdato > et_nor_p,
                       to_b4= ToAar_Oppfolgingsdato %within% interval( Operasjonsdato+1, to_nor_m-1 ),
                       to_lt= ToAar_Oppfolgingsdato > to_nor_p)
  #                     fs1_gj_kt = `1Aar_OppfolgingsType` == 4 ,
  #                     fs1_u_kt  = `1Aar_OppfolgingsType` == 5 ,
  #                     fs2_gj_kt = `2Aar_OppfolgingsType` == 4 ,
  #                     fs2_u_kt  =  `2Aar_OppfolgingsType` == 5  )

  dt    <-  dn %>% select(  c("PasientID", "OperererendeSykehus", "Operasjonsdato", "op_aar", "Operasjonsmetode", "Opmetode_GBP",
                              "et_b4", "et_nt", "et_lt", "to_b4",  "to_nt", "to_lt", "pTWL"))

  ##---------------

 
    #------------------------------------------------------------------------------

  # Gjenbrukbar funksjon for å bearbeide Rmd til html
  htmlRenderRmd <- function(srcFile, params = list()) {
    # set param needed for report meta processing
    # params <- list(tableFormat="html")
    system.file(srcFile, package="soreg") %>%
      knitr::knit() %>%
      markdown::markdownToHTML(.,
                               options = c('fragment_only',
                                           'base64_images',
                                           'highlight_code')) %>%
      shiny::HTML()
  }

  # widget
  output$appUserName <- renderText(getUserFullName(session))
  output$appOrgName <- renderText(getUserReshId(session))

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

  # Figur og tabell

  ## Figur
  output$PlotKI1 <- renderPlot({
   soreg::makeHist(df = d_full, var = input$vrb, bins = input$bn)
  })

  ## Tabell
  output$TableKI1 <- renderTable({
   soreg::makeHist(df = d_full, var = input$vrb, bins = input$bn, makeTable = TRUE)
  })

 lgg <- reactive({kortligg(input$sh, input$lggar)})
 output$ligge <- renderDataTable({ lgg() })
 ## DT::dataTableOutput('ligge')

  # Samlerapport
  ## vis
  output$samlerapport <- renderUI({
    htmlRenderRmd(srcFile = "samlerapport.Rmd",
                  params = list(var = input$varS, bins = input$binsS))
  })

  ## last ned
  output$downloadSamlerapport <- downloadHandler(
    filename = function() {
      "samlerapport.html"
    },
    content = function(file) {
      srcFile <- normalizePath(system.file("samlerapport.Rmd",
                                           package = "soreg"))
      tmpFile <- "tmpSamlerapport.Rmd"
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(srcFile, tmpFile, overwrite = TRUE)
      out <- rmarkdown::render(tmpFile,
                               output_format =  rmarkdown::html_document(),
                               params = list(var = input$varS,
                                             bins = input$binsS),
                               output_dir = tempdir())
      file.rename(out, file)
    }
  )


  # Abonnement
  ## rekative verdier for å holde rede på endringer som skjer mens
  ## applikasjonen kjører
  rv <- reactiveValues(
    subscriptionTab = rapbase::makeUserSubscriptionTab(session))

  ## lag tabell over gjeldende status for abonnement
  output$activeSubscriptions <- DT::renderDataTable(
    rv$subscriptionTab, server = FALSE, escape = FALSE, selection = 'none',
    options = list(dom = 'tp', ordning = FALSE), rownames = FALSE
  )

  ## lag side som viser status for abonnement, også når det ikke finnes noen
  output$subscriptionContent <- renderUI({
    userFullName <- rapbase::getUserFullName(session)
    userEmail <- rapbase::getUserEmail(session)
    if (length(rv$subscriptionTab) == 0) {
      p(paste("Ingen aktive abonnement for", userFullName))
    } else {
      tagList(
        p(paste0("Aktive abonnement som sendes per epost til ", userFullName,
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

# library(shiny)
# library(shinyalert)
# library(shinyWidgets)
# library(magrittr)
# library(soreg)
# library(lubridate)
# library(tibble)
# library(DT)
# library(dplyr)
# library(rapbase)

server <- function(input, output, session) {

  # Faste verdier for applikasjonen
  registry_name <- "soreg"
  # Last inn data
  # Legg til info om operasjonsår og primæroperasjon
   d_full <- soreg::get_arsrp("soreg")
    d_full %<>% dplyr::mutate(op_aar = lubridate::year(Operasjonsdato), op_primar = (TidlFedmeOp==0))
    # d_prim = d %>% filter(op_primar)
   d_prim <- d_full %>% dplyr::filter(op_primar)
  # I nokre analysar ser me berre på dei som har 6-vekesoppfølging
  # registrert. Hentar ut eiga datasett for desse
   d_prim_6v <- d_prim %>% dplyr::filter(`6U_KontrollType` %in% 1:3)

  # tidsinterval
   min_dato <-min(d_full$Operasjonsdato)    ## hvordan kan disse sendes til ui?
   max_dato <-max(d_full$Operasjonsdato)
   fyrstAar<-lubridate::year(min_dato)
   sistAar <-lubridate::year(max_dato)

# LIGGEDØGN

  # Kor mange låg mindre enn fire døgn per sjukehus
  d_kortligg_sjuk <- d_prim_6v %>%
    dplyr::group_by(OperererendeSykehus, op_aar) %>%
    dplyr::do(soreg::ki_liggetid(.)) %>%
    dplyr::arrange(desc(ind))

  kortligg_sh <- function(sh) {d_kortligg_sjuk %>% dplyr::filter(OperererendeSykehus %in% sh)}
  kortligg_yr <- function(yr) {d_kortligg_sjuk %>% dplyr::filter(op_aar %in% yr)}
  kortligg    <- function(sh, yr){d_kortligg_sjuk %>% dplyr::filter(OperererendeSykehus %in% sh, op_aar %in% yr)}

# REINNLEGGELSE
# I analysar for reinnlegging ser me berre på dei som har 6-vekesoppfølging
# registrert eller som er registrert som reinnlagd til trass for at dei
# ikkje har 6-vekesoppfølging (jf. forklaringstekst for reinnleggings-
# indikatoren i årsrapporten). Hentar ut eiga datasett for desse. Men det
# viser seg at det òg er mogleg å svara «Vet ikke» (verdi 2) på om pasienten
# vart reinnlagd. Desse gjev ingen informasjon, og vert derfor òg fjerna.
d_reinn = d_prim %>% dplyr::filter(((`6U_KontrollType` %in% 1:3) |
                             (`6U_Behandling30Dager` == 1)) &
                             (`6U_Behandling30Dager` != 2))

# Tilsvarande for alvorlege komplikasjonar
d_kompl = d_prim %>% dplyr::filter((`6U_KontrollType` %in% 1:3) | (!is.na(`6U_KomplAlvorGrad`)))




# andel pasienter som får innleggelse innen 30 dager etter operasjon per sjukehus
d_innlegg30 = d_reinn %>%
              dplyr::group_by(OperererendeSykehus, op_aar) %>%
			  dplyr::do(soreg::ki_30dager(.)) %>%
			  dplyr::arrange(desc(ind))
innl30_sh <- function(sh) {d_innlegg30 %>% filter(OperererendeSykehus %in% sh)}
innl30_yr <- function(yr) {d_innlegg30 %>% filter(op_aar %in% yr)}
innl30 <- function(sh,yr){d_innlegg30 %>% filter(OperererendeSykehus %in% sh, op_aar %in% yr)}

# KOMPLIKASJONAR

# andel pasienter som får en alvorlig komplikasjon per sjukehus
d_kompl_alv_sjukehus = d_kompl %>%
                       dplyr::group_by(OperererendeSykehus,op_aar) %>%
					             dplyr::do(soreg::ki_kompl_alv(.)) %>%
					             dplyr::arrange(desc(ind))

kompl_sh <- function(sh) {d_kompl_alv_sjukehus %>% filter(OperererendeSykehus %in% sh)}
kompl_yr <- function(yr) {d_kompl_alv_sjukehus %>% filter(op_aar %in% yr)}
kompl <- function(sh, yr){d_kompl_alv_sjukehus %>% filter(OperererendeSykehus %in% sh, op_aar %in% yr)}

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

  dt    <-  dn %>% dplyr::select(  c("PasientID", "OperererendeSykehus", "Operasjonsdato", "op_aar", "Operasjonsmetode", "Opmetode_GBP",
                                   "et_b4", "et_nt", "et_lt", "to_b4",  "to_nt", "to_lt", "pTWL"))

  ##-----------#------------------------------------------------------------------------------

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

# KI1 - KI6
#-------------------------------------------------------------------------------------- KI1 figur og tabell

# lgdgn stats::
  # Viss nokon har *veldig* mange liggedøgn, vert
  # grafen uoversiktleg. Avgrensa derfor talet på
  # liggedøgn me viser grafisk.
  maksdogn_vis = 14
  d_prim_6v %<>% dplyr::mutate(liggedogn_lenge = LiggeDogn > maksdogn_vis,
                               liggedogn_trunk = base::pmin(LiggeDogn, maksdogn_vis + 1))
  n_liggedogn_lenge = base::sum(d_prim_6v$liggedogn_lenge, na.rm=TRUE)
  liggedogn_maks = base::max(d_prim_6v$LiggeDogn, na.rm=TRUE)

  # Lang figurforklaring viss ikkje pasientane
  # er med i grafen. :)
  # liggedogngraf_forklaring = paste0(
  #   "Talet på postoperative liggedøgn etter primæroperasjonar i~", rapporteringsaar,
  #   ifelse(n_liggedogn_lenge > 0, paste0(", der vi berre viser detaljar for dei med maks ",
  #                                        maksdogn_vis," postoperative liggedøgn. I tillegg var det ",
  #                                        n_liggedogn_lenge, " ", ifelse(n_liggedogn_lenge != 1, "pasientar", "pasient"), " med \\emph{meir} enn ",
  #                                        maksdogn_vis, " postoperative liggedøgn. Pasienten som låg lengst, hadde ",
  #                                        liggedogn_maks, " postoperative liggedøgn."), "."), " Basert på data frå til saman ",
  #   num(sum(!is.na(d_prim_6v$LiggeDogn))), " operasjonar ",
  #   "(berre pasientar med seksvekers oppfølging er med).")

  # lgdgn-graf
  # Vis berre dei som låg maks så lenge på grafen
  # Ta med 0 dagar berre om det finst (elles 1)
  liggedogn_breaks = base::seq(base::pmin(1, base::min(d_prim_6v$LiggeDogn, na.rm=TRUE)),
                         maksdogn_vis + 1)
  liggedogn_tekst = liggedogn_breaks
  # liggedogn_tekst[length(liggedogn_tekst)] = paste0("\u2265", maksdogn_vis + 1)

 ## lggpl <- ggplot2::ggplot(dplyr::filter(d_prim_6v, !base::is.na(LiggeDogn)),
 ##           aes(x=liggedogn_trunk, fill=liggedogn_lenge))
  #         geom_bar(stat="count", show.legend = FALSE)
   #  scale_fill_manual(values = c("FALSE"=colPrim[3], "TRUE"=colKontr)) +
   #  scale_x_continuous(breaks=liggedogn_breaks, labels = liggedogn_tekst, expand=c(0,.6)) +
   #  scale_y_continuous(expand = expand_soyle) +
   #  xlab("Liggedøgn") + ylab("Talet på\npasienter") +
   #  fjern_x

    ## Figur
  output$PlotKI1 <- renderPlot({
   soreg::makeHist(df = d_full, var = input$vrb, bins = input$bn)
  })
  # output$lgdgn <- renderPlot({
  #
  # })

  output$PlotKI2 <- renderPlot({
    soreg::makeHist(df = d_full, var = input$vrb, bins = input$bn)
  })
  output$PlotKI3 <- renderPlot({
    soreg::makeHist(df = d_full, var = input$vrb, bins = input$bn)
  })

  ## Tabell
  output$TableKI1 <- renderTable({
   soreg::makeHist(df = d_full, var = input$vrb, bins = input$bn, makeTable = TRUE)
  })
  ## Tabell
#  output$liggdogn <- renderTable({
#    DT::dataTableOutput(df = d_full, var = input$vrb, bins = input$bn, makeTable = TRUE)
#  })

 lgg <- reactive({kortligg(input$sh, input$lggar)})
 output$liggdogn <- DT::renderDataTable({ lgg() })

 innl <- reactive({innl30(input$sh, input$lggar)})
 output$reinnl <- DT::renderDataTable({ innl() })

 kmpl <- reactive({kompl(input$sh, input$lggar)})
 output$kompl <- DT::renderDataTable({ kmpl() })

 ## DT::dataTableOutput('ligge')
#--------------------------------------------------------------------------------------------------------
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

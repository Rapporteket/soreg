library(magrittr)
library(shinyWidgets)
library(soreg)
server <- function(input, output, session) {

  rapbase::appLogger(session = session, msg = "Starting Soreg application")

  # Parameters that will remain throughout the session
  ## setting values that do depend on a Rapporteket context
  if (rapbase::isRapContext()) {
    reshId <- rapbase::getUserReshId(session)
    # reshIch <- as.character(reshId)
    userFullName <- rapbase::getUserFullName(session)
    userRole <- rapbase::getUserRole(session)
    shsene <-  RESH_table("soreg")
    userHosp <- setNames(as.list(shsene$SykehusNavn), shsene$AvdRESH)
    userHsp <- RESH_to_sh(shsene, reshId)  # VV = 103091
    brkrSh <- RESH_sh(userHosp, reshId)
   # brkrSh <- "Oslo universitetssykehus"
    author <- paste0(userFullName, "/", "Rapporteket")
  } else {
    ### if need be, define your (local) values here
  }

  # Faste verdier i sesjonen
  registryName <- "soreg"

  # Gjenbrukbar funksjon for å bearbeide Rmd til html
  htmlRenderRmd <- function(srcFile, params = list()) {
    system.file(srcFile, package = "soreg") %>%
      knitr::knit() %>%
      markdown::markdownToHTML(.,
                               options = c("fragment_only",
                                           "base64_images",
                                           "highlight_code")) %>%
      shiny::HTML()
  }

  # Brukerinformasjon i menylinja (navbar)
  output$appUserName <-
    shiny::renderText(
      paste(rapbase::getUserFullName(session),
            rapbase::getUserRole(session), sep = ", "))
  output$appOrgName <- shiny::renderText(rapbase::getUserReshId(session))
  userInfo <- rapbase::howWeDealWithPersonalData(session,
                                                 callerPkg = "soreg")
  shiny::observeEvent(input$userInfo, {
    shinyalert::shinyalert(
      "Dette vet Rapporteket om deg:", userInfo,
      type = "", imageUrl = "rap/logo.svg",
      closeOnEsc = TRUE, closeOnClickOutside = TRUE,
      html = TRUE, confirmButtonText = rapbase::noOptOutOk()
    )
  })

  # Veiledning
  output$veiledning <- renderUI({
    htmlRenderRmd("veiledning.Rmd")
  })
  # #------------------ KI   # # read in data
  dFull <- soreg::get_arsrp("soreg")
  dFull <- dFull %>%
    dplyr::filter(!(OperererendeSykehus %in% c("Volvat Bergen", "Volvat Oslo",
                                               "Haraldsplass Diakonale Sykehus AS")))
  dFull %<>%
    dplyr::mutate(
      op_aar = lubridate::year(Operasjonsdato),
      op_primar = (TidlFedmeOp == 0),
      pTWL = 100 * (BR_Vekt - a2_Vekt) / BR_Vekt,
      del20 = pTWL >= 20.0,
  et_nor_m = nitti_m(yr = 1, dag =  Operasjonsdato, l = 90),
  et_nor_p = nitti_p(yr = 1, dag =  Operasjonsdato, l = 90),
  et_nt =  a1_KontrollDato %within%  lubridate::interval(et_nor_m, et_nor_p),
  to_nor_m = nitti_m(yr = 2, dag =  Operasjonsdato, l = 90),
  to_nor_p = nitti_p(yr = 2, dag =  Operasjonsdato, l = 90),
  to_nt =  a2_KontrollDato %within%  lubridate::interval(to_nor_m, to_nor_p))
  # #-------- user controls----------  hospital ------
  output$kIix <- shiny::renderUI({
    shiny::selectInput(
      inputId = "kIix",
      label = "Kvalitetsindikator:",
      choices = c("Ki1 Liggedøgn", "Ki2 Reinnlagt",
                  "Ki3 Alvorlege komplikasjonar", "Ki4 Kontroll normtid eitt år",
                  "Ki5 Kontroll normtid to år", "Ki6 Vekttap to år"))
  })
  output$uc_sh <- shiny::renderUI({
    shinyWidgets::pickerInput(
      inputId = "sh",
      label = "Vel sjukehus",
      choices =  unique(dFull$OperererendeSykehus), #unique(dFull$OpererendeRESH), #
      selected = brkrSh , #   userHsp  # "Helse Bergen",  # eget sjukehus?
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(
        actionsBox = TRUE,
        title = "Please select a hospital",
        header = "Vel sjukehus")
    )
  })
  # # -------------------------------  operation years
  output$uc_years <- shiny::renderUI({
    ## years available, hardcoded if outside known context
    if (rapbase::isRapContext()) {
      years <- soreg::data_years(registryName)
      # remove NAs if they exist (bad registry)
      years <- years[!is.na(years)]
    } else {
      years <- c("2017", "2018", "2019", "2020")
    }
    shiny::checkboxGroupInput(
      inputId = "op_aar",
      label = "Operasjonsår:",
      choices = years,
      selected = c("2017", "2019"), # 2016:2019,
      inline = TRUE)
  })
  # #--------- primæroperasjon?
  output$uc_prim <- shiny::renderUI({
    shiny::checkboxGroupInput(
      inputId = "prim",
      label = "Operasjonstype:",
     choiceNames  = list("Primæroperasjon", "Revisjonsoperasjon"),
     choiceValues = list(TRUE, FALSE),
     selected = TRUE,
     inline = TRUE
    )
  })
  # #----------- operasjonsteknikk
  output$uc_opr <- shiny::renderUI({
    shiny::checkboxGroupInput(
      inputId = "op_tech",
      label = "Operasjonsmetode:",
      choices = unique(dFull$Operasjonsmetode),
      selected = 6,
      inline = TRUE
    )
  })
  # #----------- aggregate
  output$uc_agg <- shiny::renderUI({
    shiny::checkboxInput(
      inputId = "out_aggr",
      label = "Vis valde operasjonsår separat:",
      value = FALSE)
  })

  # # -------------  OAGB
  output$uc_oagb <- shiny::renderUI({
    if (1 %in% input$op_tech) {
      shiny::checkboxGroupInput(
      inputId = "oagb",
      label = "RYGBP OAGB",
      choices = c(1, 2),
      # selected = 2,
      inline = TRUE,
      )
    } else {NULL}
  })

  # #------------- opr date interval
  # output$uc_dates <- shiny::renderUI({
  #   shiny::dateRangeInput(
  #     inputId = "dato_iv",
  #     label = "Operasjonsinterval ?",
  #     start = min(dFull$Operasjonsdato),
  #     end = max(dFull$Operasjonsdato)
  #   )
  # })

  # ................. reaktivitet
dtl <- shiny::reactive({input$out_aggr})

slc <- shiny::reactive({
    if (is.null(input$oagb))
    {soreg::slice(dFull, input$sh, input$op_aar, input$prim, input$op_tech)} else
    {soreg::siivu(dFull, input$sh, input$op_aar, input$prim, input$op_tech,
                  input$oagb)}
})

kI <- shiny::reactive({

  switch(if (is.null(input$kIix)) "Ki1 Liggedøgn" else input$kIix,
         "Ki1 Liggedøgn" = soreg::lgg_tb(slc(), dtl()),
         "Ki2 Reinnlagt" = soreg::reinn_tb(slc(), dtl()), # slc(),
         "Ki3 Alvorlege komplikasjonar" = soreg::kompl_tb(slc(), dtl()),
         "Ki4 Kontroll normtid eitt år" = soreg::aarKtrl(slc(), k = 1, dtl()),
         "Ki5 Kontroll normtid to år" = soreg::aarKtrl(slc(), k = 2, dtl()),
         "Ki6 Vekttap to år" = soreg::detail(slc(), dtl())
  )
})
#  output$Sw <- shiny::renderText({input$out_aggr})
  output$dT <- shiny::renderTable(kI()) # .... kvalitetsindikatortabeller

pl <- shiny::reactive({

  switch(if (is.null(input$kIix)) "Ki1 Liggedøgn" else input$kIix,
         "Ki1 Liggedøgn" = soreg::lgg_gr(slc()),
         "Ki2 Reinnlagt" = soreg::reinn_gr(slc(), dtl()),
         "Ki3 Alvorlege komplikasjonar" = soreg::kompl_gr(slc()),
         "Ki4 Kontroll normtid eitt år" = soreg::aar_ktr_gr(slc(), k = 1 ),
         "Ki5 Kontroll normtid to år"   = soreg::aar_ktr_gr(slc(), k = 2 ),
         "Ki6 Vekttap to år" = soreg::wlGr(soreg::detail(slc(), dtl()), dtl())
         )
})
  output$graf <- shiny::renderPlot(pl()) # .... kvalitetsindikatorgrafer
  # #------------------

# Datadump
  ## metadata fra registerdatabasen
  meta <- reactive({
    rapbase::describeRegistryDb(registryName)
  })

  ## ta ut innhold i datadump
  contentDump <- function(file, type) {
    d <- soreg::lagDDump(registryName, input$dumpDataSet,
                         fromDate = input$dumpDateRange[1],
                         toDate = input$dumpDateRange[2],
                         session = session)
    if (type == "xlsx-csv") {
      readr::write_excel_csv2(d, file)
    } else {
      readr::write_csv2(d, file)
    }
  }

  output$dumpTabControl <- renderUI({
    selectInput("dumpDataSet", "Velg datasett:", names(meta()))
  })

  output$dumpDataInfo <- renderUI({
    p(paste("Valgt for nedlasting:", input$dumpDataSet))
  })

  output$dumpDownload <- downloadHandler(
    filename = function() {
      basename(tempfile(pattern = input$dumpDataSet,
                        fileext = ".csv"))
    },
    content = function(file) {
      contentDump(file, input$dumpFormat)
    }
  )

  # Metadata
  output$metaControl <- renderUI({
    tabs <- names(meta())
    selectInput("metaTab", "Velg tabell:", tabs)
  })

  output$metaDataTable <- DT::renderDataTable(
    meta()[[input$metaTab]], rownames = FALSE,
    options = list(
      lengthMenu = c(25, 50, 100, 200, 400),
      language = list(
        lengthMenu = "Vis _MENU_ rader per side",
        search = "S\u00f8k:",
        info = "Rad _START_ til _END_ av totalt _TOTAL_",
        paginate = list(previous = "Forrige", `next` = "Neste")
      ))
  )

  output$metaData <- renderUI({
    DT::dataTableOutput("metaDataTable")
  })


  # Eksport
  rapbase::exportUCServer("soregExport", registryName)
  rapbase::exportGuideServer("soregExportGuide", registryName)
}

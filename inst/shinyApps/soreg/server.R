library(magrittr)
library(shinyWidgets)

server <- function(input, output, session) {

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
  #------------------ KI
  # read in data
  dFull <- soreg::get_arsrp("soreg")
  dFull %<>%
    dplyr::mutate(
      op_aar = lubridate::year(Operasjonsdato),
      op_primar = (TidlFedmeOp == 0))
  d_prim <- dFull %>%
    dplyr::filter(op_primar)
  d_prim_6v <- d_prim %>%
    dplyr::filter(`6U_KontrollType` %in% 1:3)
  d_ligg <- lgg_tb(d_prim_6v)
  d_innlegg30 <- reinn_tb(d_prim)
  d_kompl <- kompl_tb(d_prim)

  dTwl  <- dFull %>%
    dplyr::filter(!is.na(`ToAar_Vekt`)) %>% # pTWL at 2 year must exist!
    dplyr::mutate(
      pTWL = 100 * (BR_Vekt - `ToAar_Vekt`) / BR_Vekt) %>%
    dplyr::mutate(del20 = pTWL >= 20.0)

  d_slv <- dTwl %>%
    dplyr::filter(Operasjonsmetode == 6)
  d_gbp <- dTwl %>%
    dplyr::filter(Operasjonsmetode == 1, Opmetode_GBP == 1)
  d_oa <- dTwl %>%
    dplyr::filter(Operasjonsmetode == 1, Opmetode_GBP == 2)


  #-------- user controls----------  hospital ------
  output$kIix <- shiny::renderUI({
    shiny::selectInput(
      inputId = "kIix",
      label = "Kvalitetsindikator:",
      choices = c("KI1", "KI2", "KI3", "KI4", "KI5", "KI6"))
  })
  output$uc_sh <- shiny::renderUI({
    shinyWidgets::pickerInput(
      inputId = "sh",
      label = "velg sjukehus",
      choices = (unique(dFull$OperererendeSykehus)),
      selected = "Testsjukhus Norge",
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(
        actionsBox = TRUE,
        title = "Please select a hospital",
        header = "This is a list of hospitals")
    )
  })
  # -------------------------------  operation years
  output$uc_years <- shiny::renderUI({
    ## years available, hardcoded if outside known context
    if (rapbase::isRapContext()) {
      years <- soreg::data_years(registryName)
      # remove NAs if they exist (bad registry)
      years <- years[!is.na(years)]
    } else {
      years <- c("2016", "2017", "2018", "2019", "2020")
    }
    shiny::checkboxGroupInput(
      inputId = "op_aar",
      label = "År:",
      choices = years,
      selected = 2015:2018)
  })
  #--------- primæroperasjon?
  output$uc_prim <- shiny::renderUI({
    shiny::checkboxGroupInput(
      inputId = "prim",
      label = "Primæaroperasjon ?",
      choices = unique(dFull$op_primar),
      selected = TRUE
    )
  })
  #----------- operasjonsteknikk
  output$uc_opr <- shiny::renderUI({
    shiny::checkboxGroupInput(
      inputId = "op_tech",
      label = "Operasjonsteknikk",
      choices = unique(dFull$Operasjonsmetode),
      selected = 6
    )
  })
  # -------------  OAGB
  output$uc_oagb <- shiny::renderUI({
     shiny::conditionalPanel(
       condition = "input.op_tech == 1", #  "`1` %in% input.op_tech",
       shiny::checkboxGroupInput(
         inputId = "oagb",
         label = "OAGB GBP",
         choices = c(1, 2),
         selected = 2)
      )
  })
  #------------- opr date interval
  output$uc_dates <- shiny::renderUI({
    shiny::dateRangeInput(
      inputId = "dato_iv",
      label = "Operasjonsinterval ?",
      start = min(dFull$Operasjonsdato),
      end = max(dFull$Operasjonsdato)
    )
  })
  # liggedøgn
  # .................
  kI <- shiny::reactive({
    switch(if (is.null(input$kIix)) "KI1" else input$kIix,
           "KI1" = soreg::lgg_tb(
             soreg::slice(dFull, input$sh, input$op_aar, input$prim,
                          input$op_tech)), # input$dato_iv
           "KI2" = soreg::reinn_tb(
             soreg::snitt(dFull, input$sh, input$op_aar)),
           "KI3" = soreg::kompl_tb(
             soreg::snitt(dFull, input$sh, input$op_aar)),
           "KI4" = soreg::aarKtrl(
             soreg::snitt(dFull, input$sh, input$op_aar), k = 1),
           "KI5" = soreg::aarKtrl(
             soreg::snitt(dFull, input$sh, input$op_aar), k = 2),
           "KI6" = soreg::twlTb(
             soreg::snitt(dFull, input$sh, input$op_aar),
             opr_tp = input$op_tech,
             opr_oa = input$oagb)
    )
  })

  output$dT <- shiny::renderTable(kI())

  pl <- shiny::reactive({
    switch(if (is.null(input$kIix)) "KI1" else input$kIix,
           "KI1" = soreg::lgg_gr(
             soreg::slice(dFull, input$sh, input$op_aar, input$prim,
                          input$op_tech)),
           "KI3" = soreg::kompl_gr(
             soreg::snitt(dFull, input$sh, input$op_aar)),
           "KI4" = soreg::aar_ktr_tb(
             soreg::snitt(dFull, input$sh, input$op_aar), k = 1),
           "KI6" = soreg::twlGr(
             soreg::snitt(dTwl, input$sh, input$op_aar),
             input$op_tech, input$oagb)
    )
  })
  output$graf <- shiny::renderPlot(pl())
  #------------------

  # Datadump
  ## metadata fra registerdatabasen
  meta <- reactive({
    rapbase::describeRegistryDb(registryName)
  })

  ## ta ut innhold i datadump
  contentDump <- function(file, type) {
    d <- soreg::getDataDump(registryName, input$dumpDataSet,
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
}

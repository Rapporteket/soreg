library(magrittr)
# library(DT)
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
  d_full <- soreg::get_arsrp("soreg")
  d_full %<>% dplyr::mutate(
    op_aar = lubridate::year(Operasjonsdato),
    op_primar = (TidlFedmeOp == 0))
  d_prim <- d_full %>% dplyr::filter(op_primar)
  d_prim_6v <- d_prim %>% dplyr::filter(`6U_KontrollType` %in% 1:3)
  d_ligg <- lgg_tb(d_prim_6v)
  d_innlegg30 <-  reinn_tb(d_prim)
  d_kompl <- kompl_tb(d_prim)
  # d_k1 <- aar_ktr_tb(d_prim, k=1)  # Column `et_b4` doesn't exist.
  # d_k2 <- aar_ktr_tb(d_prim, k=2)  #
#-------- user controls----------  hospital ------
  output$kI_ix <- shiny::renderUI({
    shiny::selectInput(
      inputId = "kI_ix",
      label = "Kvalitetsindikator:",
      choices = c("KI1", "KI2", "KI3", "KI4", "KI5", "KI6"))
  })
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
# -------------------------------  operation years
  output$uc_years <- renderUI({
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
  output$uc_prim <- renderUI({
    shiny::checkboxGroupInput(
      inputId = "prim",
      label = "Primæaroperasjon ?",
      choices = unique(d_full$op_primar),
      selected = TRUE
    )
  })
#----------- operasjonsteknikk
  output$uc_opr <- renderUI({
    shiny::checkboxGroupInput(
      inputId = "op_tech",
      label = "Operasjonsteknikk",
      choices = unique(d_full$Operasjonsmetode),
      selected = 6
    )
  })
#------------- opr date interval
  output$uc_dates <- renderUI({
    shiny::dateRangeInput(
      inputId = "dato_iv",
      label = "Operasjonsinterval ?",
      start = min(d_full$Operasjonsdato),
      end = max(d_full$Operasjonsdato)
    )
  })
# liggedøgn
# .................
kI <- reactive({
  # snitt(d_innlegg30, input$sh, input$op_aar)
  base::switch(input$kI_ix,
               "KI1" = lgg_tb(slice(d_full, input$sh, input$op_aar, input$prim, input$op_tech)),
               "KI3" = kompl_tb(snitt(d_full, input$sh, input$op_aar )),
               "KI4" = aar_ktr_tb(snitt(d_full, input$sh, input$op_aar ), k = 1),
               "KI4" = aar_ktr_tb(snitt(d_full, input$sh, input$op_aar ), k = 2)
               )

  #  slice(d_full, input$sh, input$op_aar, input$prim, input$op_tech)
  # switch(input$kIix,         "KI2" =   )
  })

  output$dT <- renderTable({  kI() })

  pl <- reactive({
    base::switch(input$kI_ix,
           "KI1" = lgg_gr(slice(d_full, input$sh, input$op_aar, input$prim, input$op_tech)),
           "KI3" = kompl_gr(snitt(d_full, input$sh, input$op_aar )),
           "KI4" = aar_ktr_tb(snitt(d_full, input$sh, input$op_aar ), k = 1)
           )
   # lgg_gr(slice(d_full, input$sh, input$op_aar, input$prim, input$op_tech))
    })
  output$graf <- renderPlot({ pl() })
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

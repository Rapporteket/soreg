addResourcePath("rap", system.file("www", package="rapbase"))
regTitle = "SoReg"

ui <- shiny::tagList(
  shiny::navbarPage(
    title = shiny::div(a(includeHTML(system.file("www/logo.svg", package="rapbase"))),
                regTitle),
    windowTitle = regTitle,
    theme = "rap/bootstrap.css",
    id = "tabs",
    shiny::tabPanel("Start",
      mainPanel(
        width = 12,
        shiny::htmlOutput("veiledning", inline = TRUE),
        shinyalert::useShinyalert(),
        rapbase::appNavbarUserWidget(
          user = uiOutput("appUserName"),
          organization = uiOutput("appOrgName"),
          addUserInfo = TRUE
        )
      )
    ),
    shiny::tabPanel("Kvalitetsindikatorer",
      shiny::sidebarLayout(
        shiny::sidebarPanel(width = 3,
          shiny::selectInput(
            inputId = KI_ix,
            label = "Kvalitetsindikator:",
            choices = c("KI1","KI2","KI3","KI4","KI5","KI6")
          ),
          shiny::uiOutput("uc_sh")
        ),
        shiny::mainPanel(width = 9,
        shiny::tabsetPanel(
          shiny::tabPanel("Figur",shiny::plotOutput("graf")),
          shiny::tabPanel("Tabell",shiny::htmlOutput("DT"))
        )                 
                         )
      )
	  ),
    shiny::tabPanel("Datadump",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          width = 4,
          uiOutput("dumpTabControl"),
          dateRangeInput("dumpDateRange", "Velg periode:",
                         start = lubridate::ymd(Sys.Date()) - lubridate::years(1),
                         end = Sys.Date(), separator = "-",
                         weekstart = 1),
          radioButtons("dumpFormat", "Velg filformat:",
                       choices = list(csv = "csv",
                                      `csv2 (nordisk format)` = "csv2",
                                      `xlsx-csv` = "xlsx-csv",
                                      `xlsx-csv2 (nordisk format)` = "xlsx-csv2")),
          downloadButton("dumpDownload", "Hent!")
        ),
        shiny::mainPanel(
          htmlOutput("dumpDataInfo")
        )
      )
    )
  ) # navbarPage
) # tagList

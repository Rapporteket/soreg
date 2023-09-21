addResourcePath("rap", system.file("www", package = "rapbase"))
regTitle <- "SoReg"

ui <- shiny::tagList(
  shiny::navbarPage(
    title = shiny::div(a(includeHTML(system.file("www/logo.svg",
                                                 package = "rapbase"))),
                       regTitle),
    windowTitle = regTitle,
    theme = "rap/bootstrap.css",
    id = "tabs",
    shiny::tabPanel(
      "Start",
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
    shiny::tabPanel(
      "Kvalitetsindikatorer",
      #   shiny::sidebarLayout(
      #     shiny::sidebarPanel(
      #       width = 3,
      shiny::uiOutput("kIix"),
      shiny::uiOutput("uc_sh"),
      shiny::uiOutput("uc_years"),
      shiny::uiOutput("uc_agg"),
      shiny::uiOutput("uc_prim"),
      shiny::uiOutput("uc_opr"),
      shiny::uiOutput("uc_oagb"), #
      #        shiny::uiOutput("uc_dates")
      #     ),
      shiny::mainPanel(
        width = 9,
        shiny::tabsetPanel(
          # shiny::tabPanel("AG", shiny::uiOutput("Sw")),
          shiny::tabPanel("Tabell",   shiny::uiOutput("dT")),
          shiny::tabPanel("Figur", plotly::plotlyOutput("graf"))
        )
      )
      #   )
    ),
    shiny::tabPanel(
      "Datadump",
        uiOutput("dumpTabControl")
    ),
    tabPanel(
      "Metadata",
      sidebarLayout(
        sidebarPanel(uiOutput("metaControl")),
        mainPanel(htmlOutput("metaData"))
      )
    ),
    shiny::tabPanel(
      "Eksport",
      shiny::sidebarLayout(
        shiny::sidebarPanel(rapbase::exportUCInput("soregExport")),
        shiny::mainPanel(rapbase::exportGuideUI("soregExportGuide"))
      )
    )
  ) # navbarPage
) # tagList

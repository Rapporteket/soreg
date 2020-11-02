library(shiny)
library(shinyalert)
library(rapbase)

addResourcePath('rap', system.file('www', package='rapbase'))
regTitle = "SoReg"

ui <- tagList(
  navbarPage(
    title = div(a(includeHTML(system.file('www/logo.svg', package='rapbase'))),
                regTitle),
    windowTitle = regTitle,
    theme = "rap/bootstrap.css",

    tabPanel("Start",
      mainPanel(width = 12,
        htmlOutput("veiledning", inline = TRUE),
        shinyalert::useShinyalert(),
        appNavbarUserWidget(user = uiOutput("appUserName"),
                            organization = uiOutput("appOrgName"),
                            addUserInfo = TRUE),
        tags$head(tags$link(rel="shortcut icon", href="rap/favicon.ico"))
      )
    ),
    tabPanel("Figur og tabell"
      ,
      sidebarLayout(
        sidebarPanel(width = 3,
          selectInput(inputId = "var",
                      label = "Variabel:",
                      c("mpg", "disp", "hp", "drat", "wt", "qsec")),
          sliderInput(inputId = "bins",
                      label = "Antall grupper:",
                      min = 1,
                      max = 10,
                      value = 5)
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Figur", plotOutput("distPlot")),
            tabPanel("Tabell", tableOutput("distTable"))
          )
        )
      )
    ),
    tabPanel("Samlerapport"
        ,
        tabPanel("Fordeling av mpg",
          sidebarLayout(
            sidebarPanel(width = 3,
              selectInput(inputId = "varS",
                          label = "Variabel:",
                          c("mpg", "disp", "hp", "drat", "wt", "qsec")),
              sliderInput(inputId = "binsS",
                          label = "Antall grupper:",
                          min = 1,
                          max = 10,
                          value = 5),
              downloadButton("downloadSamlerapport", "Last ned!")
            ),
            mainPanel(
              uiOutput("samlerapport")
            )
          )
        )
      ),
    tabPanel("Abonnement"
      ,
      sidebarLayout(
        sidebarPanel(width = 3,
          selectInput("subscriptionRep", "Rapport:", c("Samlerapport1", "Samlerapport2")),
          selectInput("subscriptionFreq", "Frekvens:",
                      list('\u00c5rlig'="årlig-year",
                           Kvartalsvis="kvartalsvis-quarter",
                           'M\u00e5nedlig'="månedlig-month",
                           Ukentlig="ukentlig-week",
                           Daglig="daglig-DSTday"),
                      selected = "månedlig-month"),
          actionButton("subscribe", "Bestill!")
        ),
        mainPanel(
          uiOutput("subscriptionContent")
        )
      )
    ),

    tabPanel("Metadata",
             sidebarLayout(
               sidebarPanel(uiOutput("meta_control")),
               mainPanel(htmlOutput("meta_data"))
             )
    )
  ) # navbarPage
) # tagList

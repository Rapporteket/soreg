library(shiny)
library(shinyalert)
library(shinyWidgets)
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
    tabPanel("KI1: Liggedøgn",
             sidebarLayout(
               sidebarPanel(width=2,
                            pickerInput(
                              inputId = "sh",
                              label = "velg sjukehus",
                              choices = unique(dt$OperererendeSykehus),
                              multiple = TRUE,
                              options = pickerOptions(
                                actionsBox = TRUE,
                                title = "Please select a hospital",
                                header = "This is a list of hospitals"
                              )),
                            checkboxGroupInput(inputId = "aar", label ="år", choices=fyrstAar:sistAar)  # multiple?

                            ),
                           mainPanel(          tabsetPanel(
                             tabPanel("Figur", plotOutput("PlotKI1")),
                             tabPanel("Tabell", tableOutput("TableKI"))
                           )
                           ))
             ),
    ##---------------------
    pickerInput(
      inputId = "sh",
      label = "velg sjukehus",
      choices = unique(dt$OperererendeSykehus),
      multiple = TRUE,
      options = pickerOptions(
        actionsBox = TRUE,
        title = "Please select a hospital",
        header = "This is a list of hospitals"
      )
    ),
    # sliderInput("ixs", label ="år", min=fyrstAar, max=sistAar, value=c(2017,2018), step=1)
    checkboxGroupInput(inputId = "aar", label ="år", choices=fyrstAar:sistAar) , # multiple?


    # radioButtons("år",label ="years", fyrstAar:sistAar)  # multiple?
    # selectInput("sykeh", label="sykehus", unique(dt$OperererendeSykehus),multiple=TRUE)
    # selectInput("n_breaks", label = "Number of bins:",
    #            choices = c(10, 20, 35, 50), selected = 20)
    dateRangeInput('dateRange',
                   label = 'Datointerval: yyyy-mm-dd',
                   start = min_dato, end = max_dato
    ),

    ##--------------------
    tabPanel("KI2",
             sidebarLayout(
               sidebarPanel(width=2,
                            selectInput(inputId = "sh",
                                        label = "sjukehus: ",
                                        c("Helse Bergen","Helse Stavanger"))
               ),
               mainPanel(          tabsetPanel(
                 tabPanel("Figur", plotOutput("PlotKI2")),
                 tabPanel("Tabell", tableOutput("TableKI2"))
               )
               ))
    ),
    tabPanel("KI3",
             sidebarLayout(
               sidebarPanel(width=2,
                            selectInput(inputId = "sh",
                                        label = "sjukehus: ",
                                        c("Helse Bergen","Helse Stavanger"))
               ),
               mainPanel(          tabsetPanel(
                 tabPanel("Figur", plotOutput("distPlot2")),
                 tabPanel("Tabell", tableOutput("distTable2"))
               )
               ))
    ),
    tabPanel("KI og tabell",
      sidebarLayout(
        sidebarPanel(width = 3,
          selectInput(inputId = "varavn",
                      label = "Variabel:",
                      c("b_ant_vekt", "b_ant_hoyde", "b_ant_kmi")),
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

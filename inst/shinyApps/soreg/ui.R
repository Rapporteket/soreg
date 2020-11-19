library(shiny)
library(shinyalert)
library(shinyWidgets)
# library(rapbase)
# library(DT)

addResourcePath('rap', system.file('www', package='rapbase'))
regTitle = "SoReg"

ui <- tagList(
    navbarPage(
    title = div(a(includeHTML(system.file('www/logo.svg', package='rapbase'))),
                regTitle),
    windowTitle = regTitle,
    theme = "rap/bootstrap.css",
#-----------------------------------------------------------------------------------
	tabPanel("Start",
      mainPanel(width = 12,
        htmlOutput("veiledning", inline = TRUE),
        shinyalert::useShinyalert(),
        rapbase::appNavbarUserWidget(user = uiOutput("appUserName"),
                            organization = uiOutput("appOrgName"),
                            addUserInfo = TRUE),
        tags$head(tags$link(rel="shortcut icon", href="rap/favicon.ico"))
      )
    ),
#------------------------------------------------------ KI1 - KI6
    tabPanel("KI1: Liggedøgn",
	    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(width=3,
                   pickerInput(
                     inputId = "sh",
                     label = "velg sjukehus",
                     choices =  c("Helse Bergen","Helse Stavanger",
                                  "Testsjukhus Norge"),   # unique(d_full$OperererendeSykehus),  #
                     selected = "Testsjukhus Norge",
                     multiple = TRUE,
                     options = pickerOptions(
                       actionsBox = TRUE,
                       title = "Please select a hospital",
                       header = "This is a list of hospitals"
                     )),
                   checkboxGroupInput(inputId = "lggar",
                                      label ="år",
                                      choices = 2014:2020,
                                      selected = 2016:2018),
                   dateRangeInput("dato_iv",
                                  "Operasjonsinterval?",
                                  start = min_dato,
                                  end = max_dato),
                   selectInput(inputId= "vrb",
                               label = "Variabel:",
                               c("BR_BMI", "PasientAlder")),
                   sliderInput("bn",  label = "Antall grupper:",
                               min = 1, max = 10,
                               value = 5)
      ),
        # sidebarPanel(
        #     sliderInput("bins",
        #                 "Number of bins:",
        #                 min = 1,
        #                 max = 50,
        #                 value = 30),
        #     selectInput("dataset",
        #                 label = "Dataset",
        #                 choices = ls("package:datasets")),
        #     numericInput("num",
        #                  "Number one",
        #                  value = 0,
        #                  min = 0,
        #                  max = 100),
        #     checkboxGroupInput("opr_aar",
        #                        "Opr År",
        #                        choices = 2014:2020)

        # MailPanel
        mainPanel(
            navbarPage("Rapporteket",
          navbarMenu("KI1: Liggedøgn",
           tabPanel("graf", plotOutput("PlotKI1")),
           tabPanel("tabell", DT::dataTableOutput("liggdogn"))),
          navbarMenu("KI2: Reinnleggelse",
           tabPanel("graf", plotOutput("PlotKI2")),
           tabPanel("tabell", DT::dataTableOutput("reinnl"))),
          navbarMenu("KI3: Komplikasjonar",
           tabPanel("graf", plotOutput("PlotKI3")),
           tabPanel("tabell", DT::dataTableOutput("kompl"))),
           tabPanel("KI4: 1-årskrl. nt.", plotOutput("dist4")),
           tabPanel("KI5: 2-årskrl. nt.", plotOutput("dist5")),
           tabPanel("KI6: Vekttap >= 20%", plotOutput("dist6"))
            ),
           navbarPage("soreg-stuff here",
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
                      tabPanel("Abonnement",
                               sidebarLayout(
                          sidebarPanel(width = 3,
                                       selectInput("subscriptionRep",
                                                   "Rapport:",
                                                   c("Samlerapport1", "Samlerapport2")),
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
                      )),
                      tabPanel("Metadata",
                               sidebarLayout(
                                sidebarPanel(uiOutput("meta_control")),
                                mainPanel(htmlOutput("meta_data"))
                               )
                      )
                      ) #navbarPage
        ) #mainPanel
    ),  #sidebarLayout


 #-------------------------------------------------------- KI og tabell kan fjernes?
 )
  ) # navbarPage
) # tagList

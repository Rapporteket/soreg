library(shiny)
library(shinyalert)
library(shinyWidgets)
# library(rapbase)
# library(DT)

addResourcePath("rap", system.file("www", package="rapbase"))
regTitle = "SoReg"

ui <- tagList(
    navbarPage(
    title = div(a(includeHTML(system.file("www/logo.svg", package="rapbase"))),
                regTitle),
    windowTitle = regTitle,
    theme = "rap/bootstrap.css",
#----------------------------------------------------------------------------80
tabPanel("Start",
  mainPanel(width = 12,
    htmlOutput("veiledning", inline = TRUE),
      shinyalert::useShinyalert(),
      rapbase::appNavbarUserWidget(
        user = uiOutput("appUserName"),
        organization = uiOutput("appOrgName"),
        addUserInfo = TRUE
        ),
        tags$head(tags$link(rel="shortcut icon", href="rap/favicon.ico"))
      )
        ),
#------------------------------------------------------ KI1 - KI6
navbarMenu("Kvalitetindikatorar",
 tabPanel("KI1",
          navbarMenu("KI1: Liggedøgn",
                     tabPanel("graf", plotOutput("lggpl")),
                     tabPanel("tabell", DT::dataTableOutput("liggdogn")) ),
 tabPanel("KI2",),
 tabPanel("KI3",),
 tabPanel("KI4",),
 tabPanel("KI5",),
 tabPanel("KI6",),
),

tabPanel("KI1: Liggedøgn",
	    # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(width=3,
      pickerInput(
        inputId = "sh",
        label = "velg sjukehus",
        choices =  c("Helse Bergen","Helse Stavanger", "Testsjukhus Norge"),
        # unique(d_full$OperererendeSykehus),  #
      selected = "Testsjukhus Norge",
      multiple = TRUE,
      options = pickerOptions
      (
       actionsBox = TRUE,
       title = "Please select a hospital",
       header = "This is a list of hospitals"
      )
      ),
  checkboxGroupInput(
    inputId = "lggar",
    label ="år",
    choices = 2014:2020,
    selected = 2016:2018),
  dateRangeInput(
    "dato_iv",
    "Operasjonsinterval?",
    start = min_dato,
    end = max_dato),
  checkboxGroupInput(
    "op_tech",
    "Operasjonsteknikk",
    choices = 1:15,
    selected = 6),
  selectInput(
    inputId = "vrb",
    label = "Variabel:",
    c("BR_BMI", "PasientAlder")),
  sliderInput
  (
    inputId = "bn",
    label = "Antall grupper:",
    min = 1, max = 10, value = 5
  )
    ),
# MailPanel
    mainPanel(
      navbarPage("Rapporteket",
    navbarMenu("KI2: Reinnleggelse",
      tabPanel("graf", plotOutput("PlotKI2")),
      tabPanel("tabell", DT::dataTableOutput("reinnl")) ),
    navbarMenu("KI3: Komplikasjonar",
      tabPanel("graf", plotOutput("PlotKI3")),
      tabPanel("tabell", DT::dataTableOutput("kompl")) ),
      tabPanel("KI4: 1-årskrl. nt.", plotOutput("dist4")),
      tabPanel("KI5: 2-årskrl. nt.", plotOutput("dist5")),
      tabPanel("KI6: Vekttap >= 20%", plotOutput("dist6")) ),

#----------------------------------------------------------------------------80

   # ) #navbarPage
  ) #mainPanel
),  #sidebarLayout
#----------------------------------------------------------------------------80

    )
  ),

tabPanel("Samlerapport",
         tabPanel("Fordeling av mpg",
                  sidebarLayout(
                    sidebarPanel(width = 3,
                                 selectInput(
                                   inputId = "varS",
                                   label = "Variabel:",
                                   c("mpg", "disp", "hp", "drat", "wt", "qsec")),
                                 sliderInput(
                                   inputId = "binsS",
                                   label = "Antall grupper:",
                                   min = 1, max = 10, value = 5),
                                 downloadButton("downloadSamlerapport", "Last ned!") ),
                    mainPanel(uiOutput("samlerapport"))))
),
# navbarPage("soreg-stuff here",
tabPanel("Abonnement",
         sidebarLayout(
           sidebarPanel(width = 3,
                        selectInput(
                          inputId = "subscriptionRep",
                          label = "Rapport:",
                          c("Samlerapport1", "Samlerapport2")),
                        selectInput(
                          inputId = "subscriptionFreq",
                          label = "Frekvens:",
                          list("\u00c5rlig" = "årlig-year",
                               Kvartalsvis = "kvartalsvis-quarter",
                               "M\u00e5nedlig" = "månedlig-month",
                               Ukentlig = "ukentlig-week",
                               Daglig = "daglig-DSTday"),
                          selected  = "månedlig-month"),
                        actionButton("subscribe", "Bestill!")
           ),
           mainPanel(uiOutput("subscriptionContent")) )),
tabPanel("Metadata",
         sidebarLayout(
           sidebarPanel(uiOutput("meta_control")),
           mainPanel(htmlOutput("meta_data")) ) ) #Metadata



    ) # navbarPage
) # tagList

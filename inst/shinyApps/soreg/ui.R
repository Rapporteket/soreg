# library(shiny)
library(shinyalert)
# library(shinyWidgets)
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
                shinyalert::useShinyalert(),
                rapbase::appNavbarUserWidget(
                  user = uiOutput("appUserName"),
                  organization = uiOutput("appOrgName"),
                  addUserInfo = TRUE
                ),
                tags$head(tags$link(rel="shortcut icon", href="rap/favicon.ico")),
                "Noe om SoReg (foreløpig feil i rendring av Rmd...)"
                #htmlOutput("veiledning", inline = TRUE)
      )
    ),
    #------------------------------------------------------ KI1 - KI6
    tabPanel("KI",
      sidebarLayout(
        sidebarPanel(width=3,
          shiny::selectInput(inputId = "KIix",
            label = "Velg indikator:",
            choices = c("KI1", "KI2", "KI3", "KI4", "KI5", "KI6")),
          shinyWidgets::pickerInput(
            inputId = "sh",
            label = "velg sjukehus",
            choices = unique(d_full$OperererendeSykehus),
                       # c("Helse Bergen","Helse Stavanger", "Testsjukhus Norge"),
            selected = "Testsjukhus Norge",
            multiple = TRUE,
            options = shinyWidgets::pickerOptions(actionsBox = TRUE,
            title = "Please select a hospital",
            header = "This is a list of hospitals")),
          shiny::uiOutput("uc_years"),
          # shiny::checkboxGroupInput(
          #   inputId = "lggar_old",
          #   label ="år",
          #   choices = 2014:2020,
          #   selected = 2016:2018),
          shiny::checkboxGroupInput(
            inputId = "op_tech",
            label = "Operasjonsteknikk",
            choices = c(1,6),
            selected = 6),
          shiny::dateRangeInput(
            inputId = "dato_iv",
            label = "Operasjonsinterval?",
            start = "2018-01-01", end = "2020-12-31"),
          shiny::selectInput(
            inputId = "vrb",
            label = "Variabel:",
            choices = c("BR_BMI", "PasientAlder")),
          shiny::sliderInput(
            inputId = "bn",
            label = "Antall grupper:",
            min = 1, max = 10, value = 5
          )),
        mainPanel(width = 9,
          shiny::tabsetPanel(
            shiny::tabPanel(" txt",shiny::textOutput("QI")),
            shiny::tabPanel("Figur", shiny::plotOutput("lggpl")),
            shiny::tabPanel("Tabell", shiny::htmlOutput("TableKI1"))))
      #                tabPanel("graf", plotOutput("lggpl")),
      #                tabPanel("tabell", DT::dataTableOutput("liggdogn")) ),
      #     navbarPage("KI2: Reinnleggelse",
      #                tabPanel("graf", plotOutput("PlotKI2")),
      #                tabPanel("tabell", DT::dataTableOutput("reinnl")) ),
      #     navbarPage("KI3: Komplikasjonar",
      #                tabPanel("graf", plotOutput("PlotKI3")),
      #                tabPanel("tabell", DT::dataTableOutput("kompl")) ),
      #     tabPanel("KI4: 1-årskrl. nt.", plotOutput("dist4")),
      #     tabPanel("KI5: 2-årskrl. nt.", plotOutput("dist5")),
      #     tabPanel("KI6: Vekttap >= 20%", plotOutput("dist6")) ),
       ), #sidebarlayout
    ), #KI

    #------------------------------------------------------ KI1 - KI6
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

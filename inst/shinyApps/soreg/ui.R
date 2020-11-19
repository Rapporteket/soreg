library(shiny)
library(shinyalert)
library(shinyWidgets)
# library(rapbase)
# library(DT)

shiny::addResourcePath("rap", system.file("www", package="rapbase"))
regTitle = "SoReg"

ui <- shiny::tagList(
  shiny::navbarPage(
    title = div(a(includeHTML(system.file('www/logo.svg', package='rapbase'))),
                regTitle),
    windowTitle = regTitle,
    theme = "rap/bootstrap.css",

    tabPanel("Start",
      mainPanel(width = 12,
                htmlOutput("veiledning", inline = TRUE),
                shinyalert::useShinyalert(),
                rapbase::appNavbarUserWidget(
                  user = uiOutput("appUserName"),
                  organization = uiOutput("appOrgName"),
                  addUserInfo = TRUE),
                tags$head(tags$link(rel="shortcut icon", href="rap/favicon.ico"))
      )
    ),

    tabPanel("Indikatorer",
      sidebarLayout(
        sidebarPanel(
          width=3,
          shiny::selectInput("select_indicator",
                             label = "Velg indikator:",
                             choices = c("k1", "k2", "k3")),
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
                         start = "2014-01-01",
                         end = "2020-12-31"),
          checkboxGroupInput("op_tech",
                             "Operasjonsteknikk",
                             choices = c("GS", "GBP", "OA"),
                             selected = "GS"),
          selectInput(inputId= "vrb",
                      label = "Variabel:",
                      c("BR_BMI", "PasientAlder")),
          sliderInput("bn",  label = "Antall grupper:",
                      min = 1, max = 10,
                      value = 5)),
        mainPanel(width = 9,
          shiny::tabsetPanel(
            shiny::tabPanel("Figur", shiny::plotOutput("PlotKI1")),
            shiny::tabPanel("Tabell", shiny::htmlOutput("TableKI1"))
          )
        )
      )
    )
  ) # navbarPage
) # tagList

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            selectInput("dataset",
                        label = "Dataset",
                        choices = ls("package:datasets")),
            numericInput("num",
                         "Number one",
                         value = 0,
                         min = 0,
                         max = 100),
            checkboxGroupInput("opr_aar",
                               "OpÅr",
                               choices = 2014:2020)
            ),
        # Show a plot of the generated distribution
        mainPanel(
            navbarPage("Rapporteket",
                       tabPanel("KI1: Liggedøgn", plotOutput("distPlot")),
                       tabPanel("KI2: Reinnleggelse", plotOutput("dist2")),
                       tabPanel("KI3: Komplikasjonar", plotOutput("dist3")),
                       tabPanel("KI4: 1-årskrl. normtid", plotOutput("dist4")),
                       tabPanel("KI5: 2-årskrl. normtid", plotOutput("dist5")),
                       tabPanel("KI6: Vekttap minst 20%", plotOutput("dist6"))
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
                      )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    }) 
    
    output$liggeTable <- renderDataTable({
        soreg::makeHist(df = d_full, var = input$vrb, bins = input$bn, makeTable = TRUE)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

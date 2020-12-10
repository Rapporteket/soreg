

smerte :: ui
tabPanel("Datadump"
         ,
         sidebarLayout(
           sidebarPanel(width = 4,
                        uiOutput("dumpTabControl"),
                        dateRangeInput("dumpDateRange", "Velg periode:",
                                       start = lubridate::ymd(Sys.Date())- years(1),
                                       end = Sys.Date(), separator = "-",
                                       weekstart = 1),
                        radioButtons("dumpFormat", "Velg filformat:",
                                     choices = list(csv = "csv",
                                                    `csv2 (nordisk format)` = "csv2",
                                                    `xlsx-csv` = "xlsx-csv",
                                                    `xlsx-csv2 (nordisk format)` = "xlsx-csv2")),
                        downloadButton("dumpDownload", "Hent!")
           ),
------------------------------------------------------------------------------------------     


smerte:: server

# Datadump
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
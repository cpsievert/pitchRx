library(shiny)
library(pitchRx)

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
  
    output$strikeFX <- reactivePlot(function() {
      if (input$method == "local") {
        data <- read.csv(input$directory, sep=input$seperator)
      } else {
        if (input$start_query) {
          raw.data <- scrapeFX(start=input$start, end=input$end, player=input$player, type=input$type)
          if (input$write_file) {
            date <- gsub(":", "-", as.character(as.POSIXct(Sys.Date())))
            write.csv(raw.data$atbat, paste("atbats", date, ".csv", sep=""))
            write.csv(raw.data$pitch, paste("pitches", date, ".csv", sep=""))
          }
          data <- join(raw.data$pitch, raw.data$atbat, by = c("num", "url"), type = "inner")
        }
      }
      print(strikeFX(data))
    })
    
})
library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("PITCHf/x Data Collection and Visualization App"),
  
  sidebarPanel(  
    #Select one of two options for uploading data
    wellPanel(
      radioButtons("method", "Choose a method:", 
        c("Enter local file" = "local", "Parse source files" = "source")
      )
    ),
    #Parameters for data collection 
    wellPanel(
      conditionalPanel(
        condition = "input.method == 'local'",
        textInput("directory", "File name (with file path):", paste(getwd())),
        radioButtons("seperator", "",
                     c("Comma-seperated" = ",", "Tab-seperated" = "\t"))
      ),
      conditionalPanel(
        condition = "input.method == 'source'",
        textInput("start", "Start Date:", "2011-07-01"),
        textInput("end", "End Date:", "2011-07-02"),
        textInput("player", "Player(s) of Interest:", "Everyone"),
        selectInput("type", "Position of interest:", choices = c("pitcher", "batter")),
        checkboxInput("write_file", strong("Write Data to Current Directory"), TRUE),
        checkboxInput("start_query", strong("Start Query"), FALSE)
      )
    )
  ),
  #Main panel with output from server
  mainPanel(
    plotOutput("strikeFX")
  )
))
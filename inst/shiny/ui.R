library(shiny)

# This app assumes one already has PITCHf/x data available to visualize (or wants to use sample data)
shinyUI(pageWithSidebar(
  
  headerPanel("PITCHf/x Visualization App"),
  
  sidebarPanel(  
    helpText(HTML("<h3>Data source</h3>")),
    radioButtons("dataSource", "",
                 c("Use sample dataset" = "sample", "Use local file" = "local", "Collect data from the source" = "source")),
    HTML("<hr />"),
    conditionalPanel(
      condition = "input.dataSource == 'sample'",
      helpText(HTML("<div style=\"text-indent: 25px\">This sample dataset contains every four-seam fastball and cutting fastball thrown by Mariano Rivera and Phil Hughes over the 2011 season.</div>"))
    ),
    conditionalPanel(
      condition = "input.dataSource == 'local'",            
      fileInput(inputId = "file", label="PITCHf/x data stored in csv format:")
    ),
    conditionalPanel(
      condition = "input.dataSource == 'source'",
      helpText(HTML("<div style=\"text-indent: 25px\">See <a href='http://cpsievert.wordpress.com/2013/01/10/easily-obtain-mlb-pitchfx-data-using-r/'>my post</a> on collecting PITCHf/x data from the source using <a href='http://cran.r-project.org/web/packages/pitchRx/'>pitchRx</a>.</div>"))
    ),
    HTML("<hr />"),
    helpText(HTML("<h3>Visualization Method</h3>")),
    radioButtons("visMethod", "",
                 c("Visualize strikezones" = "strike", 
                   "Animate flight paths" = "animate")),
    HTML("<hr />"),
    helpText(HTML("<h3>Axis Settings</h3>")),
    conditionalPanel(
      condition = "input.visMethod == 'custom'",
      uiOutput("customX"),
      uiOutput("customY")
    ),
    numericInput("xmin", "x-axis minimum:", -3.5),
    numericInput("xmax", "x-axis maximum:", 3.5),
    numericInput("ymin", "y-axis minimum", 0),
    numericInput("ymax", "y-axis maximum", 7),
    checkboxInput("coord.equal", strong("Preserve Plotting Persepective"), TRUE),
    helpText(HTML("<h3>Facetting</h3>")),
    selectInput("facet1", "Column-wise Split:", 
                choices = c("stand", "pitch_type", "pitcher_name", "top_inning", "No facet", "Enter my own")),
    conditionalPanel(
      condition = "input.facet1 == 'Enter my own'",
      textInput("facet1custom", "Type variable name here:", " ")
    ),
    selectInput("facet2", "Row-wise Split:", 
                choices = c("No facet", "pitch_type", "pitcher_name", "top_inning", "Enter my own")),
    conditionalPanel(
      condition = "input.facet2 == 'Enter my own'",
      textInput("facet2custom", "Type variable name here:", " ")
    ),
    HTML("<hr />"),
    helpText(HTML("<h3>Plotting Geometries</h3>")),
    radioButtons("geom", "",
                 c("point" = "point", 
                   "tile" = "tile",
                   "hex" = "hex",
                   "bin" = "bin")),
    wellPanel(
      conditionalPanel(
        condition = "input.geom == 'point'",
        uiOutput("pointColor"),
        sliderInput("point_alpha", "Alpha (transparency):",
                    min = 0, max = 1, value = 0.5, step = 0.1),
        sliderInput("point_size", "Size:",
                    min = 0.5, max = 8, value = 5, step = 0.5),
        checkboxInput("point_contour", strong("Add contour lines"), FALSE),
        conditionalPanel(
          condition = "input.visMethod == 'strike'",
        checkboxInput("point_adjust", strong("Adjust vertical locations to aggregate strikezone"), TRUE)
        )
      ),
      conditionalPanel(
        condition = "input.visMethod == 'strike'",
        conditionalPanel(
          condition = "input.geom == 'tile'",
          checkboxInput("tile_contour", strong("Add contour lines"), FALSE),
          checkboxInput("tile_adjust", strong("Adjust vertical locations to aggregate strikezone"), TRUE)
        ),
        conditionalPanel(
          condition = "input.geom == 'hex'",
          checkboxInput("hex_contour", strong("Add contour lines"), FALSE),
          sliderInput("hex_xbin", "Hex Width:",
                      min = 0.1, max = 3, value = 0.25, step = 0.05),
          sliderInput("hex_ybin", "Hex Height:",
                      min = 0.1, max = 3, value = 0.25, step = 0.05),
          checkboxInput("hex_adjust", strong("Adjust vertical locations to aggregate strikezone"), TRUE)
        ),
        conditionalPanel(
          condition = "input.geom == 'bin'",
          checkboxInput("bin_contour", strong("Add contour lines"), FALSE),
          sliderInput("bin_xbin", "Bin Width:",
                      min = 0.1, max = 3, value = 0.25, step = 0.05),
          sliderInput("bin_ybin", "Bin Height:",
                      min = 0.1, max = 3, value = 0.25, step = 0.05),
          checkboxInput("bin_adjust", strong("Adjust vertical locations to aggregate strikezone"), TRUE)
      )
    )
  ),
    #panel for density geometries
    conditionalPanel( 
      condition = "input.geom == 'bin' || input.geom == 'hex' || input.geom == 'tile'",
      helpText(HTML("<h3>Alter Density(ies)</h3>")),
      uiOutput("denVar1"),
      conditionalPanel(
        condition = "input.denVar1 != 'None'",
        uiOutput("vals1")
      ),
      uiOutput("denVar2"),
      conditionalPanel(
        condition = "input.denVar2 != 'None'",
        uiOutput("vals2")
      )
    )
  ),
  
   #Main panel with static (strikezone) plot and download button
    HTML("<div class=\"span8\">
            <a id=\"downloadPlot\" class=\"btn shiny-download-link\" target=\"_blank\">Download Current Plot</a>
            <div id=\"staticPlot\" class=\"shiny-plot-output\" style=\"position:fixed ; width: 60% ; height: 80%\">
            </div>
          </div>")
  
))
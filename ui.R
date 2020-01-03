# Development TODOs for DECA shiny app

# Single scatterplot with multiple selectable countries
# Add mouse-over to view names of other countries?
# Add ability to select from a list of standard plots

# re-calculate PCs; add to available plots

# Design workflow that allows people to quickly generate all plots for desk review

# Design a new visualization for gender gaps -- maybe a barbell plot?

library(shiny)

shinyUI(fluidPage(
  
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #CFCDC9;}"))
  ),
  
  titlePanel("DECA app (alpha)"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput('plot_type','Select a plot:',choices=plot_list),
      selectizeInput('country','Select a country:',choices=all_countries),
      checkboxInput('score','Show summary score?',TRUE),
      sliderInput('shade','Fraction to shade:',0,1,0.5),
      checkboxInput('pred','Show predictions?',FALSE),
      uiOutput('pc_choice'),
      hr(),
      checkboxInput('help_me','Show help text?',FALSE),
      htmlOutput('how_to')
    ),
    
    mainPanel(
       uiOutput("scaledPlot")
    )
  )
))

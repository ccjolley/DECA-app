# Development TODOs for DECA shiny app

# make a utility file to re-generate plot_data.csv from scratch

# Single scatterplot with multiple selectable countries
# Add mouse-over to view names of other countries?
# Add ability to select from a list of standard plots

# Design workflow that allows people to quickly generate all plots for desk review

# Design a new visualization for gender gaps -- maybe a barbell plot?

library(shiny)

shinyUI(fluidPage(
  
  titlePanel("J2SR-style test plot"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput('plot_type','Select a plot:',choices=plot_list),
      selectizeInput('country','Select a country:',choices=all_countries),
      checkboxInput('score','Show summary score?',TRUE),
      sliderInput('shade','Fraction to shade:',0,1,0.5),
      checkboxInput('pred','Show predictions?',FALSE),
      uiOutput('pc_choice')
    ),
    
    mainPanel(
       uiOutput("scaledPlot")
    )
  )
))

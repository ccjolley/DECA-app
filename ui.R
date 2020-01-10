# Development TODOs for DECA shiny app

# hyperlinked list of data sources shown in a given plot (probably below plot?)

# radio buttons with options to filter comparison countries (by geography and income)

# re-calculate PCs; add to available plots

# Design workflow that allows people to quickly generate all plots for desk review
# Key thing here is giving country selection priority over plot type selection;
# people are more likely to want to see a bunch of plots for the same country
# than vice versa

# Design a new visualization for gender gaps -- maybe a barbell plot?

# Add standard description of how normalization works

# BUGS:
# There are some non-countries in my all_countries list

library(shiny)

shinyUI(fluidPage(
  
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #CFCDC9;}"))
  ),
  
  titlePanel("DECA app (alpha version)"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons('plot_category','Plot category:',choices=c('Summary plot','Scatter plot')),
      uiOutput('plot_options')
    ),
    
    mainPanel(
       uiOutput("main_plot"),
       htmlOutput('plot_sources')
    )
  )
))

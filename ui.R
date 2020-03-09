# Development TODOs for DECA shiny app

# radio buttons with options to filter comparison countries (by geography and income)

# re-calculate PCs; add to available plots

# Design a new visualization for gender gaps -- maybe a barbell plot?

# Add standard description of how normalization works

# Any GSMA Intelligence data series I should add?
# - Market penetration, unique mobile (internet) subscribers
# - Annual growth rate of unique mobile subscribers
# - Unique mobile internet subscribers
# - % connections of various kinds (2G-5G, smartphone, etc.)
# - effective price stats
# - network coverage stats

library(shiny)

shinyUI(fluidPage(
  
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #CFCDC9;}"))
  ),
  
  titlePanel("DECA app (alpha version)"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons('plot_category','Plot category:',choices=c('Summary plot','Scatter plot','Gap plot')),
      uiOutput('plot_options')
    ),
    
    mainPanel(
       uiOutput("main_plot"),
       htmlOutput('plot_sources')
    )
  )
))

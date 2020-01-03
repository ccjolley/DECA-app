# Development TODOs for DECA shiny app

# Double-check that everything included in lit reviews is available
# How to get nicer fonts (ggplot)? - stuck on this one
# Can I get mouse-overs of absolute values from the plot?
# (I think so: https://stackoverflow.com/questions/27965931/tooltip-when-you-mouseover-a-ggplot-on-shiny)
# how to re-size plots to reflect number of rows?
# change default so that, if a particular variable is missing for a country, it just omits the red
#   dot on that row, rather than leaving the row out entirely (also an option to change that default)

# Single scatterplot with multiple selectable countries
# Add mouse-over to view names of other countries?
# Add ability to select from a list of standard plots

# Design workflow that allows people to quickly generate all plots for desk review

# Design a new visualization for gender gaps -- maybe a barbell plot?

library(shiny)
library(Cairo)
library(extrafont)

source('utils.R')

CairoFonts(
  regular="FreeSans:style=Medium",
  bold="FreeSans:style=Bold",
  italic="FreeSans:style=Oblique",
  bolditalic="FreeSans:style=BoldOblique"
)

plot_list <- c('Infrastructure','Access and use','Digital literacy',
               'Digital literacy gender gaps','Affordability','Digital society',
               'Censorship and civil liberties','Freedom on the Net',
               'Privacy and surveillance','Information integrity',
               'Cybersecurity','EIU Global Microscope',
               'GSMA Mobile Money Regulatory Index','Findex',
               'Findex barriers to access','Findex access gaps',
               'IMF Financial Access Survey','Digital trade and e-commerce')

# if I want to filter the country list based on the plot type chosen, I need to be able to generate a list of viable countries for each plot
# this means countries where data exists for at least one variable in the plot
# the best way to do this is probably to change my approach -- instead of a separate function for each plot, I have a
# master data table with all of the variables included in any plot and a named list of the variables required for each plot.

shinyUI(fluidPage(
  
  titlePanel("J2SR-style test plot"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput('plot_type','Select a plot:',choices=plot_list),
      selectizeInput('country','Select a country:',choices=all_countries),
      checkboxInput('score','Show summary score?',TRUE),
      sliderInput('shade','Fraction to shade:',0,1,0.5),
      checkboxInput('pred','Show predictions?',FALSE),
      # numericInput('pcs','Number of principal components:',5,min=1,max=30)
      uiOutput('pc_choice')
    ),
    
    mainPanel(
       plotOutput("dotPlot")
    )
  )
))

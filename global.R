library(Cairo)
library(extrafont)

CairoFonts(
  regular="FreeSans:style=Medium",
  bold="FreeSans:style=Bold",
  italic="FreeSans:style=Oblique",
  bolditalic="FreeSans:style=BoldOblique"
)
options(shiny.usecairo=FALSE)

source('plots.R')

plot_list <- plot_vars$plot_name
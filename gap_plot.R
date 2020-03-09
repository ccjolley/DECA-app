source('plots.R')
library(stringr)

wb_findex <- read_csv('findex.csv') %>%
  select(-starts_with('barrier_'))

# TODO: find another source on urban/rural population split so I can calculate urban levels
# TODO: bring in any other gender gap data (e.g., from ITU) -- currently just uses Findex

# A better plot for visualizing gender (and similar) gaps
# Instead of showing a singe "gap score", show the absolute numbers in a barbell plot

# TODO: need a way to filter the available country list based on the variables selected
#       UNLESS this means that changing the plot variable resets the country list; then
#       this probably isn't worth it -- just filter by countries in WB findex

gap_list <- c('Male/Female','Rich/Poor','Educated/Uneducated','Old/Young','Employed/Unemployed','Overall/Rural')
gap_vars <- c('Account ownership','Borrowing','Digital payments','Mobile money')

available_countries_gap <- function(meas) {
  measurements <- tibble(meas_label=gap_vars,
                         prefix=c('acct','borrow','dig_pay','mm'))
  filter(measurements,meas_label==meas)$prefix
  # TODO: return countries for which this column isn't NA.
}

gap_plot <- function(gap,meas,country_list) {
  gap_types <- tibble(gap_label=gap_list,
                      suffix1=c('_m','_rich','_ed','_old','_labor',''),
                      suffix2=c('_f','_poor','_uned','_young','_nolabor','_rural'))
  measurements <- tibble(meas_label=gap_vars,
                         prefix=c('acct','borrow','dig_pay','mm'))
  
  var1 <- paste0(filter(measurements,meas_label==meas)$prefix,
                 filter(gap_types,gap_label==gap)$suffix1)
  var2 <- paste0(filter(measurements,meas_label==meas)$prefix,
                 filter(gap_types,gap_label==gap)$suffix2)
  lab1 <- str_extract(gap,"^[A-Za-z]+")
  lab2 <- str_extract(gap,"[A-Za-z]+$")
  
  segs <- wb_findex %>%
    select(country,var1,var2) %>%
    filter(country %in% country_list) %>%
    rename(v1=2,v2=3) %>%
    mutate(m = (v1+v2)/2,
           country=fct_reorder(country,m)) %>%
    select(-m)
  
  dots <- segs %>%
    melt %>%
    mutate(variable=ifelse(variable=='v1',lab1,lab2),
           variable=factor(variable,levels=c(lab1,lab2))) 
  
  ggplot(segs) +
    geom_segment(aes(x=v1,xend=v2,y=country,yend=country),color='#6C6463') +
    geom_point(data=dots,aes(x=value,y=country,color=variable),size=5) +
    theme_USAID + colors_USAID +
    xlab('Mobile money usage (WB Findex)') +
    scale_x_continuous(labels=scales::percent_format(accuracy=1)) +
    theme(axis.title.y=element_blank(),
          legend.title=element_blank()) +
    xlab(paste0(meas,' (WB Findex)')) 

  # TODO: Add a caption with an explanatory note for variables that need it (defn of old, young, rich, poor, etc.)
  
}



# gap_plot('Rich/Poor','Borrowing',
#          c('Kenya','Tanzania','Uganda','Ethiopia','Rwanda','Mali'))

source('plots.R')
library(stringr)

wb_findex <- read_csv('findex.csv') %>%
  select(-starts_with('barrier_'))

# TODO: find another source on urban/rural population split so I can calculate urban levels

# A better plot for visualizing gender (and similar) gaps
# Instead of showing a singe "gap score", show the absolute numbers in a barbell plot

gap_list <- c('Male/Female','Rich/Poor','Educated/Uneducated','Old/Young','Employed/Unemployed','Urban/Rural')
gap_captions <- c('','"Rich" = richest 40% of population, "Poor" = poorest 60% of population',
                  '"Educated" = secondary or higher, "Uneducated" = primary or less',
                  '"Old" = 25+ years, "Young" = 15-24 years','',
                  'NOTE: WB Findex doesn\'t disaggregate urban populations; urban values inferred')
gap_vars <- c('Account ownership','Borrowing','Digital payments','Mobile money','Internet use')

get_gap_vars <- function(gap,meas) {
  gap_types <- tibble(gap_label=gap_list,
                      suffix1=c('_m','_rich','_ed','_old','_labor','_urban'),
                      suffix2=c('_f','_poor','_uned','_young','_nolabor','_rural'))
  measurements <- tibble(meas_label=gap_vars,
                         prefix=c('acct','borrow','dig_pay','mm','internet'))
  var1 <- paste0(filter(measurements,meas_label==meas)$prefix,
                 filter(gap_types,gap_label==gap)$suffix1)
  var2 <- paste0(filter(measurements,meas_label==meas)$prefix,
                 filter(gap_types,gap_label==gap)$suffix2)
  c(var1,var2)
}

gap_plot <- function(gap,meas,country_list) {
  if (meas == 'Internet use' && gap != 'Male/Female') {
    stop('Only the Male/Female gap is available for the ITU Internet use variable. Please choose another plot.')
  }
  gv <- get_gap_vars(gap,meas)
  var1 <- gv[1]
  var2 <- gv[2]
  lab1 <- str_extract(gap,"^[A-Za-z]+")
  lab2 <- str_extract(gap,"[A-Za-z]+$")
  
  segs <- wb_findex %>%
    select(country,var1,var2) %>%
    filter(country %in% country_list) %>%
    rename(v1=2,v2=3) %>%
    mutate(m = (v1+v2)/2,
           country=fct_reorder(country,m)) %>%
    select(-m) %>%
    na.omit
  
  dots <- segs %>%
    melt %>%
    mutate(variable=ifelse(variable=='v1',lab1,lab2),
           variable=factor(variable,levels=c(lab1,lab2))) 
  if (meas=='Internet use') {
    source <- 'ITU' 
  } else {
    source <- 'WB Findex'
  }
  ggplot(segs) +
    geom_segment(aes(x=v1,xend=v2,y=country,yend=country),color='#6C6463') +
    geom_point(data=dots,aes(x=value,y=country,color=variable),size=5) +
    theme_USAID + colors_USAID +
    scale_x_continuous(labels=scales::percent_format(accuracy=1)) +
    theme(axis.title.y=element_blank(),
          legend.title=element_blank()) +
    xlab(paste0(meas,' (',source,')')) +
    labs(title=paste0(meas,' (',gap,')'),
         caption=gap_captions[which(gap_list==gap)])

}


gap_plot('Male/Female','Mobile money',
        c('Cyprus','Finland','Brazil','Argentina'))

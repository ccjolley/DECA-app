### This should never be run as part of the online app; it depends on files 
### that are only present on my local version.
source('utils.R')

### load all files
wd <- getwd()
setwd("~/Projects/DECA")
source('itu.R')
source('gsma.R')
source('wef.R')
source('a4ai.R')
source('sdg4.R')
source('vdem.R')
source('fotn.R')
source('rsf.R')
source('ncsi.R')
source('open_data.R')
source('wjp.R')
source('eiu.R')
source('wb.R')
source('imf.R')
source('postal.R')
source('dtri.R')
ws <- read_csv('ws_pc.csv')
setwd(wd)
source('addwef.R')

### get rename_all so I can sanity check this
source('plots.R')

### join everything together
plot_frame <- a4ai
for (t in list(dtri,eiu,fotn,gsma,iipd,itu,ncsi,open_data,rsf,sdg4,vdem,wb,wef,
               wef_private,wef_public,wef_literacy,wjp,ws)) {
  plot_frame <- full_join(plot_frame,t,by='country')
} 
plot_frame <- plot_frame %>% select(country,rename_all$variable)

write_csv(plot_frame,'plot_data.csv')


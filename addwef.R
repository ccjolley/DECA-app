###############################################################################
# WEF
###############################################################################
rename_wef_private <- tibble(
  series = c("Firm-level technology absorption, 1-7 (best)","Capacity for innovation, 1-7 (best)",
             "PCT patents, applications/million pop.","ICT use for business-to-business transactions, 1-7 (best)",
             "Business-to-consumer Internet use, 1-7 (best)","Extent of staff training, 1-7 (best)",
             "Availability of latest technologies, 1-7 (best)"),
  label = c("Firm-level tech absorption","Innovation capacity",
            "PCT patent applications","ICT use for B2B",
            "B2C Internet use","Extent of staff training",'Latest tech available'),
  variable = c('tech_abs','inno_capacity','pct_patents','b2b_use','b2c_use','staff_training',
               'tech_avail'),
  flip=FALSE
)

wef_private <- read_excel('../DECA/data/WEF_NRI_2012-2016_Historical_Dataset.xlsx',sheet=2,skip=3) %>%
  rename(code=`Code NRI 2016`, series=`Series unindented`) %>%
  filter(series %in% rename_wef_private$series, 
         Attribute=='Value', 
         Edition==2016) %>%
  left_join(rename_wef_private,by='series') %>%
  select(variable,Albania:Zimbabwe) %>%
  melt(id.var='variable',variable.name='country') %>%
  dcast(country ~ variable) %>%
  mutate_at(2:8,as.numeric) %>%
  fix_adm0

rename_wef_public <- tibble(
  series = c("Importance of ICTs to gov’t vision, 1-7 (best)", "Government Online Service Index, 0–1 (best)",
             "Gov’t success in ICT promotion, 1-7 (best)","ICT use & gov’t efficiency, 1-7 (best)",
             "E-Participation Index, 0–1 (best)","Gov’t procurement of advanced tech, 1-7 (best)",
             "Availability of latest technologies, 1-7 (best)"),
  variable = c('ict_vision','online_services','ict_promotion','ict_gov_efficiency','e_participaton','gov_procure',
               'tech_avail'),
  label = c("Importance of ICTs to gov’t vision", "Government Online Service Index",
            "Gov’t success in ICT promotion","ICT improves gov't services",
            "Value of gov't websites","Gov’t procurement of advanced tech",'Latest tech available'),
  flip=FALSE
)

wef_public <- read_excel('../DECA/data/WEF_NRI_2012-2016_Historical_Dataset.xlsx',sheet=2,skip=3) %>%
  rename(code=`Code NRI 2016`, series=`Series unindented`) %>%
  filter(series %in% rename_wef_public$series, 
         Attribute=='Value', 
         Edition==2016) %>%
  left_join(rename_wef_public,by='series') %>%
  select(variable,Albania:Zimbabwe) %>%
  melt(id.var='variable',variable.name='country') %>%
  dcast(country ~ variable) %>%
  mutate_at(2:8,as.numeric) %>%
  fix_adm0

rename_wef_literacy <- tibble(
  series = c("Tertiary education gross enrollment rate, %",
             "Quality of management schools, 1-7 (best)",
             "Quality of educational system, 1-7 (best)",
             "Quality of math & science education, 1-7 (best)",
             "Secondary education gross enrollment rate, %",
             "Adult literacy rate, %",
             "Knowledge-intensive jobs, % workforce",
             "Internet access in schools, 1-7 (best)"),
  label = c('Tertiary enrollment (gross)','Mgmt. school quality','Ed system quality',
            'Math & science ed quality','Secondary enrollment (gross)','Adult literacy',
            'Knowledge-intensive jobs','Internet in schools'),
  variable = c('tert_enroll','mgmt_school','ed_quality','stem_quality','sec_enroll',
               'adult_lit','knowledge_jobs','internet_schools'),
  flip=FALSE
)

wef_literacy <- read_excel('../DECA/data/WEF_NRI_2012-2016_Historical_Dataset.xlsx',sheet=2,skip=3) %>%
  rename(code=`Code NRI 2016`, series=`Series unindented`) %>%
  filter(series %in% rename_wef_literacy$series, 
         Attribute=='Value', 
         Edition==2016) %>%
  left_join(rename_wef_literacy,by='series') %>%
  select(variable,Albania:Zimbabwe) %>%
  melt(id.var='variable',variable.name='country') %>%
  dcast(country ~ variable) %>%
  mutate_at(2:9,as.numeric) %>%
  fix_adm0

# remove variables duplicated across two datasets
wef_public <- select(wef_public,-tech_avail)

plot_frame_new <- full_join(plot_frame,wef_private,by='country') %>%
  full_join(wef_public,by='country') %>% 
  full_join(wef_literacy,by='country')

# sanity check
setdiff(plot_frame_new$country,plot_frame$country)
c(nrow(plot_frame_new),nrow(plot_frame))
c(ncol(plot_frame_new),ncol(plot_frame))

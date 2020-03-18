library(tidyverse)

###############################################################################
# Read findex data
###############################################################################
rename_tbl <- tibble(
  full_name=c("Made or received digital payments in the past year (% age 15+)",
              "Made or received digital payments in the past year, male  (% age 15+)",                                                                             
              "Made or received digital payments in the past year, in labor force  (% age 15+)",
              "Made or received digital payments in the past year, out of labor force  (% age 15+)",
              "Made or received digital payments in the past year, female  (% age 15+)",
              "Made or received digital payments in the past year, young adults  (% age 15-24)",
              "Made or received digital payments in the past year, older adults  (% age 25+)",
              "Made or received digital payments in the past year, primary education or less (% age 15+)",
              "Made or received digital payments in the past year, secondary education or more (% age 15+)",
              "Made or received digital payments in the past year, income, poorest 40%  (% age 15+)",
              "Made or received digital payments in the past year, income, richest 60% (% age 15+)",
              "Made or received digital payments in the past year, rural  (% age 15+)",
              "Mobile money account (% age 15+)",
              "Mobile money account, male  (% age 15+)",
              "Mobile money account, in labor force (% age 15+)",
              "Mobile money account, out of labor force (% age 15+)",
              "Mobile money account, female (% age 15+)",
              "Mobile money account, young adults  (% age 15-24)",
              "Mobile money account, older adults (% age 25+)",
              "Mobile money account, primary education or less (% age 15+)",
              "Mobile money account, secondary education or less (% age 15+)", # guessing this was a typo on their part
              "Mobile money account, income, poorest 40% (% age 15+)",
              "Mobile money account, income, richest 60%  (% age 15+)",
              "Mobile money account, rural  (% age 15+)",
              "Borrowed from a financial institution or used a credit card (% age 15+)",
              "Borrowed from a financial institution or used a credit card, male (% age 15+)",
              "Borrowed from a financial institution or used a credit card, in labor force (% age 15+)",
              "Borrowed from a financial institution or used a credit card, out of labor force (% age 15+)",
              "Borrowed from a financial institution or used a credit card, female (% age 15+)",
              "Borrowed from a financial institution or used a credit card, young adults (% age 15-24)",
              "Borrowed from a financial institution or used a credit card, older adults (% age 25+)",
              "Borrowed from a financial institution or used a credit card, primary education or less (% age 15+)",
              "Borrowed from a financial institution or used a credit card, secondary education or more (% age 15+)",
              "Borrowed from a financial institution or used a credit card, income, poorest 40% (% age 15+)",
              "Borrowed from a financial institution or used a credit card, income, richest 60% (% age 15+)",
              "Borrowed from a financial institution or used a credit card, rural (% age 15+)",
              "Financial institution account (% age 15+)",
              "Financial institution account,male(% age 15+)",
              "Financial institution account, in labor force(% age 15+)",
              "Financial institution account, out of labor force (% age 15+)",
              "Financial institution account,female(% age 15+)",
              "Financial institution account,young adults(% age 15-24)",
              "Financial institution account, older adults(% age 25+)",
              "Financial institution account, primary education or less(% age 15+)",
              "Financial institution account, seconday education or more(% age 15+)",
              "Financial institution account,income,poorest 40% (% age 15+)",
              "Financial institution account,income,richest 60% (% age 15+)",
              "Financial institution account, rural(% age 15+)",
              "No account because financial institutions are too far away (% without a financial institution account, age 15+)",
              "No account because financial services are too expensive (% without a financial institution account, age 15+)",
              "No account because of lack of necessary documentation (% without a financial institution account, age 15+)",
              "No account because of lack of trust in financial institutions (% without a financial institution account, age 15+)",
              "No account because of religious reasons (% without a financial institution account, age 15+)",
              "No account because of insufficient funds (% without a financial institution account, age 15+)",
              "No account because someone in the family has an account (% without a financial institution account, age 15+)",
              "No account because of no need for financial services ONLY (% without a financial institution account, age 15+)" 
  )                  
) %>% mutate(
  short_name=sub("(% age 15+)",'',full_name,fixed=TRUE),
  short_name=sub("(% age 15-24)",'',short_name,fixed=TRUE),
  short_name=sub("(% age 25+)",'',short_name,fixed=TRUE),
  short_name=sub(' (% without a financial institution account, age 15+)','',short_name,fixed=TRUE),
  short_name=sub(', ?male *','_m',short_name),
  short_name=sub(', ?female *','_f',short_name),
  short_name=sub(', in labor force *','_labor',short_name),
  short_name=sub(', out of labor force *','_nolabor',short_name),
  short_name=sub(', ?young adults *','_young',short_name),
  short_name=sub(', older adults *','_old',short_name),
  short_name=sub(', primary education or less *','_uned',short_name),
  short_name=sub(', secondar?y education or more *','_ed',short_name),
  short_name=sub(', secondary education or less *','_ed',short_name), 
  short_name=sub(', ?income, ?poorest 40% *','_poor',short_name),
  short_name=sub(', ?income, ?richest 60% *','_rich',short_name),
  short_name=sub(', rural *','_rural',short_name),
  short_name=sub('Made or received digital payments in the past year *','dig_pay',short_name),
  short_name=sub('Mobile money account *','mm',short_name),
  short_name=sub('Borrowed from a financial institution or used a credit card *','borrow',short_name),
  short_name=sub('Financial institution account *','acct',short_name),
  short_name=sub('No account because ','barrier',short_name),
  short_name=sub('financial institutions are too far away','_toofar',short_name),
  short_name=sub('financial services are too expensive','_tooexpens',short_name),
  short_name=sub('of lack of necessary documentation','_nodocs',short_name),
  short_name=sub('of lack of trust in financial institutions','_trust',short_name),
  short_name=sub('of religious reasons','_relig',short_name),
  short_name=sub('of insufficient funds','_nofunds',short_name),
  short_name=sub('someone in the family has an account','_fam',short_name),
  short_name=sub('of no need for financial services ONLY','_noneed',short_name)
)                

wb_findex <- read_excel('../DECA/data/Global Findex Database.xlsx',sheet=1) %>%
  rename(year=`...1`, country=`...3`) %>%
  filter(!country %in% non_countries,year==2017) %>% 
  select(-year,-2,-4,-5) %>%
  melt(id.var='country') %>%
  rename(full_name=variable) %>%
  left_join(rename_tbl,by='full_name') %>%
  na.omit %>%
  select(country,short_name,value) %>%
  dcast(country ~ short_name) %>%
  fix_adm0 

###############################################################################
# Just one gender gap variable from ITU; get that in there also
###############################################################################
itu_gender <- read_excel('../DECA/data/Individuals using the internet by gender_Jun2019.xlsx',skip=2) %>%
  slice(1:115) %>%
  mutate(Male=as.numeric(Male)/100,
         Female=as.numeric(Female)/100) %>%
  rename(country=1,internet_m=Male,internet_f=Female) %>%
  select(country,internet_m,internet_f) %>%
  fix_adm0

###############################################################################
# Save results
###############################################################################
full_join(itu_gender,wb_findex,by='country') %>%
  write_csv('findex.csv')

source('j2sr_plot.R')

# NOTE: The tables below are artifacts of a time when I had separate plotting
# functions for each plot. Now I have one general-purpose plotting function
# that does them all, but I'm keeping these tables around as a way to keep
# track of which variables appear in which plots.

###############################################################################
# Access & Use
###############################################################################
rename_access <- tibble(
  variable=c("mobile_subs","internet_users","fixed_internet","mobile_broadband",
             "nri_usage","mci_afford","mci_consumer",
             "fixed_bb","hh_mobile","hh_computer","hh_internet","ind_computer",
             "ind_mobile","internet_gender_gap","ind_internet","mobile_cell","fixed_tel"),
    label=c('Mobile subscriptions','Internet users','Fixed internet subscriptions',
            'Mobile broadband subscriptions','ICT Use','Affordability',
            'Consumer readiness','Fixed broadband subscriptions',
            'HH mobile ownership','HH computer ownership','HH internet use',
            'Individual computer use','Individual mobile use',
            'Gender gap in internet use','Individual internet use',
            'Mobile subscriptions','Fixed telephone subscriptions'),
  category='Access and Use',
  source=c('WEF NRI','WEF NRI','WEF NRI','WEF NRI','WEF NRI','GSMA MCI','GSMA MCI','ITU','ITU','ITU','ITU',
           'ITU','ITU','ITU','ITU','ITU','ITU')
) %>%
  mutate(flip=(variable %in% c('Cost_1_GB_Share_GNICM','internet_gender_gap')))

###############################################################################
# Under Access & Use -- specific plot for digital literacy
###############################################################################
rename_sdg4 <- tibble(
  variable=c("copy_paste","create_pres","install_soft","move_device","move_file",
             "new_device","sent_email","use_spreadsheet","write_program"),
  label=c('Copy/paste','Create presentation','Install software','Move files to a device',
          'Copy/move file or folder','Install a new device','Sent email with attachment',
          'Use spreadsheet','Write program'),
  category='Digital literacy',
  source='UNESCO',
  flip=FALSE
) 

rename_sdg4_gaps <- tibble(
  variable=c("copy_paste_gender_gap","create_pres_gender_gap","install_soft_gender_gap","move_device_gender_gap",
             "move_file_gender_gap","new_device_gender_gap","sent_email_gender_gap","use_spreadsheet_gender_gap",
             "write_program_gender_gap"),
  label=c('Copy/paste','Create presentation',
          'Install software','Move files to device','Copy/move file or folder',
          'Install new device','Sent email with attachment','Use spreadsheet',
          'Write program'),
  category='Digital literacy gender gaps',
  source='UNESCO',
  flip=FALSE
) 

###############################################################################
# Affordability
###############################################################################
rename_afford <- tibble(
  variable=c("Cost_1_GB_Share_GNICM","access_a4ai","overall_a4ai",
             'mobile_tariffs','handset_prices','taxation','inequality',
             'smartphone_cost','prepaid_cost','postpaid_cost','fixed_bb_cost','arpu',
             'wireless_market_share','bb_market_share'),
  label=c('Cost of 1GB data','Access index','Overall index',
          'Mobile tariffs','Handset prices','Taxation',
          'Inequality',
          'Smartphone cost','Prepaid mobile cost','Postpaid mobile cost',
          'Fixed broadband cost','Avg. revenue per user','Wireless market share',
          'Broadband market share'),
  category='Affordability',
  source=c('A4AI','A4AI','A4AI','GSMA MCI','GSMA MCI','GSMA MCI','GSMA MCI',
           'EIU 3i','EIU 3i','EIU 3i','EIU 3i','EIU 3i','EIU 3i','EIU 3i')
) %>%
  mutate(flip=(variable %in% c('Cost_1_GB_Share_GNICM','wireless_market_share','bb_market_share',
                               'prepaid_cost','postpaid_cost','fixed_bb_cost')))

###############################################################################
# Censorship, information integrity, and digital rights
###############################################################################
rename_censor <- tibble(
  variable=c('v2smgovfilprc','v2smgovshut','v2smgovsm',
             'v2smgovsmalt','v2smgovsmcenprc','v2smregcon',
             'v2smdefabu','v2smarrest','v2x_civlib','v2x_clpol',
             'v2x_clpriv','press_freedom'),
  label=c("Gov filtering in practice","Gov shutdown in practice","Social media shutdowns",
          "Social media alternatives","Social media censorship","Internet legal regulation content",
          "Abuse of defamation/copyright law by elites","Arrests for political content","Civil liberties",
          "Political civil liberties","Private civil liberties","Press freedom"),
  category='Censorship and civil liberties',
  source=c(rep('V-Dem',11),'RSF')
) %>%
  mutate(flip=(variable=='press_freedom'))

rename_fotn <- tibble(
  variable=c('access_obstacles','content_limits',
             'user_violations','fotn_total'),
  label=c('Obstacles to access (FH)','Limits on content (FH)',
          'Violations of user rights (FH)','Freedom on the net (FH)'),
  category='Freedom on the Net',
  source='Freedom House',
  flip=TRUE
)

rename_privacy <- tibble(
  variable = c('v2smprivcon','v2smgovsmmon','v2smprivex','v2smregcap','v2smregapp',
               'v2smlawpr'),
  label=c('Privacy protection by law content','Social media monitoring','Privacy protection by law exists',
          'Gov capacity to regulate online content','Content regulation done by state (not private)',
          'Defamation protection'),
  category='Privacy and surveillance',
  source='V-Dem'
) %>%
  mutate(flip=grepl('\\(.*\\)',label))

rename_info <- tibble(
  variable = c("v2smgovdom","v2smgovab","v2smpardom","v2smparab","v2smfordom","v2smforads"),
  label=c("Government dissemination of false information domestic","Government dissemination of false information abroad",
          "Party dissemination of false information domestic","Party dissemination of false information abroad", 
          "Foreign governments dissemination of false information","Foreign governments ads"),
  category='Information integrity',
  source='V-Dem',
  flip=FALSE
)

###############################################################################
# Cybersecurity
###############################################################################
rename_cyber <- tibble(
  label=c('Gov cyber capacity','Political parties cyber capacity',
          'Global Cybersecurity Index','National Cyber Security Index',
          'Gov filtering capacity','Gov shutdown capacity'),
  variable=c('v2smgovcapsec','v2smpolcap','itu_gci','ncsi','v2smgovfilcap',
             'v2smgovshutcap'),
  category='Cybersecurity',
  source=c('V-Dem','V-Dem','ITU','Estonia','V-Dem','V-Dem'),
  flip=FALSE
)

###############################################################################
# Digital society and governance
###############################################################################
rename_society <- tibble(
  variable=c('v2x_civlib','v2x_clpol','v2x_clpriv','mci_content','ict_laws','nri_enviro',
             'v2smonex','v2smonper','v2smmefra','v2smorgviol','v2smorgavgact','v2smorgelitact',
             'v2smcamp','v2smpolsoc','v2smpolhate','open_data',
             'open_gov','public_laws','right_to_info','civic_part','complaint'),
  label=c('Civil liberties','Political civil liberties',
          'Private civil liberties','Content & Services',
          'Laws relating to ICTs','Environment subindex',
          "Online media existence",
          "Online media perspectives",
          "Online media fractionalization",
          "Use of social media to organize offline violence",
          "Average people’s use of social media to organize offline action",
          "Elites’ use of social media to organize offline action",
          "Party/candidate use of social media in campaigns",
          "Polarization of society",
          "Political parties hate speech",'Open Data Index',
          'Open government','Publicized laws and gov data',
          'Right to information','Civic participation','Complaint mechanisms'),
  category='Digital society',
  source=c('V-Dem','V-Dem','V-Dem','GSMA MCI','WEF','WEF NRI','V-Dem','V-Dem',
           'V-Dem','V-Dem','V-Dem','V-Dem','V-Dem','V-Dem','V-Dem','OKF','WJP',
           'WJP','WJP','WJP','WJP'),
  flip=FALSE
)

###############################################################################
# Digital economy 
###############################################################################
### EIU Global Microscope
rename_eiu <- tibble(
  variable=c("gov_support","stability_integrity","products_outlets",
             "consumer_protection","infrastructure_eiu","overall_eiu"),
  label=c('Government and policy support','Stability and integrity','Products and outlets',
          'Consumer protection','Infrastructure','EIU overall'),
  category='Digital finance',
  source='EIU Global Microscope',
  flip=FALSE
)

### GSMA Mobile Money Regulation Index
rename_mmri <- tibble(
  variable=c("mmri","mmri_auth","mmri_consumer","mmri_transact","mmri_kyc",
             "mmri_agent","mmri_infra"),
  label=c('MMRI','Authorization','Consumer Protection','Transaction Limits',
          'KYC','Agent network','Infrastructure and investment environment'),
  category='Digital finance',
  source='GSMA MMRI',
  flip=FALSE
)

### World Bank Findex
rename_barrier <- tibble(
  variable=c("barrier_fam","barrier_nodocs","barrier_nofunds","barrier_noneed",
             "barrier_relig","barrier_tooexpens","barrier_toofar","barrier_trust"),
  label=c('Family','No documents','No funds','No need','Religion','Too expensive',
          'Too far away','Lack of trust'),
  category='Digital finance barriers',
  source='WB Findex',
  flip=FALSE)

rename_findex <- tibble(
  variable=c("acct","borrow","dig_pay","mm"),
  label=c('Account ownership','Borrowed','Used digital payments','Mobile money'),
  category='Digital finance',
  source='WB Findex',
  flip=FALSE
)

rename_findex_gaps <- tibble(
  variable=c('acct_gender_gap',"borrow_gender_gap","dig_pay_gender_gap",'mm_gender_gap',"acct_wealth_gap",
             "borrow_wealth_gap","dig_pay_wealth_gap",'mm_wealth_gap',"acct_age_gap",
             "borrow_age_gap","dig_pay_age_gap",'mm_age_gap',"acct_ed_gap","borrow_ed_gap",
             "dig_pay_ed_gap",'mm_ed_gap',"acct_rural_gap","borrow_rural_gap",
             "dig_pay_rural_gap",'mm_rural_gap'),
  label=c('Account (GENDER)',
          'Borrowing (GENDER)','Digital payments (GENDER)','Mobile money (GENDER)',
          'Account (WEALTH)',
          'Borrowing (WEALTH)','Digital payments (WEALTH)','Mobile money (WEALTH)','Account (AGE)',
          'Borrowing (AGE)','Digital payments (AGE)','Mobile money (AGE)','Account (EDUCATION)',
          'Borrowing (EDUCATION)','Digital payments (EDUCATION)','Mobile money (EDUCATION)',
          'Account (URBAN/RURAL)','Borrowing (URBAN/RURAL)',
          'Digital payments (URBAN/RURAL)','Mobile money (URBAN/RURAL)'),
  category='Digital finance gaps',
  source='WB Findex',
  flip=FALSE
)

###############################################################################
# Infrastructure
###############################################################################

rename_infra <- tibble(
  variable=c("mci_infra","mci_infra_coverage","cov_2G","cov_3G","cov_4G",
             "mci_infra_performance",'download','upload','latency',"mci_infra_enabling",'elect',
             'bandwidth_gsma',"servers","tlds","ixps",
             'mci_infra_spectrum','spectrum_dd','spectrum_low','spectrum_high'),
  label=c('Infrastructure (MCI subindex)','Network coverage (MCI dimension)',
          '2G coverage','3G coverage','4G coverage',
          'Network performance (MCI dimension)','Mobile download speed','Mobile upload speed','Mobile latency',
          'Enabling infrastructure (MCI dimension)','Electricity access','Interational internet bandwidth',
          'Secure internet servers','Top-level domains per capita','IXPs per capita','Spectrum (MCI dimension)',
          'Digital dividend spectrum','Spectrum below 1 GHz','Spectrum 1-3 GHz'),
  category='Infrastructure',
  source='GSMA MCI',
  flip=FALSE
)

###############################################################################
# Digital trade and e-commerce
###############################################################################
rename_trade <- tibble(
  variable=c('db_score','db_trade','db_startbiz','iipd',
             'dtri','fiscal_market','establishment','data_restrictions','trading_restrictions'),
  label=c('Doing business score','Trade across borders',
          'Starting a business','Postal development index',
          'Digital Trade Restrictiveness','Fiscal restrictions & market access',
          'Establishment restrictions','Restrictions on data','Trading restrictions'),
  category='Digital trade and e-commerce',
  source=c('WB Doing Business','WB Doing Business','WB Doing Business',
           'Universal Postal Union','DTRI','DTRI','DTRI','DTRI','DTRI')
) %>%
  mutate(flip=grepl('DTRI',label))


# TODO: Use the mean for the overall score on this plot (while keeping PC1 
# on others), because DTRI and Doing Business aren't very well-correlated 
# (see China, for example)

###############################################################################
# WEF
###############################################################################
rename_wef_private <- tibble(
  variable = c('tech_abs','inno_capacity','pct_patents','b2b_use','b2c_use','staff_training',
               'tech_avail'),
  label = c("Firm-level tech absorption","Innovation capacity",
            "PCT patent applications","ICT use for B2B",
            "B2C Internet use","Extent of staff training",'Latest tech available'),
  category='Private sector adoption',
  source='WEF NRI',
  flip=FALSE
)

rename_wef_public <- tibble(
  variable = c('ict_vision','online_services','ict_promotion','ict_gov_efficiency','e_participaton','gov_procure',
               'tech_avail'),
  label = c("Importance of ICTs to gov’t vision", "Government Online Service Index",
            "Gov’t success in ICT promotion","ICT improves gov't services",
            "Value of gov't websites","Gov’t procurement of advanced tech",'Latest tech available'),
  category='Public sector adoption',
  source='WEF NRI',
  flip=FALSE
)


rename_wef_literacy <- tibble(
  variable = c('tert_enroll','mgmt_school','ed_quality','stem_quality','sec_enroll',
               'adult_lit','knowledge_jobs','internet_schools'),
  label = c('Tertiary enrollment (gross)','Mgmt. school quality','Ed system quality',
            'Math & science ed quality','Secondary enrollment (gross)','Adult literacy',
            'Knowledge-intensive jobs','Internet in schools'),
  category='Digital literacy',
  source='WEF NRI',
  flip=FALSE
)

rename_womanstats <- tibble(
  variable='ws_PC1',
  label='Aggregate gender score',
  category='Gender',
  source='WomanStats',
  flip=FALSE
)

###############################################################################
# One plotting function to rule them all
###############################################################################

rename_all <- rbind(rename_access,rename_afford,rename_barrier,rename_censor,
                    rename_fotn,rename_info,rename_privacy,
                    rename_cyber,rename_eiu,rename_findex,rename_findex_gaps,
                    rename_infra,rename_mmri,rename_sdg4,
                    rename_sdg4_gaps,rename_society,rename_trade,
                    rename_wef_literacy,rename_wef_private,rename_wef_public,
                    rename_womanstats) %>%
  distinct

write_csv(rename_all,'app_variables.csv')

# sanity checks
#rename_all$variable %>% table() %>% sort
#rename_all$label %>% table() %>% sort

plot_frame <- read_csv('plot_data.csv')

plot_vars <- tibble(
  plot_name = c('Infrastructure','Access and use','Digital literacy',
                'Digital literacy gender gaps','Affordability',
                'WEF digital literacy','WEF private sector','WEF public sector',
                'Digital society',
                'Censorship and civil liberties','Freedom on the Net',
                'Privacy and surveillance','Information integrity',
                'Cybersecurity','EIU Global Microscope',
                'GSMA Mobile Money Regulatory Index','Findex',
                'Findex barriers to access','Findex access gaps',
                'Digital trade and e-commerce'),
  varlist = sapply(list(rename_infra,rename_access,rename_sdg4,rename_sdg4_gaps,
                        rename_afford,rename_wef_literacy,rename_wef_private,rename_wef_public,
                        rename_society,rename_censor,
                        rename_fotn,rename_privacy,rename_info,rename_cyber,
                        rename_eiu,rename_mmri,rename_findex,rename_barrier,
                        rename_findex_gaps,rename_trade),function(x) list(x$variable))
)

available_countries <- function(pname) {
  v <- filter(plot_vars,plot_name==pname)$varlist %>% unlist
  tmp <- plot_frame %>% select(country,v)
  present <- rowSums(!is.na(select(tmp,-country)))
  tmp$country[present>0] %>% sort
}

get_sources <- function(pname,cname) {
  v1 <- filter(plot_vars,plot_name==pname)$varlist %>% unlist
  v2 <- plot_frame %>%
    filter(country==cname) %>%
    select_if(~ !any(is.na(.))) %>%
    names
  tmp <- rename_all %>% filter(variable %in% v1,variable %in% v2)
  unique(tmp$source)
}

# test run
# sapply(plot_vars$plot_name,function(x) length(available_countries(x)))
  
# unified plotting function

deca_plot <- function(pname,country_name,show_pred=FALSE,shade_fraction=0.5,
                       sort_order='cor',num_pcs=5,overall_score='PC1',show_sources=FALSE) {
  if (!pname %in% plot_vars$plot_name) {
    message(paste0('ERROR: ',pname,' is not a valid plot name.'))
  }
  v <- filter(plot_vars,plot_name==pname)$varlist %>% unlist
  rename <- rename_all %>% filter(variable %in% v)
  if (show_sources) {
    rename <- rename %>%
      mutate(label = paste0(label,' (',source,')'))
  }
  plot_frame %>%
    select(country,v) %>%
    j2sr_style_plot(rename,country_name,show_pred,
                    shade_fraction,sort_order,num_pcs,overall_score) +
    ggtitle(paste0(pname,': ',country_name))
}

# test run
#
#deca_plot('Censorship and civil liberties','Kenya') +
#  theme(plot.margin=unit(c(5.5,5.5,30,5.5),'points'))
#deca_plot('WEF public sector','Kenya')

# resize plots based on number of rows
num_rows <- function(pname,country_name) {
  v <- filter(plot_vars,plot_name==pname)$varlist %>% unlist
  na_count <- plot_frame %>% 
    filter(country==country_name) %>% 
    select(v) %>%
    is.na %>%
    rowSums
  length(v) - na_count
}

########### DECA scatter plots
scatter_list <- c('Internet users / Mobile tariffs',
                  'Gender score / Digital payments gender gap',
                  'Government filtering / Government cyber capacity',
                  'Postal development / Internet users')

deca_scatter <- function(plot_type,highlight_list) {
  if (!plot_type %in% scatter_list) {
    message(paste0('ERROR: ',plot_type,' is not a valid plot type!'))
    return()
  }
  if (plot_type == scatter_list[1]) {
    xlabel <- 'Percent of internet users (ITU)'
    ylabel <- 'Affordability of mobile tariffs (GSMA)'
    plotme <- select(plot_frame,country,ind_internet,mobile_tariffs)
  } else if (plot_type == scatter_list[2]) {
    xlabel <- 'Aggregate gender score (derived from WomanStats)'
    ylabel <- 'Gender gap in digital payments'
    plotme <- select(plot_frame,country,ws_PC1,dig_pay_gender_gap)
  } else if (plot_type == scatter_list[3]) {
    xlabel <- 'Government filtering in practice'
    ylabel <- 'Government cyber capacity'
    plotme <- select(plot_frame,country,v2smgovfilprc,v2smgovcapsec)
  } else if (plot_type == scatter_list[4]) {
    xlabel <- 'Integrated Index for Postal Development'
    ylabel <- 'Percent of internet users (ITU)'
    plotme <- select(plot_frame,country,iipd,ind_internet)
  }
  highlight_scatter(plotme,highlight_list) +
    xlab(xlabel) + ylab(ylabel)
}

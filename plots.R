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
    label=c('Mobile subscriptions (WEF)','Internet users (WEF)','Fixed internet subscriptions (WEF)',
            'Mobile broadband subscriptions (WEF)','ICT Use (J2SR/WEF)','Affordability (GSMA)',
            'Consumer readiness (GSMA)','Fixed broadband subscriptions (ITU)',
            'HH mobile ownership (ITU)','HH computer ownership (ITU)','HH internet use (ITU)',
            'Individual computer use (ITU)','Individual mobile use (ITU)',
            'Gender gap in internet use (ITU)','Individual internet use (ITU)',
            'Mobile subscriptions (ITU)','Fixed telephone subscriptions (ITU)')
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
  label=c('Cost of 1GB data (A4AI)','Access index (A4AI)','Overall index (A4AI)',
          'Mobile tariffs (GSMA)','Handset prices (GSMA)','Taxation (GSMA)',
          'Inequality (GSMA)',
          'Smartphone cost (3i)','Prepaid mobile cost (3i)','Postpaid mobile cost (3i)',
          'Fixed broadband cost (3i)','Avg. revenue per user (3i)','Wireless market share (3i)',
          'Broadband market share (3i)')
) %>%
  mutate(flip=(variable %in% c('Cost_1_GB_Share_GNICM','wireless_market_share','bb_market_share',
                               'prepaid_cost','postpaid_cost','fixed_bb_cost')))

###############################################################################
# Censorship, information integrity, and digital rights
###############################################################################
rename_censor <- tibble(
  variable=c('v2smgovfilprc','v2x_civlib','v2smregcon','v2smarrest','v2x_clpol','v2smdefabu',
             'v2smgovshut','v2smgovsm','v2smgovsmalt','v2smgovsmcenprc','v2x_clpriv','press_freedom'),
  label=c("Gov filtering in practice","Gov shutdown in practice","Social media shutdown in practice",
          "Social media alternatives","Social media censorship","Internet legal regulation content",
          "Abuse of defamation/copyright law by elites","Arrests for political content","Civil liberties",
          "Political civil liberties","Private civil liberties","Press freedom (RSF)")
) %>%
  mutate(flip=(variable=='press_freedom'))

rename_fotn <- tibble(
  variable=c('access_obstacles','content_limits',
             'user_violations','fotn_total'),
  label=c('Obstacles to access (FH)','Limits on content (FH)',
          'Violations of user rights (FH)','Freedom on the net (FH)'),
  flip=TRUE
)

rename_privacy <- tibble(
  variable = c('v2smprivcon','v2smgovsmmon','v2smprivex','v2smregcap','v2smregapp',
               'v2smlawpr'),
  label=c('Privacy protection by law content','Social media monitoring','Privacy protection by law exists',
          'Gov capacity to regulate online content','Content regulation done by state (not private)',
          'Defamation protection')
) %>%
  mutate(flip=grepl('\\(.*\\)',label))

rename_info <- tibble(
  variable = c("v2smgovdom","v2smgovab","v2smpardom","v2smparab","v2smfordom","v2smforads"),
  label=c("Government dissemination of false information domestic","Government dissemination of false information abroad",
          "Party dissemination of false information domestic","Party dissemination of false information abroad", 
          "Foreign governments dissemination of false information","Foreign governments ads"),
  flip=FALSE
)

###############################################################################
# Cybersecurity
###############################################################################
rename_cyber <- tibble(
  label=c('Gov cyber capacity (VDem)','Political parties cyber capacity (VDem)',
          'Global Cybersecurity Index (ITU)','National Cyber Security Index (Estonia)',
          'Gov filtering capacity (VDem)','Gov shutdown capacity (VDem)'),
  variable=c('v2smgovcapsec','v2smpolcap','itu_gci','ncsi','v2smgovfilcap',
             'v2smgovshutcap'),
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
  label=c('Civil liberties (VDem)','Political civil liberties (VDem)',
          'Private civil liberties (VDem)','Content & Services (GSMA)',
          'Laws relating to ICTs (WEF)','Environment subindex (WEF)',
          "Online media existence (VDem)",
          "Online media perspectives (VDem)",
          "Online media fractionalization (VDem)",
          "Use of social media to organize offline violence (VDem)",
          "Average people’s use of social media to organize offline action (VDem)",
          "Elites’ use of social media to organize offline action (VDem)",
          "Party/candidate use of social media in campaigns (VDem)",
          "Polarization of society (VDem)",
          "Political parties hate speech (VDem)",'Open Data Index (OKF)',
          'Open government (WJP/J2SR)','Publicized laws and gov data (WJP)',
          'Right to information (WJP)','Civic participation (WJP)','Complaint mechanisms (WJP)'),
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
  flip=FALSE
)

### GSMA Mobile Money Regulation Index
rename_mmri <- tibble(
  variable=c("mmri","mmri_auth","mmri_consumer","mmri_transact","mmri_kyc",
             "mmri_agent","mmri_infra"),
  label=c('MMRI','Authorization','Consumer Protection','Transaction Limits',
          'KYC','Agent network','Infrastructure and investment environment'),
  flip=FALSE
)

### World Bank Findex
rename_barrier <- tibble(
  variable=c("barrier_fam","barrier_nodocs","barrier_nofunds","barrier_noneed",
             "barrier_relig","barrier_tooexpens","barrier_toofar","barrier_trust"),
  label=c('Family','No documents','No funds','No need','Religion','Too expensive',
          'Too far away','Lack of trust'),
  flip=FALSE
)

rename_findex <- tibble(
  variable=c("acct","borrow","dig_pay","mm"),
  label=c('Account ownership','Borrowed','Used digital payments','Mobile money'),
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
          'Nework performance (MCI dimension)','Mobile download speed','Mobile upload speed','Mobile latency',
          'Enabling infrastructure (MCI dimension)','Electricy access','Interational internet bandwidth',
          'Secure internet servers','Top-level domains per capita','IXPs per capita','Spectrum (MCI dimension)',
          'Digital dividend spectrum','Spectrum below 1 GHz','Spectrum 1-3 GHz'),
  flip=FALSE
)

###############################################################################
# Digital trade and e-commerce
###############################################################################
rename_trade <- tibble(
  variable=c('db_score','db_trade','db_startbiz','iipd',
             'dtri','fiscal_market','establishment','data_restrictions','trading_restrictions'),
  label=c('Doing business score (WB)','Trade across borders (WB)',
          'Starting a business (WB)','Postal development index',
          'Digital Trade Restrictiveness (DTRI)','Fiscal restrictions & market access (DTRI)',
          'Establishment restrictions (DTRI)','Restrictions on data (DTRI)','Trading restrictions (DTRI)')
) %>%
  mutate(flip=grepl('DTRI',label))


# TODO: Use the mean for the overall score on this plot (while keeping PC1 
# on others), because DTRI and Doing Business aren't very well-correlated 
# (see China, for example)

###############################################################################
# One plotting function to rule them all
###############################################################################

rename_all <- rbind(rename_access,rename_afford,rename_barrier,rename_censor,
                    rename_fotn,rename_info,rename_privacy,
                    rename_cyber,rename_eiu,rename_findex,rename_findex_gaps,
                    rename_infra,rename_mmri,rename_sdg4,
                    rename_sdg4_gaps,rename_society,rename_trade)

plot_frame <- read_csv('plot_data.csv')

plot_vars <- tibble(
  plot_name = c('Infrastructure','Access and use','Digital literacy',
                'Digital literacy gender gaps','Affordability','Digital society',
                'Censorship and civil liberties','Freedom on the Net',
                'Privacy and surveillance','Information integrity',
                'Cybersecurity','EIU Global Microscope',
                'GSMA Mobile Money Regulatory Index','Findex',
                'Findex barriers to access','Findex access gaps',
                'Digital trade and e-commerce'),
  varlist = sapply(list(rename_infra,rename_access,rename_sdg4,rename_sdg4_gaps,
                        rename_afford,rename_society,rename_censor,
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

# test run
# sapply(plot_vars$plot_name,function(x) length(available_countries(x)))
  
# unified plotting function

deca_plot <- function(pname,country_name,show_pred=FALSE,shade_fraction=0.5,
                       sort_order='none',num_pcs=5,overall_score='mean') {
  v <- filter(plot_vars,plot_name==pname)$varlist %>% unlist
  rename <- rename_all %>% filter(variable %in% v)
  plot_frame %>%
    select(country,v) %>%
    j2sr_style_plot(rename,country_name,show_pred,
                    shade_fraction,sort_order,num_pcs,overall_score) +
    ggtitle(paste0(pname,': ',country_name))
}

# test run
#deca_plot('Censorship and civil liberties','Kenya') + theme(text=element_text(family='Palatino'))

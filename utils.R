library(readxl)
library(dplyr)
library(reshape2)
library(readr)
library(ggplot2)
library(ggrepel)

###############################################################################
# Standardize country names. When possible, names should match the Natural
# Earth shapefiles so that I can make maps as needed.
#   fix_adm0() assumes that it's getting a data frame with country names stored
#   in a column called 'country'.
###############################################################################
fix_adm0 <- function(df,mapping=FALSE) {
  rename_df <- data_frame(country=c('Bahamas, The','Bahamas','Burma/Myanmar','Burma','Cape Verde',
                                    'Congo, Democratic Republic of the Congo',
                                    'Democratic Republic of Congo',
                                    'Dem. Rep. Congo',
                                    'Congo, Dem. Rep.',
                                    'Congo, Democratic Republic',
                                    'Congo, Republic of','Congo, Rep.','Congo',
                                    'Côte d\'Ivoire','Cote d\'Ivoire',
                                    'Democratic Republic of Vietnam','Viet Nam',
                                    'Egypt, Arab Rep.','Gambia, The','The Gambia','Hong Kong SAR',
                                    'Hong Kong SAR, China','Iran, Islamic Rep.',
                                    'Korea, Dem. People’s Rep.','Korea, North','Korea, Rep.','Korea, South',
                                    'Lao PDR','Lao P.D.R.','Macao SAR, China',
                                    'Macedonia, FYR','Micronesia, Fed. Sts.',
                                    'Russian Federation','São Tomé and Príncipe',
                                    'Syrian Arab Republic','United States','Venezuela, RB',
                                    'Yemen, Rep.','Kyrgyz Republic','St. Vincent and The Grenadines',
                                    'Slovak Republic','Bolivia (Plurinational State of)',
                                    'Bosnia Herzegovina','China, Taiwan Province of',
                                    'Micronesia','Indonesia (...2002)',
                                    'Iran (Islamic Republic of)','Korea, Dem. People\'s Rep. of',
                                    'Korea, Republic of','Lao People\'s Dem. Rep.',
                                    'Libyan Arab Jamahiriya','Micronesia (Federated States of)',
                                    'Republic of Korea','Republic of Moldova','Sudan (...2011)',
                                    'Taiwan, China','TFYR of Macedonia','The former Yugoslav Rep. of Macedonia',
                                    'United Rep. of Tanzania','Tanzania','United States Virgin Islands',
                                    'Venezuela (Bolivarian Rep. of)','China, Hong Kong SAR',
                                    'China, Macao SAR','Dem. Rep. of the Congo','Timor-Leste','Kazakstan',
                                    'Palestinian Territory, Occupied','State of Palestine','Palestine/Gaza','Palestine/West Bank',
                                    'Brunei Darussalam','Swaziland','Serbia','Sao Tome and Principe',
                                    'Czech Republic','Bosnia','Central AfR',
                                    'Congo, Democratic Republic of','DominicanRep',
                                    'Unitd Kingdm','GuineaBiss','Equa Guinea','Kyrgyz',
                                    'Korea North','Korea South','UAE','USA','SierraLeo',
                                    'Palestine','Sudan South','Papua NG','Slovak Rep',
                                    'Trinidad','Central African Rep.','Congo (Rep. of the)',
                                    'Democratic People\'s Republic of Korea','Dem. People\'s Rep. of Korea',
                                    'Dominican Rep.','Korea (Rep. of)','Lao People\'s Democratic Republic',
                                    'Nepal (Republic of)','Taiwan, Province of China','North Macedonia',
                                    'The Republic of North Macedonia','Hong Kong, China','Macao, China',
                                    'Eswatini','Lao People’s Democratic Republic','The Democratic Republic Of The Congo',
                                    'Islamic Republic of Iran','United Kingdom of Great Britain and Northern Ireland',
                                    "China, People's Republic of","Egypt, Arab Republic of",'PNG',"Yemen, Republic of",
                                    'Hong Kong, SAR China',"Congo Brazzaville","D.R. Congo","Congo, DR",
                                    "Venezuela (Bolivarian Republic Of)","DRC",
                                    "Hong Kong","Afghanistan, Islamic Republic of","Armenia, Republic of","Azerbaijan, Republic of",
                                    "China, P.R.: Hong Kong","China, P.R.: Macao","China, P.R.: Mainland",
                                    "Eswatini, Kingdom of","Iran, Islamic Republic of","Kosovo, Republic of",
                                    "Marshall Islands, Republic of","Micronesia, Federated States of",
                                    "North Macedonia, Republic of","Serbia, Republic of","Timor-Leste, Dem. Rep. of",
                                    "Venezuela, Republica Bolivariana de","United Republic Of Tanzania",'Bosnia-Herzegovina',
                                    'Central African Rep','D R Congo','Trinidad/Tobago',
                                    "China (People's Rep.)","Czech Rep.","Korea (Rep.)","Iran (Islamic Rep.)",
                                    "Tanzania (United Rep.)","Honduras (Rep.)","Lao","Bahrain (Kingdom)",
                                    "Rwanda (Rep.)","Panama (Rep.)","Saint Christopher (Kitts) and Nevis",
                                    "Venezuela (Bolivarian Rep.)","Syrian Arab Rep.","Congo (Rep.)",
                                    "State of Libya","Moldova (Republic of)",'Great Britain'),
                          country_new=c('The Bahamas','The Bahamas','Myanmar','Myanmar','Cabo Verde',
                                        'Democratic Republic of the Congo',
                                        'Democratic Republic of the Congo',
                                        'Democratic Republic of the Congo',
                                        'Democratic Republic of the Congo',
                                        'Democratic Republic of the Congo',
                                        'Republic of the Congo','Republic of the Congo','Republic of the Congo',
                                        'Ivory Coast','Ivory Coast',
                                        'Vietnam','Vietnam',
                                        'Egypt','Gambia','Gambia','Hong Kong',
                                        'Hong Kong','Iran',
                                        'North Korea','North Korea','South Korea','South Korea',
                                        'Laos','Laos','Macau',
                                        'Macedonia','Federated States of Micronesia',
                                        'Russia','Sao Tome and Principe',
                                        'Syria','United States of America','Venezuela',
                                        'Yemen','Kyrgyzstan','St. Vincent and the Grenadines',
                                        'Slovakia','Bolivia',
                                        'Bosnia and Herzegovina','Taiwan',
                                        'Federated States of Micronesia','Indonesia',
                                        'Iran','North Korea',
                                        'South Korea','Laos',
                                        'Libya','Federated States of Micronesia',
                                        'South Korea','Moldova','Sudan',
                                        'Taiwan','Macedonia','Macedonia',
                                        'United Republic of Tanzania','United Republic of Tanzania','Virgin Islands (U.S.)',
                                        'Venezuela','Hong Kong','Macau','Democratic Republic of the Congo','East Timor','Kazakhstan',
                                        'West Bank and Gaza','West Bank and Gaza','West Bank and Gaza','West Bank and Gaza',
                                        'Brunei','eSwatini','Republic of Serbia','São Tomé and Principe',
                                        'Czechia','Bosnia and Herzegovina','Central African Republic',
                                        'Democratic Republic of the Congo','Dominican Republic',
                                        'United Kingdom','Guinea-Bissau','Equatorial Guinea',
                                        'Kyrgyzstan','North Korea','South Korea','United Arab Emirates',
                                        'United States of America','Sierra Leone','West Bank and Gaza',
                                        'South Sudan','Papua New Guinea','Slovakia',
                                        'Trinidad and Tobago','Central African Republic','Republic of the Congo',
                                        'North Korea','North Korea','Dominican Republic','South Korea',
                                        'Laos','Nepal','Taiwan','Macedonia','Macedonia','Hong Kong','Macao',
                                        'eSwatini','Laos','Democratic Republic of the Congo','Iran',
                                        'United Kingdom','China','Egypt','Papua New Guinea','Yemen',
                                        'Hong Kong',"Republic of the Congo","Democratic Republic of the Congo",
                                        "Democratic Republic of the Congo","Venezuela","Democratic Republic of the Congo",
                                        "Hong Kong","Afghanistan","Armenia","Azerbaijan",
                                        "Hong Kong","Macao","China","eSwatini","Iran","Kosovo",
                                        "Marshall Islands","Federated States of Micronesia",
                                        "Macedonia","Republic of Serbia","East Timor","Venezuela",
                                        "United Republic of Tanzania",'Bosnia and Herzegovina','Central African Republic',
                                        'Democratic Republic of the Congo','Trinidad and Tobago',
                                        "China","Czechia","South Korea","Iran",
                                        "United Republic of Tanzania","Honduras","Laos","Bahrain",
                                        "Rwanda","Panama","Saint Kitts and Nevis",
                                        "Venezuela","Syria","Republic of the Congo",
                                        "Libya",'Moldova','United Kingdom'))
                                       
  tmp <- df %>%
    mutate(country=sub('Saint\\.','Saint',country),
           country=sub('St\\.','Saint',country)) %>%
    left_join(rename_df,by='country') %>%
    mutate(country = ifelse(is.na(country_new),country,country_new)) %>%
    select(-country_new)
  # So hard to get Cote d'Ivoire right
  tmp$country[grep('voire',tmp$country)] <- 'Ivory Coast'
  # GADM insists on a few weird country names; don't use them unless you're making a map
  if (!mapping) {
    tmp <- tmp %>%
      mutate(country=ifelse(country=='Macedonia','North Macedonia',country),
             country=ifelse(country=='United States of America','United States',country),
             country=ifelse(country=='United Republic of Tanzania','Tanzania',country))
  }
  tmp
}

###############################################################################
# Country lists
#
# Note that all country names came from the Natural Earth shapefile set and
# shouldn't be taken as the authoritative position of anybody on which 
# countries really exist or what they are called. Certainly not the U.S. 
# government.
###############################################################################

usaid_countries <- c('Afghanistan','Albania','Angola','Armenia','Azerbaijan',
                     'Bangladesh','Belarus','Benin','Bosnia and Herzegovina',
                     'Botswana','Brazil','Burkina Faso','Myanmar','Burundi',
                     'Cambodia','Cameroon','Central African Republic','Chad',
                     'China','Colombia','Ivory Coast','Cuba','Cyprus',
                     'Democratic Republic of the Congo','Djibouti',
                     'Dominican Republic','Ecuador','Egypt',
                     'El Salvador','eSwatini','Ethiopia','Georgia','Ghana',
                     'Guatemala','Guinea',
                     'Guyana','Haiti','Honduras','India','Indonesia','Iraq',
                     'Jamaica','Jordan','Kazakhstan',
                     'Kenya','Kosovo','Kyrgyzstan','Laos','Lebanon','Lesotho',
                     'Liberia','Libya','North Macedonia','Madagascar','Malawi','Maldives',
                     'Mali','Mauritania','Mexico','Moldova','Mongolia','Montenegro',
                     'Morocco','Mozambique','Namibia','Nepal','Nicaragua','Niger',
                     'Nigeria','Pakistan','Panama','Paraguay','Peru','Philippines',
                     'Republic of the Congo','Rwanda','Senegal','Republic of Serbia','Sierra Leone',
                     'Somalia','South Africa','South Sudan','Sri Lanka','Sudan','Syria',
                     'Tajikistan','Tanzania','Thailand','East Timor','Tunisia','Turkmenistan',
                     'Uganda','Ukraine','Uzbekistan','Venezuela','Vietnam',
                     'West Bank and Gaza','Yemen','Zambia','Zimbabwe')

non_usaid_countries <- c("Algeria","Andorra","Antigua and Barbuda",
                         "Argentina","Australia","Austria","Bahrain",
                         "Barbados","Belgium","Belize","Bhutan",
                         "Bolivia","Brunei","Bulgaria","Cabo Verde",
                         "Canada","Chile","Comoros","Costa Rica",
                         "Croatia","Czechia","Denmark","Dominica",
                         "Equatorial Guinea","Eritrea","Estonia","Federated States of Micronesia",
                         "Fiji","Finland","France","Gabon",
                         "Gambia","Germany","Greece","Grenada",
                         "Guinea-Bissau","Hong Kong","Hungary","Iceland","Iran",
                         "Ireland","Israel","Italy","Japan",
                         "Kashmir","Kiribati","Kuwait","Latvia",
                         "Liechtenstein","Lithuania","Luxembourg","Malaysia",
                         "Malta","Marshall Islands","Mauritius","Monaco",
                         "Nauru","Netherlands","New Zealand","Northern Cyprus",
                         "North Korea","Norway","Oman","Palau",
                         "Papua New Guinea","Poland","Portugal","Qatar",
                         "Romania","Russia","Saint Kitts and Nevis","Saint Lucia",
                         "Saint Vincent and the Grenadines","Samoa","San Marino","São Tomé and Principe",
                         "Saudi Arabia","Seychelles","Singapore","Slovakia",
                         "Slovenia","Solomon Islands","Somaliland","South Korea",
                         "Spain","Suriname","Sweden","Switzerland",
                         "Taiwan","The Bahamas","Togo","Tonga",
                         "Trinidad and Tobago","Turkey","United Arab Emirates","United Kingdom",
                         "United States","Uruguay","Vanuatu","Vatican",
                         "Western Sahara")

all_countries <- c(usaid_countries,non_usaid_countries) %>% sort

# USAID bureau groupings
africa <- c('Angola','Benin','Botswana','Burkina Faso','Burundi','Cameroon',
            'Central African Republic','Chad','Ivory Coast',
            'Democratic Republic of the Congo','Djibouti','eSwatini','Ethiopia',
            'Ghana','Guinea','Kenya','Lesotho','Liberia','Madagascar','Malawi',
            'Mali','Mauritania','Mozambique','Namibia','Niger','Nigeria',
            'Republic of the Congo','Rwanda','Senegal','Sierra Leone','Somalia',
            'South Africa','South Sudan','Sudan','Tanzania',
            'Uganda','Zambia','Zimbabwe')
asia <- c('Bangladesh','Myanmar','Cambodia','China','India','Indonesia',
          'Kazakhstan','Kyrgyzstan','Laos','Maldives','Nepal','Philippines',
          'Sri Lanka','Tajikistan','Thailand','East Timor','Turkmenistan',
          'Uzbekistan','Vietnam','Afghanistan','Pakistan','Mongolia')
e_and_e <- c('Albania','Armenia','Azerbaijan','Belarus','Bosnia and Herzegovina',
             'Cyprus','Georgia','Kosovo','North Macedonia','Moldova','Montenegro',
             'Russia','Republic of Serbia','Ukraine')
lac <- c('Bolivia','Brazil','Colombia','Cuba','Dominican Republic','Ecuador',
         'El Salvador','Guatemala','Haiti','Honduras','Jamaica','Mexico',
         'Nicaragua','Panama','Paraguay','Peru','Venezuela','Guyana')
me <- c('Egypt','Iraq','Jordan','Lebanon','Libya','Morocco','Syria',
        'Tunisia','West Bank and Gaza','Yemen')

# WB lending groups
low_income <- c('Afghanistan','Guinea-Bissau','Sierra Leone','Benin','Haiti',
                'Somalia','Burkina Faso','North Korea','South Sudan','Burundi',	
                'Liberia','Syria','Central African Republic','Madagascar','Tajikistan',
                'Chad','Malawi','Tanzania','Comoros','Mali',
                'Togo','Democratic Republic of the Congo','Mozambique','Uganda',
                'Eritrea','Nepal','Yemen','Ethiopia','Niger','Zimbabwe','Gambia',
                'Rwanda','Guinea','Senegal')
lmic <- c('Angola','Indonesia','Papua New Guinea','Bangladesh','Kenya','Philippines',
          'Bhutan','Kiribati','São Tomé and Principe','Bolivia','Kosovo','Solomon Islands',
          'Cabo Verde','Kyrgyzstan','Sri Lanka','Cambodia','Laos','Sudan',
          'Cameroon','Lesotho','eSwatini','Republic of the Congo','Mauritania',
          'East Timor','Ivory Coast','Federated States of Micronesia','Tunisia','Djibouti','Moldova',
          'Ukraine','Egypt','Mongolia','Uzbekistan','El Salvador','Morocco','Vanuatu',
          'Georgia','Myanmar','Vietnam','Ghana','Nicaragua','West Bank and Gaza',
          'Honduras','Nigeria','Zambia','India','Pakistan')
umic <- c('Albania','Fiji','Namibia','Algeria','Gabon','Nauru','American Samoa',
          'Grenada','Paraguay','Armenia','Guatemala','Peru','Azerbaijan',
          'Guyana','Romania','Belarus','Iran','Russia','Belize','Iraq','Samoa',
          'Bosnia and Herzegovina','Jamaica','Republic of Serbia','Botswana',
          'Jordan','South Africa','Brazil','Kazakhstan','Saint Lucia','Bulgaria',
          'Lebanon','Saint Vincent and the Grenadines','China','Libya','Suriname',
          'Colombia','North Macedonia','Thailand','Costa Rica','Malaysia','Tonga',
          'Cuba','Maldives','Turkey','Dominica','Marshall Islands','Turkmenistan',
          'Dominican Republic','Mauritius','Tuvalu','Equatorial Guinea','Mexico',
          'Venezuela','Ecuador','Montenegro')
high_income <- c('Andorra','Germany','Oman','Antigua and Barbuda','Gibraltar','Palau',
                 'Argentina','Greece','Panama','Aruba','Greenland','Poland','Australia',
                 'Guam','Portugal','Austria','Hong Kong','Puerto Rico','The Bahamas','Hungary',
                 'Qatar','Bahrain','Iceland','San Marino','Barbados','Ireland','Saudi Arabia',
                 'Belgium','Isle of Man','Seychelles','Bermuda','Israel','Singapore',
                 'British Virgin Islands','Italy','Sint Maarten (Dutch part)',
                 'Brunei','Japan','Slovakia','Canada','South Korea',
                 'Slovenia','Cayman Islands','Kuwait','Spain','Channel Islands','Latvia',
                 'Saint Kitts and Nevis','Chile','Liechtenstein','St. Martin (French part)',
                 'Croatia','Lithuania','Sweden','Curaçao','Luxembourg','Switzerland',
                 'Cyprus','Macao','Taiwan','Czechia','Malta','Trinidad and Tobago','Denmark',
                 'Monaco','Turks and Caicos Islands','Estonia','Netherlands',
                 'United Arab Emirates','Faroe Islands','New Caledonia','United Kingdom',
                 'Finland','New Zealand','United States','France',
                 'Northern Mariana Islands','Uruguay','French Polynesia','Norway',
                 'Virgin Islands (U.S.)')

# why do I need a list of non-countries? Because they are in a lot of datasets.
non_countries <- c("Arab world","Developing","East Asia & Pacific",
                   "East Asia & Pacific (excluding high income)","Euro area",
                   "Europe & Central Asia","Europe & Central Asia (excluding high income)",
                   "High income","High income: OECD","Latin America & Caribbean",
                   "Latin America & Caribbean (excluding high income)",
                   "Lower middle income","Low income","Middle East & North Africa",
                   "Middle East & North Africa (excluding high income)",
                   "Middle income","North America","South Asia","Sub-Saharan Africa",
                   "Sub-Saharan Africa (excluding high income)","Upper middle income",
                   "World",'BCEAO',"Eastern Caribbean","Eastern Caribbean Currency Union",
                   "All countries","Africa","Asia","Central Asia",
                   "East Asia","Europe","Europe (EU)","Gulf Cooperation Council",
                   "Latin America","Middle East and North Africa","North Africa",
                   "Southeast Asia-Pacific","Eastern Europe & Central Asia",
                   "EU + EFTA + North America","Low","Lower middle","Upper middle",
                   "High")

###############################################################################
# Plotting functions
###############################################################################
approved_colors <-c(
  '#002F6C', # USAID blue
  '#BA0C2F',  # USAID red
  '#6C6463',   # dark gray
  '#A7C6ED', # light blue
  '#651D32', # dark red
  '#CFCDC9' # light gray
) 

colors_USAID <- scale_color_manual(values=approved_colors)
fill_USAID <- scale_fill_manual(values=approved_colors)

theme_USAID <- theme( 
  axis.ticks = element_blank(), 
  axis.ticks.length = unit(0, units = "points"), 
  
  strip.text = element_blank(),
  strip.background = element_blank(),
  
  panel.border = element_blank(), 
  panel.grid = element_blank(), 
  panel.background = element_blank(), 
  plot.background = element_blank(),
  legend.key = element_blank(),
  text=element_text(family="Liberation Sans")) 

###############################################################################
# Utilities for PCA calculations
###############################################################################

### Function that removes columns that are NA for more than a given fraction of 
### USAID countries
trim_columns <- function(df,frac,keep_list=NULL) {
  # if no keep_list provided, include all countries
  if (is.null(keep_list)) {
    keep_list <- df$country
  }
  na_frac <- df %>% 
    filter(country %in% keep_list) %>%
    is.na %>%
    colMeans
  keep_names <- na_frac[na_frac <= (1-frac)] %>% names
  df %>% select(keep_names)
}

### Function that removes non-USAID countries for which more than a given 
### fraction of rows are missing. I'm willing to work a little harder to impute
### missing values for USAID countries, but I'm not going to spend that effort
### on Montserrat, for example.

trim_rows <- function(df,frac,keep_list=c()) {
  tmp <- df %>% filter(!country %in% keep_list)
  missing <- tmp %>% 
    select(-country) %>%
    is.na %>%
    rowMeans
  also_keep <- tmp$country[missing <= (1-frac)]
  df %>% filter(country %in% c(keep_list,also_keep))
}

### Scatterplot of PCA results
highlight_scatter <- function(df,highlight_countries,f=0.03) {
  plotme <- df %>%
    rename(pc1=2,pc2=3) %>%
    filter(!is.na(pc1) & !is.na(pc2)) %>%
    mutate(color=ifelse(pc1 < quantile(pc1,probs=f,na.rm=TRUE) | pc1 > quantile(pc1,probs=1-f,na.rm=TRUE) |
                          pc2 < quantile(pc2,probs=f,na.rm=TRUE) | pc2 > quantile(pc2,probs=1-f,na.rm=TRUE),'a','c'),
           color=ifelse(country %in% highlight_countries,'b',color),
           highlight= (color != 'c'),
           label=ifelse(highlight,country,NA))
  
  ggplot(plotme,aes(x=pc1,y=pc2,color=color,label=label)) +
    geom_point(size=1) +
    geom_point(data=filter(plotme,highlight),size=3,shape=1) +
    theme_USAID + colors_USAID +
    theme(legend.position = 'none') +
    geom_text_repel(size=5) 
}


pc_scatter <- function(df,highlight_countries,f=0.03) {
  df %>%
    select(country,PC1,PC2) %>%
    highlight_scatter(highlight_countries,f) +
    xlab('PC1') + ylab('PC2')
}  


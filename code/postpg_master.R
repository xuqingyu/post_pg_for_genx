library('tidyverse')
library('yaml')


# workingfolder <- "/Users/qingyuxu/Dropbox (Princeton)/PJM/GenX_PJM_Data/2022_PJM_CE/"
workingfolder <- "/tigress/qingyux/GenX/PJM/2022_PJM_CE/"
foldername_fn <- "2022-09-09_names_createnewfolder.csv"
miscfoldername <- "PJM_misc_files/"
files_to_copy_from_misc <- c('Capacity_reserve_margin_slack.csv',
                            'CO2_cap_slack.csv',
                            'Energy_share_requirement_slack.csv')
name_of_shell = 'Run_myopic.sh'
name_of_runfile = 'Run_myopic_ce.jl'
years = c("2025","2030","2035")
n_totalhour = 4368
tofolder = 'pjm_ce_ira_final/'
frompgfolder = 'PJM_Final_raw/'
fromfolder = 'PJM_Final_raw_modified'
ngprice_variation = 'census_division' # chose from 'national' and 'census_division'
ngpriceadder = 3
min_trans_upperbound = cbind(year = years, min = c(750, 750,750)) %>% 
  as_tibble() %>% 
  mutate(min = as.numeric(min))



source('./code/postpg_header.R')
source('./code/postpg_backupcopyandpastefolders.R')
source('./code/postpg_pjm_ce.R')

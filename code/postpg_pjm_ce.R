
# Deep Decarbonization ----


temp_zonelist = c('MIS_Central','MIS_East','NY_East','NY_West',
                  'PJM_COMD','PJM_Delaware','PJM_Dom','PJM_NJCoast','PJM_NJLand',
                  'PJM_PECO','PJM_SMAC','PJM_WEST','PJM_WestMAC',
                  'SC_TVA','SC_VACA')

CarbonTaxTable = as_tibble(cbind(TaxLevel = c(1:9),
                                 `2025` = c(4.5, 5.0, 5.5, 6.0, 6.5, 7.5, 9.5, 
                                            12.0, 14.5),
                                 `2030` = c(7.0, 8.5, 10.5, 14.5, 22.5, 38.0, 70.0, 
                                            135.0, 285.0)))
EnergyCreditTable = as_tibble(cbind(CreditLevel = c(1:9),
                                 `2025` = c(2.0, 3.5, 4.0, 10.5, 11.5, 13.5, 16.0, 
                                            18.0, 20.5),
                                 `2030` = c(6.5, 9.5, 12.5, 16.5, 22.5, 31.5, 46.5, 
                                            73.5, 124.5)))


for (y in years) {
  for (i in 1:nrow(foldernames)) {
    new_folder = paste0(workingfolder,tofolder,'/', y ,'/',
                        foldernames$case_id[i],'_', y ,'_',
                        foldernames$case_description[i])
    new_settings_folder = paste0(workingfolder,tofolder,'/', y ,'/',
                                 foldernames$case_id[i],'_', y ,'_',
                                 foldernames$case_description[i],'/Settings')
    setting = read_yaml(paste0(new_settings_folder,'/genx_settings.yml'))
    # Modify ER cases
    setting$CO2LoadRateCap = as.integer(0)
    ERStringlist = paste(seq(40, 100, 5),'ER',sep = "")
    ERRatelist = 1-seq(40, 100, 5)/100
    dd_region_member <- tibble(`Region_description` = temp_zonelist,
                               `Network_zones` =   paste('z',c(1:length(temp_zonelist)), sep ='') ,
                               `CO_2_Cap_Zone_1` = c(0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0), # PJM
                               `CO_2_Cap_Zone_2` = c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), # MISO
                               `CO_2_Cap_Zone_3` = c(0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), # NYISO
                               `CO_2_Cap_Zone_4` = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1))
    if (grepl('ER', foldernames$case_description[i])) {
      for (j in 1:length(ERStringlist)) {
        if (grepl(ERStringlist[j], foldernames$case_description[i])) {
          r = ERRatelist[j]
          if (y %in% c(2022, 2025, 2030)) {
            carbon_intensity <- tibble(`Region_description` = temp_zonelist, 
                                       MLoadRate = rep(round(0.415 - (0.415-0.607*r)/(2030-2019)*(as.numeric(y)-2019),3), 
                                                       length(temp_zonelist)))
          }
          # if (y %in% c(2040, 2050)) {
          #   carbon_intensity <- tibble(`Region_description` = temp_zonelist, 
          #                              MLoadRate = rep(round(0.607*r- (0.607*r-0.000)/(2050-2030)*(as.numeric(y)-2030),3),
          #                                              length(temp_zonelist)))
          # }        
          DD_region_cap <- carbon_intensity %>%
            mutate(CO2Zone = NA) %>%
            mutate(CO2Zone = replace(CO2Zone, grepl("PJM_",`Region_description`), 'CO_2_Max_LoadRate_1')) %>%
            mutate(CO2Zone = replace(CO2Zone, grepl("MIS_",`Region_description`), 'CO_2_Max_LoadRate_2')) %>%
            mutate(CO2Zone = replace(CO2Zone, grepl("NY_",`Region_description`), 'CO_2_Max_LoadRate_3')) %>%
            mutate(CO2Zone = replace(CO2Zone, grepl("SC_",`Region_description`), 'CO_2_Max_LoadRate_4')) %>%
            pivot_wider(names_from = CO2Zone, values_from = MLoadRate) %>%
            select(`Region_description`, `CO_2_Max_LoadRate_1`,`CO_2_Max_LoadRate_2`,`CO_2_Max_LoadRate_3`,`CO_2_Max_LoadRate_4`)
          DD_carbon_table <- left_join(dd_region_member, DD_region_cap, by = c('Region_description')) %>%
            select(`Region_description`, Network_zones, CO_2_Cap_Zone_1,CO_2_Cap_Zone_2,CO_2_Cap_Zone_3, CO_2_Cap_Zone_4,
                   CO_2_Max_LoadRate_1,CO_2_Max_LoadRate_2,CO_2_Max_LoadRate_3,CO_2_Max_LoadRate_4)
          DD_carbon_table[is.na(DD_carbon_table)]<-0
          write_csv(DD_carbon_table, paste0(new_folder,"/Inputs/CO2_loadrate_cap.csv"))
          file.copy(paste0(misc_filefolder,'/CO2_loadrate_cap_slack.csv',
                           paste0(new_folder,"/Inputs/CO2_loadrate_cap_slack.csv")))
        }
      }
      setting$CO2LoadRateCap = as.integer(1)
    }
    
    # Modify CES cases
    setting$EnergyShareRequirement = as.integer(1)
    CESStringlist = paste(seq(40,100,5),'CES',sep = "")
    CESRatelist = seq(40,100,5)/100
    if (grepl('CES', foldernames$case_description[i])) {
      for (j in 1:length(CESStringlist)) {
        if (grepl(CESStringlist[j], foldernames$case_description[i])) {
          if (y %in% c(2022, 2025, 2030)) {
            temptarget = (CESRatelist[j] - .4)/(2030-2020)*(as.numeric(y) - 2020) + 0.4
          }
          # if (y %in% c(2040, 2050)) {
          #   temptarget = (1 - CESRatelist[j])/(2050-2030)*(as.numeric(y) - 2030)
          # }          
          ces_table = read_csv(paste0(new_folder,"/Inputs/Energy_share_requirement.csv"), 
                               col_types = cols()) %>%
            mutate(ESR_2 = replace(ESR_2, grepl('PJM_', `Region_description`), temptarget),
                   ESR_5 = replace(ESR_5, grepl('NY_', `Region_description`), temptarget),
                   ESR_7 = replace(ESR_5, grepl('MIS_', `Region_description`), temptarget),
                   ESR_8 = replace(ESR_8, grepl('SC_', `Region_description`), temptarget)) %>%
            write_csv(paste0(new_folder,"/Inputs/Energy_share_requirement.csv"))
        }
      }
    }
    
    # Min Tech 
    setting$MinCapReq = as.integer(1)
    if (grepl('nonuclearretire', foldernames$case_description[i])) {
      if (y == 2025) {
        min_tech = read_csv(paste0(misc_filefolder,'/mincap2025_yesnuclearsupport.csv'), 
                            col_types = cols())
      }
      if (y == 2030) {
        min_tech = read_csv(paste0(misc_filefolder,'/mincap2030_yesnuclearsupport.csv'), 
                            col_types = cols())
      }      
    } else {
      if (y == 2025) {
        min_tech = read_csv(paste0(misc_filefolder,'/mincap2025_nonuclearsupport.csv'), 
                            col_types = cols())
      }
      if (y == 2030) {
        min_tech = read_csv(paste0(misc_filefolder,'/mincap2030_nonuclearsupport.csv'), 
                            col_types = cols())
      }
    }
    
    # Modify Clean Capacity mechanism
    CAPCESStringlist = paste0('CECAP',c(1:9))
    CAPCESRatelist = seq(55,95,5)/100
    if (grepl('CECAP', foldernames$case_description[i])) {
      setting$CECAP = as.integer(1)
      for (j in 1:length(CAPCESStringlist)) {
        if (grepl(CAPCESStringlist[j], foldernames$case_description[i])) {
          if (y %in% c(2022, 2025, 2030)) {
            temptarget = (CAPCESRatelist[j] - .4)/(2030-2020)*(as.numeric(y) - 2020) + 0.4
          }
          setting$CECAPTarget = temptarget
          if (y == 2025) {
            additionalmintech = as_tibble(cbind(MinCapReqConstraint = as.integer(c(22, 23, 24, 25)),
                                                Constraint_Description = c('MISO_CleanPower', 'NY_CleanPower', 'PJM_CleanPower', 'SERC_CleanPower'),
                                                Min_MW = temptarget * c(38819, 16762, 94813, 69329),
                                                PriceCap = c(99999,99999,99999,99999)))
          }
          if (y == 2030) {
            additionalmintech = as_tibble(cbind(MinCapReqConstraint = as.integer(c(22, 23, 24, 25)),
                                                Constraint_Description = c('MISO_CleanPower', 'NY_CleanPower', 'PJM_CleanPower', 'SERC_CleanPower'),
                                                Min_MW = temptarget * c(42560, 18031, 102334, 74212),
                                                PriceCap = c(99999,99999,99999,99999)))
          }
        }
      }
      min_tech = rbind(min_tech, additionalmintech)
    }
    write_csv(min_tech, paste0(new_folder,"/Inputs/Minimum_capacity_requirement.csv"))   
    
    # Max Tech 
    setting$MaxCapReq = as.integer(0)
    if (grepl('nonewgas', foldernames$case_description[i])) {
      max_tech = read_csv(paste0(new_folder,"/Inputs/Maximum_capacity_limit.csv"), 
                          col_types = cols()) %>%
        mutate(Max_MW = 0) %>%
        write_csv(paste0(new_folder,"/Inputs/Maximum_capacity_limit.csv"))
      setting$MaxCapReq = as.integer(1)
    } 
    if (grepl('nglimit', foldernames$case_description[i])) {
      max_tech = read_csv(paste0(misc_filefolder,'/max_cap_limitedgas.csv'), 
                          col_types = cols()) %>%
        write_csv(paste0(new_folder,"/Inputs/Maximum_capacity_limit.csv"))
      setting$MaxCapReq = as.integer(1)
    }
    
    # Network
    interregionalline = c('MIS_Central_to_PJM_COMD',
                          'MIS_Central_to_PJM_WEST',
                          'MIS_Central_to_SC_TVA',
                          'MIS_East_to_PJM_WEST',
                          'NY_East_to_PJM_NJLand',
                          'NY_West_to_PJM_WestMAC',
                          'PJM_Dom_to_SC_VACA',
                          'PJM_WEST_to_SC_TVA',
                          'PJM_WEST_to_SC_VACA')
    if (grepl('nointertrans', foldernames$case_description[i])) {
      network = read_csv(paste0(new_folder,"/Inputs/Network.csv"), 
                          col_types = cols());
      interregional_rows = which(network$transmission_path_name %in% interregionalline)
      network$Line_Max_Reinforcement_MW[interregional_rows] = 0
      network <- network %>%
        write_csv(paste0(new_folder,"/Inputs/Network.csv"))
    }
    if (grepl('halfintertrans', foldernames$case_description[i])) {
      network = read_csv(paste0(new_folder,"/Inputs/Network.csv"), 
                         col_types = cols());
      interregional_rows = which(network$transmission_path_name %in% interregionalline)
      network$Line_Max_Reinforcement_MW[interregional_rows] = 0.5*network$Line_Max_Reinforcement_MW[interregional_rows]
      network <- network %>%
        write_csv(paste0(new_folder,"/Inputs/Network.csv"))
    }
    
    # Generator data.csv
    gen_info = read_csv(paste0(new_folder,"/Inputs/Generators_data.csv"), 
                          col_types = cols())
    if (y == 2030) {
      mga = read_csv(paste0(misc_filefolder,'/mga_columns.csv'), 
                     col_types = cols())
      gen_info <- gen_info %>%
        left_join(mga, by = 'Resource')
    }
    if (grepl('CECAP', foldernames$case_description[i])) {
      gen_info <- gen_info %>%
        mutate(MinCapTag_22 = MISO_CleanPower * CF_prior,
               MinCapTag_23 = NY_CleanPower * CF_prior,
               MinCapTag_24 = PJM_CleanPower * CF_prior,
               MinCapTag_25 = SERC_CleanPower * CF_prior)
    }
    if (grepl('_EnergyCredit', foldernames$case_description[i])) {
      gen_info <- gen_info %>%
        mutate(EC_Eligibility_1 = MISO_CleanPower,
               EC_Eligibility_2 = NY_CleanPower,
               EC_Eligibility_3 = PJM_CleanPower,
               EC_Eligibility_4 = SERC_CleanPower)
    } 
    
    write_csv(gen_info, paste0(new_folder,"/Inputs/Generators_data.csv"))
    
    # Energy_credit.csv
    if (grepl('_EnergyCredit', foldernames$case_description[i])){
      setting$`EnergyCredit` = as.integer(1)
      for (lev in c(1:9)) {
        if (grepl(paste0('_EnergyCredit',lev), foldernames$case_description[i])) {
          if (y == 2025) {
            ceclevel = EnergyCreditTable$`2025`[lev]
          } 
          if (y == 2030) {
            ceclevel = EnergyCreditTable$`2030`[lev]
          }           
        }
      }
      EC_Table = as_tibble(cbind(EnergyCreditCategory = c(1:4),
                                 category_description = c('MISO_CleanEnergyCredit',
                                                          'NY_CleanEnergyCredit',
                                                          'PJM_CleanEnergyCredit',
                                                          'SERC_CleanEnergyCredit'),
                                 Energy_Credit_per_MWh = rep(ceclevel, 4))) %>%
        write_csv(paste0(new_folder,"/Inputs/Energy_credit.csv"))
    }
    
    # Carbon Tax
    if (grepl('_CarbonTax', foldernames$case_description[i])){
      setting$`CarbonTax` = as.integer(1)
      for (lev in c(1:9)) {
        if (grepl(paste0('_CarbonTax',lev), foldernames$case_description[i])) {
          if (y == 2025) {
            taxlevel = CarbonTaxTable$`2025`[lev]
          } 
          if (y == 2030) {
            taxlevel = CarbonTaxTable$`2030`[lev]
          }           
        }
      }
      Tax_Table = as_tibble(cbind(`Region description` = temp_zonelist,
                                  Network_zones = paste0('z',c(1:15)),
                                  CO2Tax = rep(taxlevel, 15))) %>%
        write_csv(paste0(new_folder,"/Inputs/Carbon_tax.csv"))
    }
    
    # Write yaml
    write_yaml(setting, paste0(new_settings_folder,'/genx_settings.yml'))
  }
}
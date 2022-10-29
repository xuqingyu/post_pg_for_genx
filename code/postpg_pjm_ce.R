
# Deep Decarbonization ----


temp_zonelist = c('MIS_Central','MIS_East','NY_East','NY_West',
                  'PJM_COMD','PJM_Delaware','PJM_Dom','PJM_NJCoast','PJM_NJLand',
                  'PJM_PECO','PJM_SMAC','PJM_WEST','PJM_WestMAC',
                  'SC_TVA','SC_VACA')

CarbonTaxTable = as_tibble(cbind(TaxLevel = c(1:9),
                                 `2025` = c(4.30, 4.35, 4.39, 4.46,  4.52,  4.93,  8.16, 10.98, 14.11),
                                 `2030` = c(4.94, 5.36, 5.97, 7.60, 17.16, 37.10, 69.43, 133.22, 262.97)))
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
    # Modify Fuel price
    originalfuel = read_csv(paste0(new_folder,"/Inputs/Fuels_data.csv"),
                            col_types = cols())
    modifiedfuel <- originalfuel
    fuelnames = colnames(modifiedfuel)
    fuelrows = c(2:nrow(modifiedfuel))
    for (k in 2:ncol(modifiedfuel)) {
      mul = 1
      if (y == 2025) {
        if (grepl('highfuelcost',foldernames$case_description[i])) {
          if (grepl('_naturalgas', fuelnames[k])) {
            mul = 1.824
          }
          if (grepl('_coal', fuelnames[k])) {
            mul = 1.111
          }
          if (grepl('_distillate', fuelnames[k])) {
            mul = 1.313
          }
        }
        if (grepl('mediumfuelcost',foldernames$case_description[i])) {
          if (grepl('_naturalgas', fuelnames[k])) {
            mul = 1.452
          }
          if (grepl('_coal', fuelnames[k])) {
            mul = 1.061
          }
          if (grepl('_distillate', fuelnames[k])) {
            mul = 1.198
          }
        }
        if (grepl('lowfuelcost',foldernames$case_description[i])) {
          if (grepl('_naturalgas', fuelnames[k])) {
            mul = 1.043
          }
          if (grepl('_coal', fuelnames[k])) {
            mul = 0.995
          }
          if (grepl('_distillate', fuelnames[k])) {
            mul = 1.060
          }
        }
      }
      if (y == 2030) {
        if (grepl('highfuelcost',foldernames$case_description[i])) {
          if (grepl('_naturalgas', fuelnames[k])) {
            mul = 1.490
          }
          if (grepl('_coal', fuelnames[k])) {
            mul = 1.054
          }
          if (grepl('_distillate', fuelnames[k])) {
            mul = 1.103
          }
        }
        if (grepl('mediumfuelcost',foldernames$case_description[i])) {
          if (grepl('_naturalgas', fuelnames[k])) {
            mul = 1.000
          }
          if (grepl('_coal', fuelnames[k])) {
            mul = 1.000
          }
          if (grepl('_distillate', fuelnames[k])) {
            mul = 1.000
          }
        }
        if (grepl('lowfuelcost',foldernames$case_description[i])) {
          if (grepl('_naturalgas', fuelnames[k])) {
            mul = 0.786
          }
          if (grepl('_coal', fuelnames[k])) {
            mul = 0.957
          }
          if (grepl('_distillate', fuelnames[k])) {
            mul = 0.962
          }
        }
      }
      if (y == 2035) {
        if (grepl('highfuelcost',foldernames$case_description[i])) {
          if (grepl('_naturalgas', fuelnames[k])) {
            mul = 1.615
          }
          if (grepl('_coal', fuelnames[k])) {
            mul = 1.068
          }
          if (grepl('_distillate', fuelnames[k])) {
            mul = 1.123
          }
        }
        if (grepl('mediumfuelcost',foldernames$case_description[i])) {
          if (grepl('_naturalgas', fuelnames[k])) {
            mul = 1.000
          }
          if (grepl('_coal', fuelnames[k])) {
            mul = 1.000
          }
          if (grepl('_distillate', fuelnames[k])) {
            mul = 1.000
          }
        }
        if (grepl('lowfuelcost',foldernames$case_description[i])) {
          if (grepl('_naturalgas', fuelnames[k])) {
            mul = 0.769
          }
          if (grepl('_coal', fuelnames[k])) {
            mul = 0.941
          }
          if (grepl('_distillate', fuelnames[k])) {
            mul = 0.936
          }
        }
      }
      modifiedfuel[fuelrows, k] = mul * modifiedfuel[fuelrows, k]
    }
    write_csv(modifiedfuel, paste0(new_folder,"/Inputs/Fuels_data.csv"))
    
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
          if (y %in% c(2022, 2025, 2030, 2035)) {
            carbon_intensity <- tibble(`Region_description` = temp_zonelist, 
                                       MLoadRate = rep(max(round(0.415 - (0.415-0.607*r)/(2030-2019)*(as.numeric(y)-2019),3),0.03), 
                                                       length(temp_zonelist)))
            # Note that 0.03 means 95% emission reduction compared to 2005
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
          file.copy(paste0(misc_filefolder,'/CO2_loadrate_cap_slack.csv'),
                           paste0(new_folder,"/Inputs/CO2_loadrate_cap_slack.csv"))
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
          if (y %in% c(2022, 2025, 2030, 2035)) {
            temptarget = min((CESRatelist[j] - .4)/(2030-2020)*(as.numeric(y) - 2020) + 0.4,1)
          }
          # if (y %in% c(2040, 2050)) {
          #   temptarget = (1 - CESRatelist[j])/(2050-2030)*(as.numeric(y) - 2030)
          # }
          ces_table = read_csv(paste0(new_folder,"/Inputs/Energy_share_requirement.csv"), 
                               col_types = cols()) %>%
            mutate(ESR_2 = replace(ESR_2, grepl('PJM_', `Region_description`), temptarget),
                   ESR_5 = replace(ESR_5, grepl('NY_', `Region_description`), temptarget),
                   ESR_7 = replace(ESR_7, grepl('MIS_', `Region_description`), temptarget),
                   ESR_8 = replace(ESR_8, grepl('SC_', `Region_description`), temptarget)) %>%
            write_csv(paste0(new_folder,"/Inputs/Energy_share_requirement.csv"))
        }
      }
    }
    if (grepl('NJ100', foldernames$case_description[i])) {
      # if (y ==2025) {njrpstarget = 0.45; njcestarget = 0.50}
      # if (y ==2030) {njrpstarget = 0.60; njcestarget = 0.75}
      if (y ==2035) {
        njrpstarget = 0.50; 
        njcestarget = 1.00;
        njcesinstatetarget = 0;
        ces_table = read_csv(paste0(new_folder,"/Inputs/Energy_share_requirement.csv"), 
                             col_types = cols()) %>%
          mutate(ESR_1 = replace(ESR_1, grepl('PJM_NJ', `Region_description`), njrpstarget)) %>%
          mutate(ESR_9 = 0, ESR_10 = 0) %>%
          mutate(ESR_9 = replace(ESR_9, grepl('PJM_NJ', `Region_description`), njcestarget),
                 ESR_10 = replace(ESR_10, grepl('PJM_NJ', `Region_description`), njcesinstatetarget),) %>%
          write_csv(paste0(new_folder,"/Inputs/Energy_share_requirement.csv"))
        esr_slack = read_csv(paste0(new_folder,"/Inputs/Energy_share_requirement_slack.csv"), 
                             col_types = cols())
        njrows = as_tibble(cbind(ESR_Constraint = c('ESR_9','ESR_10'), 
                                Constraint_description = c('NJ_InstateRPS', 'NJ_InstateCES'),
                                PriceCap = c(1500, 1500)))
        esr_slack <- rbind(esr_slack, njrows) %>%
          write_csv(paste0(new_folder,"/Inputs/Energy_share_requirement_slack.csv"))
      }
    }
    
    # CO2 Credit
    if (grepl('ira_', foldernames$case_description[i])) {
      co2credit <- read_csv(paste0(misc_filefolder,'/CO2_credit_ira.csv'), 
                            col_types = cols())
    } else {
      co2credit <- read_csv(paste0(misc_filefolder,'/CO2_credit.csv'), 
                            col_types = cols())      
    }
    write_csv(co2credit, paste0(new_folder,"/Inputs/CO2_credit.csv"))
    rm(co2credit)
    
    
    # Min Tech 
    setting$MinCapReq = as.integer(1)
    if (grepl('ira_', foldernames$case_description[i])) {
      if (y == 2025) {
        min_tech = read_csv(paste0(misc_filefolder,'/mincap2025_yesnuclearsupport.csv'), 
                            col_types = cols())
      }
      if (y == 2030) {
        min_tech = read_csv(paste0(misc_filefolder,'/mincap2030_yesnuclearsupport.csv'), 
                            col_types = cols())
      } 
      if (y == 2035) { # Nuclear supports ends in 2030
        if (grepl('nuclearsupport', foldernames$case_description[i])) {
          min_tech = read_csv(paste0(misc_filefolder,'/mincap2035_yesnuclearsupport.csv'), 
                              col_types = cols())
        } else {
          min_tech = read_csv(paste0(misc_filefolder,'/mincap2035_nonuclearsupport.csv'), 
                              col_types = cols())
        }
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
      if (y == 2035) {
        min_tech = read_csv(paste0(misc_filefolder,'/mincap2035_nonuclearsupport.csv'), 
                            col_types = cols())
      } 
    }
    if (grepl('NJ100', foldernames$case_description[i])) {
      if (!grepl('nonjnuclearsupport', foldernames$case_description[i])) {
        if (y == 2035) {
          njnuclearrow = which(min_tech$Constraint_Description == 'NJ_Nuclear')
          min_tech$Min_MW[njnuclearrow] = 3625
        }
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
                                                PriceCap = c(1000000,1000000,1000000,1000000)))
          }
          if (y == 2030) {
            additionalmintech = as_tibble(cbind(MinCapReqConstraint = as.integer(c(22, 23, 24, 25)),
                                                Constraint_Description = c('MISO_CleanPower', 'NY_CleanPower', 'PJM_CleanPower', 'SERC_CleanPower'),
                                                Min_MW = temptarget * c(42560, 18031, 102334, 74212),
                                                PriceCap = c(1000000,1000000,1000000,1000000)))
          }
        }
      }
      min_tech = rbind(min_tech, additionalmintech)
    }
    write_csv(min_tech, paste0(new_folder,"/Inputs/Minimum_capacity_requirement.csv"))   
    
    # Max Tech 
    # setting$MaxCapReq = as.integer(0)
    # if (grepl('nonewgas', foldernames$case_description[i])) {
    #   max_tech = read_csv(paste0(new_folder,"/Inputs/Maximum_capacity_limit.csv"), 
    #                       col_types = cols()) %>%
    #     mutate(Max_MW = 0, PriceCap = 1000000) %>%
    #     write_csv(paste0(new_folder,"/Inputs/Maximum_capacity_limit.csv"))
    #   setting$MaxCapReq = as.integer(1)
    # } 
    # if (grepl('nglimit', foldernames$case_description[i])) {
    #   max_tech = read_csv(paste0(misc_filefolder,'/max_cap_limitedgas.csv'), 
    #                       col_types = cols()) %>%
    #     write_csv(paste0(new_folder,"/Inputs/Maximum_capacity_limit.csv"))
    #   setting$MaxCapReq = as.integer(1)
    # }
    setting$MaxCapReq = as.integer(1)
    if (y == 2025) {
        max_tech = read_csv(paste0(misc_filefolder,'/max_cap_ira_2025.csv'),
                            col_types = cols()) %>%
          write_csv(paste0(new_folder,"/Inputs/Maximum_capacity_limit.csv"))
    }
    if (y == 2030) {
      max_tech = read_csv(paste0(misc_filefolder,'/max_cap_ira_2030.csv'),
                          col_types = cols()) %>%
        write_csv(paste0(new_folder,"/Inputs/Maximum_capacity_limit.csv"))
    }
    
    if (y == 2035) {
      # if (grepl('NJ100', foldernames$case_description[i])) {
      #   max_tech = read_csv(paste0(misc_filefolder,'/max_cap_ira_2035_nj100.csv'),
      #                       col_types = cols()) %>%
      #     write_csv(paste0(new_folder,"/Inputs/Maximum_capacity_limit.csv"))        
      # } else {
        max_tech = read_csv(paste0(misc_filefolder,'/max_cap_ira_2035.csv'),
                            col_types = cols()) %>%
          write_csv(paste0(new_folder,"/Inputs/Maximum_capacity_limit.csv"))
      # }
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
    if (grepl('NJ100', foldernames$case_description[i])) {
      if (y == 2035) {
        # New Jersey network unlimited upper bound
        network = read_csv(paste0(new_folder,"/Inputs/Network.csv"), 
                           col_types = cols());
        njland_coast = which(network$transmission_path_name == 'PJM_NJCoast_to_PJM_NJLand')
        network$Line_Max_Reinforcement_MW[njland_coast] = 1e5
        network <- network %>%
          write_csv(paste0(new_folder,"/Inputs/Network.csv"))
      }
    }    
    if (grepl('unlimitedtrans', foldernames$case_description[i])) {
      if (y == 2035) {
        network = read_csv(paste0(new_folder,"/Inputs/Network.csv"), 
                           col_types = cols());
        network$Line_Max_Reinforcement_MW = 1e5
        network <- network %>%
          write_csv(paste0(new_folder,"/Inputs/Network.csv"))
      }
    }    
    # Generator data.csv
    gen_info = read_csv(paste0(new_folder,"/Inputs/Generators_data.csv"), 
                          col_types = cols())
    # Add nc OSW column
    ncoswrows = which(grepl('offshore|offshore',gen_info$Resource) &
                        gen_info$region == 'SC_VACA')
    gen_info <- gen_info %>%
      mutate(MinCapTag_22 = as.integer(0)) %>%
      mutate(MinCapTag_22 = replace(MinCapTag_22, ncoswrows, as.integer(1)))
      
    if (y == 2030) {
      mga = read_csv(paste0(misc_filefolder,'/mga_columns.csv'), 
                     col_types = cols())
      gen_info <- gen_info %>%
        left_join(mga, by = 'Resource')
    }
    if (y == 2035) {
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
    # Retrofit and Repowering
    for (x in c('retrofitngcc', 'retrofitngct','repowercoal')) {
      if (x %in% c('retrofitngcc', 'retrofitngct')) {
        cost_col = grep('Retro1_Inv_Cost_per_MWyr|Retro2_Inv_Cost_per_MWyr',colnames(gen_info))
      }
      if (x %in% c('repowercoal', 'retrofitngct')) {
        cost_col = grep('Retro1_Inv_Cost_per_MWyr',colnames(gen_info))
      }
      retrofit_row = which(grepl(x, gen_info$Resource))
      original_inv_cost_col = grep('^Inv_Cost_per_MWyr',colnames(gen_info))
      gen_info[retrofit_row, cost_col] <- gen_info[retrofit_row, original_inv_cost_col]
      gen_info[retrofit_row, original_inv_cost_col] <- 0
    }
    
    if (grepl('NJ100', foldernames$case_description[i])) {
      if (y == 2035) {
        caprescols = grep('CapRes', colnames(gen_info))
        njcoalngccrows = setdiff(intersect(grep('natural_gas|coal|petroleum|naturalgas_',gen_info$Resource),
                                           grep('PJM_NJ',gen_info$region)),
                                 grep('naturalgas_ccccs', gen_info$Resource))
        gen_info[njcoalngccrows, caprescols] <- 0
        gen_info <- gen_info %>%
          mutate(ESR_9 = (grepl('PJM_NJ',gen_info$region) * ESR_2 + 
                            (!grepl('PJM_NJ',gen_info$region)) * ESR_1), # NJ's CES only supports renewable and instate clean firm capacity
                 ESR_10 = grepl('PJM_NJ',gen_info$region) * ESR_2) # Instate CES
      }
    }    
    # Fix Min Power of on thermal unit
    nonthermals = which(gen_info$THERM == 0)
    gen_info$Min_Power[nonthermals] <- 0
    
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
      setting$`CO2Tax` = as.integer(1)
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
        write_csv(paste0(new_folder,"/Inputs/CO2_tax.csv"))
    }
    
    # Max Investment
    setting$MaxInvReq = as.integer(1)
    if (y == 2025) {
      max_inv = read_csv(paste0(misc_filefolder,'/max_inv_2025.csv'),
                          col_types = cols()) %>%
        write_csv(paste0(new_folder,"/Inputs/Maximum_investment_limit.csv"))
    }
    if (y == 2030) {
      max_inv = read_csv(paste0(misc_filefolder,'/max_inv_2030.csv'),
                         col_types = cols()) %>%
        write_csv(paste0(new_folder,"/Inputs/Maximum_investment_limit.csv"))
    }
    if (y == 2035) {
      max_inv = read_csv(paste0(misc_filefolder,'/max_inv_2035.csv'),
                         col_types = cols()) %>%
        write_csv(paste0(new_folder,"/Inputs/Maximum_investment_limit.csv"))
    }
    
    # Time experience
    timetext = "23:00:00"
    memerorypercore = '15G'
    cpu = '8'
    if (grepl('Y1', foldernames$case_description[i])) {
      setting$OperationWrapping = as.integer(0)
      timetext = "23:59:00"
      memerorypercore = '30G'
      if (grepl('Y1w', foldernames$case_description[i])) {
        setting$OperationWrapping = as.integer(1)
        # change the rep period and other so that 
        # the full year data can also work with wraping
        t_load = read_csv(paste0(new_folder,"Load_data.csv"), 
                          col_types = cols())
        t_load$Timesteps_per_Rep_Period[1] <- 8760
        t_load$Sub_Weights[1] <- 8760
        write_csv(t_load, paste0(new_folder,"Load_data.csv"), na = "")
        # copy the Period_map.csv
        periodmap = read_csv(paste0(misc_filefolder,'/Period_map_y1w.csv'),
                           col_types = cols()) %>%
          write_csv(paste0(new_folder,"/Inputs/Period_map.csv"))
        # copy the representative point file
        reppoint = read_csv(paste0(misc_filefolder,'/Representative_Period_y1w.csv'),
                             col_types = cols()) %>%
          write_csv(paste0(new_folder,"/Inputs/Representative_Period.csv"))
      }
    }
    if (grepl('D1|D2|D3|D4|D7|W1', foldernames$case_description[i])) {
      timetext = "2:00:00"
    }
    if (grepl('D14|W2', foldernames$case_description[i])) {
      timetext = "2:00:00"
    }
    if (grepl('D21|D28|W3|W4|M1', foldernames$case_description[i])) {
      timetext = "6:00:00"
    }
    if (grepl('D56|W8|M2', foldernames$case_description[i])) {
      timetext = "8:00:00"
    }
    if (grepl('D112|D224|W16|W32|M4|M8', foldernames$case_description[i])) {
      timetext = "23:59:00"
    }
    if (grepl('D364|W52|M13', foldernames$case_description[i])) {
      timetext = "23:59:00"
      memerorypercore = '20G'
    }
    modifysh <- file(paste0(new_folder,'/',name_of_shell))
    writeLines(c(
      "#!/bin/bash",
      paste0("#SBATCH --job-name=",substr(foldernames$case_id[i],1,6),"              # create a short name for your job"),
      "#SBATCH --nodes=1                           # node count",
      "#SBATCH --ntasks=1                          # total number of tasks across all nodes",
      # "#SBATCH --cpus-per-task=8                   # cpu-cores per task (>1 if multi-threaded tasks)",
      paste0("#SBATCH --cpus-per-task=",cpu,              "      # cpu-cores per task (>1 if multi-threaded tasks)"),
      # "#SBATCH --mem-per-cpu=15G                    # memory per cpu-core",
      paste0("#SBATCH --mem-per-cpu=", memerorypercore,              "      # memory per cpu-core"),
      # "#SBATCH --time=23:00:00                     # total run time limit (HH:MM:SS)",
      paste0("#SBATCH --time=",timetext,              "      # total run time limit (HH:MM:SS)"),
      "#SBATCH --output=\"test.out\" ",
      "#SBATCH --error=\"test.err\" ",
      "#SBATCH --mail-type=end                    # notifications for job done & fail",
      "#SBATCH --mail-user=qingyux@princeton.edu  # send-to address",
      "module purge",
      "module add julia/1.6.1",
      "module add CPLEX/12.9.0",
      "module add gurobi/8.1.1",
      paste0("julia --project=\"/home/qingyux/GenX\" ", name_of_runfile),
      "date"
    ), modifysh)
    close(modifysh)
    
    # Write yaml
    write_yaml(setting, paste0(new_settings_folder,'/genx_settings.yml'))
  }
}
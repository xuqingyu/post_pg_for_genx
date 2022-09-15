# Copy the fodler and change the name of the folder
# Change the case id and case description
# Common for all cases


# Step 1 backup the files
file.copy(paste(pgresult, list.files(pgresult) , sep = '/'), 
          fromfolderpath, recursive = T, overwrite = T)

origin_folder_list = unique(select(foldernames,c(origin_case_id,origin_case_description)))
for (y in years) {
  for (j in 1:dim(origin_folder_list)[1]) {
    origin_folder = paste0(workingfolder,fromfolder,'/', y, '/', 
                           origin_folder_list$origin_case_id[j],"_", y, "_",origin_folder_list$origin_case_description[j],'/')
    # modify fuel prices
    print('modify fuel prices')
    RunFdr = paste0(origin_folder, '/Inputs/')
    if (ngprice_variation == 'census_division') {
      source('./code/postpg_monthlyngvariation.R')
    }
    if (ngprice_variation == 'national') {
      source('./code/postpg_nationalmonthlyngvariation.R')
    }  
    
    # calculate prior capacity factor
    print('calculate prior capacity factor')
    # Read in the load profile to construct the time-weight
    t_load = read_csv(paste0(RunFdr,"Load_data.csv"), 
                      col_types = cols())
    n_hour_per_period = t_load$Timesteps_per_Rep_Period[1]
    sub_weights = t_load$Sub_Weights %>% na.omit()
    time_weight = rep(sub_weights, each = n_hour_per_period)/n_hour_per_period
    n_hour_modeled = sum(time_weight)
    # Read in the variability 
    gen_var = read_csv(paste0(RunFdr,"Generators_variability.csv"), 
                       col_types = cols()) %>%
      select(-Time_Index) %>%
      t()
    resource_name = as_tibble_col(rownames(gen_var), column_name = "Resource")
    gen_cf <- (gen_var %*% time_weight / n_hour_modeled) %>%
      as_tibble_col(column_name = "CF_prior") %>%
      mutate(CF_prior = as.vector(round(CF_prior, 4))) %>%
      cbind(resource_name)
    # print(gen_cf)
    write_csv(gen_cf, paste0(RunFdr,'temp_capacityfactor.csv'))
    
    # modify generators_data.csv
    print('modify generators_data.csv')
    gen_info_fn <- paste0(RunFdr,'Generators_data.csv')
    gen_info <- read_csv(gen_info_fn, col_types = cols());
    # add in the prior cf
    print('add in the prior cf')
    gen_info <- left_join(gen_info, gen_cf, by = "Resource")
    print('VOM of battery')
    batteryrows = which(grepl('batter', gen_info$Resource))
    gen_info <- gen_info %>%
      mutate(Var_OM_Cost_per_MWh_In = replace(Var_OM_Cost_per_MWh_In, batteryrows, 0.15),
             Var_OM_Cost_per_MWh = replace(Var_OM_Cost_per_MWh, batteryrows, 0.15))
    print('modify pump hydros duration and min max duration')
    psrows = which(grepl('pumped_storage', gen_info$Resource))
    # 15.5 hour is the national average of duration in U.S., reported by Sandia
    psrowscapmwh = 15.5*gen_info$Existing_Cap_MW 
    gen_info <- gen_info %>%
      mutate(Min_Duration = replace(Min_Duration, psrows, 14),
             Max_Duration = replace(Max_Duration, psrows, 16),
             Existing_Cap_MWh = replace(Existing_Cap_MWh, psrows, psrowscapmwh[psrows]))
    print('Existing wind PTC')
    existingwind_rows = which(grepl('onshore_wind_turbine', gen_info$Resource))
    gen_info <- gen_info %>%
      mutate(Var_OM_Cost_per_MWh = replace(Var_OM_Cost_per_MWh, existingwind_rows, -7.2))
    print('Turning off Advanced Nuclear')
    advnuclear_rows = which(grepl('advnuclear',gen_info$technology))
    gen_info <- gen_info %>%
	    mutate(New_Build = replace(New_Build, advnuclear_rows,as.integer(0)))
    print('CCS and dg cost modification')
    # CCS and dg cost modification
    if (file.exists(region_tech_costadder_fn)){
      region_tech_costadder <- read_csv(region_tech_costadder_fn, col_types = cols()) %>%
        filter(year == y) %>%
        select(-year)
      # inv_annuity_adder	fom_adder	vom_in_adder	vom_out_adder
      gen_info <- gen_info %>%
        left_join(region_tech_costadder, by = c("region", "technology")) %>%
        mutate(inv_annuity_adder = replace(inv_annuity_adder, which(is.na(inv_annuity_adder)), 0),
               fom_adder = replace(fom_adder, which(is.na(fom_adder)), 0),
               vom_in_adder = replace(vom_in_adder, which(is.na(vom_in_adder)), 0),
               vom_out_adder = replace(vom_out_adder, which(is.na(vom_out_adder)), 0)) %>%
        mutate(Inv_Cost_per_MWyr = Inv_Cost_per_MWyr + inv_annuity_adder,
               Fixed_OM_Cost_per_MWyr = Fixed_OM_Cost_per_MWyr + fom_adder) %>%
        select(-c(inv_annuity_adder,fom_adder, vom_in_adder, vom_out_adder))
    }
    gen_info <- gen_info %>%
      mutate(CapRes_duration_requirement = 8)
    print('write gen_data.csv')
    write_csv(gen_info, gen_info_fn)
    print('write gen_data.csv finished')
    # modify network.csv
    print('modify network.csv')
    network_info_fn <- paste0(RunFdr,'Network.csv')
    network_info <- read_csv(network_info_fn, col_types = cols())
    if (file.exists(network_template_fn)){
      network_template = read_csv(network_template_fn, col_types = cols()) %>%
        select(transmission_path_name,Line_Reinforcement_Cost_per_MWyr) %>%
        rename(temp_cost = Line_Reinforcement_Cost_per_MWyr)
      network_info <- network_info %>%
        left_join(network_template, by = 'transmission_path_name') %>%
        mutate(Line_Reinforcement_Cost_per_MWyr = temp_cost) %>%
        select(-temp_cost)
    }
    temp_min_trans = min_trans_upperbound$min[min_trans_upperbound$year == y]
    network_info <- network_info %>%
      mutate(Line_Max_Reinforcement_MW = replace(Line_Max_Reinforcement_MW, 
                                                 Line_Max_Reinforcement_MW <= temp_min_trans, 
                                                 temp_min_trans))
    write_csv(network_info, network_info_fn)
  }
}


dir.create(paste0(workingfolder,tofolder), recursive = T)
for (y in years) {
  dir.create(paste0(workingfolder, tofolder,'/', y ,'/'), recursive = T)    
  for (i in 1:nrow(foldernames)) {
    # create new folders
    new_folder = paste0(workingfolder,tofolder,'/', y ,'/',
                        foldernames$case_id[i],'_', y ,'_',
                        foldernames$case_description[i])
    dir.create(new_folder)
    
    # copy data from the original folder
    origin_folder = paste0(workingfolder,fromfolder,'/', y, '/', 
                           foldernames$origin_case_id[i],"_", y, "_",foldernames$origin_case_description[i],'/')
    list_of_files <- list.files(origin_folder)
    file.copy(file.path(origin_folder,list_of_files), new_folder, recursive = T, overwrite = T)
    
    # copying settings
    new_settings_folder = paste0(workingfolder,tofolder,'/', y ,'/',
                                 foldernames$case_id[i],'_', y ,'_',
                                 foldernames$case_description[i],'/Settings')
    dir.create(new_settings_folder)
    file.copy(file.path(original_settings_folder, settings_file_list), 
              new_settings_folder, recursive = T, overwrite = T)
    
    # modify settings to correct case id and case_descriptions
    setting = read_yaml(paste0(new_settings_folder,'/genx_settings.yml'))
    setting$case_id = foldernames$case_id[i]
    setting$case_name = foldernames$case_description[i]
    setting$CO2Capture = as.integer(1)
    setting$CO2Credit = as.integer(1)
    setting$PolicyTransmissionLossCoverage = as.integer(1)
    write_yaml(setting, paste0(new_settings_folder,'/genx_settings.yml'))
    
    # copying run files
    file.copy(file.path(runfile), new_folder, recursive = T, overwrite = T)
    
    # copying misc files
    if (length(miscfiles) !=0) {
      file.copy(file.path(miscfiles), paste0(new_folder,"/Inputs"), recursive = T, overwrite = T)
    }
    
    # copy and modify shell
    file.copy(file.path(shellfile), new_folder, recursive = T, overwrite = T)
    
    modifysh <- file(paste0(new_folder,'/',name_of_shell))
    writeLines(c(
      "#!/bin/bash",
      paste0("#SBATCH --job-name=",substr(foldernames$case_id[i],1,6),"              # create a short name for your job"),
      "#SBATCH --nodes=1                           # node count",
      "#SBATCH --ntasks=1                          # total number of tasks across all nodes",
      "#SBATCH --cpus-per-task=6                   # cpu-cores per task (>1 if multi-threaded tasks)",
      "#SBATCH --mem-per-cpu=16G                    # memory per cpu-core",
      "#SBATCH --time=23:00:00                     # total run time limit (HH:MM:SS)",
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
  }
}



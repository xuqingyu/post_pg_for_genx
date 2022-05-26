# Header, setup the path

misc_filefolder = paste0(workingfolder, miscfoldername)
targetfolder <- paste0(workingfolder, tofolder)
pgresult = paste0(workingfolder,frompgfolder)
fromfolderpath = paste0(workingfolder, fromfolder)
dir.create(paste0(fromfolderpath))

foldernames <- read_csv(paste0(workingfolder,foldername_fn), col_types = cols())
caseids = foldernames
n_case <- dim(caseids)[1]
yearlist = years


original_settings_folder = paste0(misc_filefolder,'Settings/')
settings_file_list = list.files(original_settings_folder)
region_tech_costadder_fn <- paste(misc_filefolder, c('region_tech_costadder.csv'), sep = '/')
network_template_fn <- paste(misc_filefolder, c('Network_template.csv'), sep = '/')
miscfiles = paste(misc_filefolder, files_to_copy_from_misc, sep = '/')
shellfile = paste(misc_filefolder, name_of_shell, sep = '/')
runfile = paste(misc_filefolder, name_of_runfile, sep = '/')
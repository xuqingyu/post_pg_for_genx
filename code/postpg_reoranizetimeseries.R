# Reorganize the time series

# read the load to grab the necessory information

path <- temppath
representative_point_fn <- file.path(path, 'Representative_Period.csv')
representative_point <- read_csv(representative_point_fn)

load_tobe_write_fn <- file.path(path, 'Load_data.csv')
load_tobe_write <- read_csv(load_tobe_write_fn, col_types = cols())
n_hour_per_period <- load_tobe_write$Timesteps_per_Rep_Period[1]

original_hourid = ((rep(as.numeric(str_remove(representative_point$slot,'p')), 
                        each = n_hour_per_period) - 1) * n_hour_per_period + 
                     rep(c(1:n_hour_per_period), times = nrow(representative_point)))

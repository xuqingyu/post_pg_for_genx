ngmultiplier = read_csv('./data/National_Natural_Price_Factor_2019.csv', col_types = cols()) %>%
  mutate(Month = as.numeric(Month)) %>%
  select(-c(Date, Year, `NG Price`, `Annual NG Price`))
fuel_price <- read_csv(paste0(RunFdr,'/Fuels_data.csv'), col_types = cols())[-1,]
fuel_price_original <- read_csv(paste0(RunFdr,'/Fuels_data.csv'), col_types = cols())
if (grepl("superhighngcost",RunFdr)) {
  for (c in 1:ncol(fuel_price)) {
    if (grepl("naturalgas", colnames(fuel_price)[c])) {
      fuel_price[,c] = fuel_price[,c] + ngpriceadder
    }
  }
}
# fuel_rowone stores the emission factor.
fuel_rowone <- read_csv(paste0(RunFdr,'/Fuels_data.csv'), col_types = cols())[1,] 
fuel_order = colnames(fuel_price)[-1]
reprepoint <- read_csv(paste0(RunFdr,'/Representative_Period.csv'), col_types = cols())
rep_hour <- select(read_csv(paste0(RunFdr,'/Load_data.csv'), col_types = cols()), 
                   Timesteps_per_Rep_Period)$Timesteps_per_Rep_Period[1]

mapping = tibble(
  Period = paste('p',ceiling(c(1:8760)/rep_hour),sep=""),
  Period_Hour = rep(c(1:rep_hour), length = 8760),
  Month = rep(c(1:12), times = c(31,28,31,30,31,30,31,31,30,31,30,31)*24))

fuel_price_variate <- fuel_price %>%
  mutate(Period = rep(reprepoint$slot, each = rep_hour),
         Period_Hour = rep(c(1:rep_hour), length = rep_hour*(dim(reprepoint)[1]))) %>%
  left_join(mapping) %>%
  pivot_longer(cols = -c(Time_Index, Period, Period_Hour, Month), 
               names_to = 'fuel', values_to = 'price') %>%
  left_join(ngmultiplier)
fuel_price_variate$Factor[which(!grepl('naturalgas',fuel_price_variate$fuel))] <- 1
fuel_price_variate <- fuel_price_variate %>%
  mutate(price_var = round(price * Factor,2),
         fuel = factor(fuel, levels = fuel_order)) %>%
  select(-c(Period, Period_Hour, Month, price, Factor)) %>%
  pivot_wider(id_cols = Time_Index, names_from = 'fuel', values_from = 'price_var')
write_csv(rbind(fuel_rowone, fuel_price_variate), paste0(RunFdr,'/Fuels_data.csv'))
write_csv(fuel_price_original, paste0(RunFdr,'/Fuels_data_original.csv'))

rm(fuel_price_variate, fuel_price, mapping, rep_hour, reprepoint, fuel_order, fuel_rowone, ngmultiplier)

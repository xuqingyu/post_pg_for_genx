# need tidy verse
randomize_cluster <- function(path) {
  period_map_fn <- file.path(path, 'Period_map.csv')
  period_map <- read_csv(period_map_fn, col_types = cols())
  if ('cluster_medoids' %in% colnames(period_map)) {
    print('This Period_map.csv has already been randomized')
  } else {
    print('start to randomize the Period_map.csv')
    print('grab medoids')
    medoids <- unique(select(period_map,Rep_Period_Index,Rep_Period)) %>%
      arrange(Rep_Period_Index) %>%
      mutate(cluster_medoids = Rep_Period)
    print('Randomization begins')
    for (i in 1:nrow(medoids)){
      members <- period_map$Period_Index[which(period_map$Rep_Period_Index==i)]
      medoids$Rep_Period[i] = sample(members, 1)
    }
    print('overwrite the files')
    period_map <- period_map %>%
      rename(cluster_medoids = Rep_Period) %>%
      left_join(medoids) %>%
      select(Period_Index, Rep_Period_Index, Month, Rep_Period,cluster_medoids) %>%
      write_csv(period_map_fn)
    
    Representative_Period <- select(medoids, Rep_Period) %>%
      rename(slot = Rep_Period) %>%
      mutate(slot = paste0('p',slot)) %>%
      write_csv(file.path(path, 'Representative_Period.csv'))
  }
} 

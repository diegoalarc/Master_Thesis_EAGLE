library(tidyverse)

# Merge the values of the bands from Carmen Rosa farm with the information of the weather station
# Carmen Rosa
defaultW <- getOption('warn') 

options(warn = -1)

Value_bands_cm <- list.files(path = '/home/diego/GITHUP_REPO/Master_Thesis_EAGLE/Original_data/Values_bands',
                    full.names = TRUE,
                    pattern = 'Carmen_Rosa')

for (k in 1:length(Value_bands_cm)){

  file_name <- strsplit(Value_bands_cm[k],'/')[[1]][8]
  
  df1 <- read.csv(Value_bands_cm[k])
  
  # Merge with Temperature
  df2 <- read.csv('/home/diego/GITHUP_REPO/Master_Thesis_EAGLE/Original_data/weather_station/Carmen_Rosa/Temperature.csv')
  
  # Filter by Weather_station
  df2 <- df2[df2$Weather_station == 'Carmen_Rosa',]
  
  df3 <- merge(df1, df2, by.x=c('Year'), by.y=c('Year'))
  
  # Merge with Humidity
  df4 <- read.csv('/home/diego/GITHUP_REPO/Master_Thesis_EAGLE/Original_data/weather_station/Carmen_Rosa/Humidity.csv')
  
  # Filter by Weather_station
  df4 <- df4[df4$Weather_station == 'Carmen_Rosa',]
  
  df5 <- merge(df3, df4, by.x=c('Year'), by.y=c('Year'))
  
  # Merge with GGD
  df6 <- read.csv('/home/diego/GITHUP_REPO/Master_Thesis_EAGLE/Original_data/weather_station/Carmen_Rosa/GGD.csv')
  
  # Filter by Weather_station
  df6 <- df6[df6$Weather_station == 'Carmen_Rosa',]
  
  df7 <- merge(df5, df6, by.x=c('Year'), by.y=c('Year'))
  
  # Merge with Evapotranspiration
  df8 <- read.csv('/home/diego/GITHUP_REPO/Master_Thesis_EAGLE/Original_data/weather_station/Carmen_Rosa/Evapotranspiration.csv')
  
  df8 <- df8[df8$Weather_station == 'Carmen_Rosa',]
  
  df9 <- merge(df7, df8, by.x=c('Year'), by.y=c('Year'))
  
  # Clean the weather station column
  df9$Weather_station.x <- NULL
  df9$Weather_station.y <- NULL
  df9$Weather_station.x <- NULL
  df9$Weather_station.y <- NULL
  
  # Merge with Fertilizer
  df10 <- read.csv('/home/diego/GITHUP_REPO/Master_Thesis_EAGLE/Original_data/Fertilizers/Fertilizer.csv')
  
  # Filter by Weather_station
  df10 <- df10[df10$CAMPO == 'CARMEN ROSA',]
  
  df10 <- merge(df9, df10, by.x=c('Year', 'Field'), by.y=c('Year', 'Field'))

  # Clean rows are not necessary
  df10$CAMPO <- NULL
  df10$Variety <- NULL
  df10$Season <- NULL
  
  # Save dataframe as .CSV
  write.csv(df10, 
            paste0('/home/diego/GITHUP_REPO/Master_Thesis_EAGLE/Original_data/Merge_values/Merge_',file_name), 
            row.names = F, quote = F)
}

################################################################################
# Merge the values of the bands from Quimetal farm with the information of the weather station
# Quimetal
Value_bands_q <- list.files(path = '/home/diego/GITHUP_REPO/Master_Thesis_EAGLE/Original_data/Values_bands',
                          full.names = TRUE,
                          pattern = 'Quimetal')

for (k in 1:length(Value_bands_q)){
  
  file_name <- strsplit(Value_bands_q[k],'/')[[1]][8]
  
  df1 <- read.csv(Value_bands_q[k])
  
  # Merge with Temperature
  df2 <- read.csv('/home/diego/GITHUP_REPO/Master_Thesis_EAGLE/Original_data/weather_station/Quimetal/Temperature.csv')
  
  # Filter by Weather_station
  df2 <- df2[df2$Weather_station == 'Talagante_Talagante',]
  
  df3 <- merge(df1, df2, by.x=c('Year'), by.y=c('Year'))
  
  # Merge with Humidity
  df4 <- read.csv('/home/diego/GITHUP_REPO/Master_Thesis_EAGLE/Original_data/weather_station/Quimetal/Humidity.csv')
  
  # Filter by Weather_station
  df4 <- df4[df4$Weather_station == 'Talagante_Talagante',]
  
  df5 <- merge(df3, df4, by.x=c('Year'), by.y=c('Year'))
  
  # Merge with GGD
  df6 <- read.csv('/home/diego/GITHUP_REPO/Master_Thesis_EAGLE/Original_data/weather_station/Quimetal/GGD.csv')
  
  # Filter by Weather_station
  df6 <- df6[df6$Weather_station == 'Talagante_Talagante',]
  
  df7 <- merge(df5, df6, by.x=c('Year'), by.y=c('Year'))
  
  # Merge with Evapotranspiration
  df8 <- read.csv('/home/diego/GITHUP_REPO/Master_Thesis_EAGLE/Original_data/weather_station/Quimetal/Evapotranspiration.csv')
  
  # Filter by Weather_station
  df8 <- df8[df8$Weather_station == 'Talagante_Talagante',]
  
  df9 <- merge(df7, df8, by.x=c('Year'), by.y=c('Year'))
  
  # Clean the weather station column
  df9$Weather_station.x <- NULL
  df9$Weather_station.y <- NULL
  df9$Weather_station.x <- NULL
  df9$Weather_station.y <- NULL
  
  # Merge with Fertilizer
  df10 <- read.csv('/home/diego/GITHUP_REPO/Master_Thesis_EAGLE/Original_data/Fertilizers/Fertilizer.csv')
  
  # Filter by Weather_station
  df10 <- df10[df10$CAMPO == 'LORETO',]
  
  df10 <- merge(df9, df10, by.x=c('Year', 'Field'), by.y=c('Year', 'Field'))
  
  # Clean rows are not necessary
  df10$CAMPO <- NULL
  df10$CAMPO.x <- NULL
  df10$CAMPO.y <- NULL
  df10$Variety <- NULL
  df10$Season <- NULL
  
  # Save dataframe as .CSV
  write.csv(df10, 
            paste0('/home/diego/GITHUP_REPO/Master_Thesis_EAGLE/Original_data/Merge_values/Merge_',file_name), 
            row.names = F, quote = F)
}

options(warn = defaultW)

################################################################################
# Merge values with the productive summary of each field in Carmen Rosa farm between 2018 & 2020
# This is the reason why the above variables are called with the years 2018 to 2020.
# Although the values of the images were obtained in the years 2017, 2018 and 2019, 
# they will be used to calculate the production of subsequent years.
# Carmen Rosa
Value_bands_cm_m <- list.files(path = '/home/diego/GITHUP_REPO/Master_Thesis_EAGLE/Original_data/Merge_values',
                            full.names = TRUE,
                            pattern = 'Merge_Carmen_Rosa_')

for (k in 1:length(Value_bands_cm_m)){
  # File name
  file_name <- strsplit(Value_bands_cm_m[k],'/')[[1]][8]
  
  # Read the dataframe merged before
  df1 <- read.csv(Value_bands_cm_m[k])
  
  # Merge with Productive summary of each field in Carmen Rosa farm
  df2 <- read.csv('/home/diego/GITHUP_REPO/Master_Thesis_EAGLE/Original_data/Productive_summary.csv')
  
  # Merge by two fields
  df3 <- merge(df1, df2, by.x=c('Year', 'Field'), by.y=c('Year', 'Field'))
  
  # Clean the df3
  df3$Week <- NULL
  
  # Reorden the dataframe
  df4 <- df3 %>%
    relocate("id", "Year", "Field", "Variety", "Kg_He" , "Area_He")
  
  df4 <- df4[order(df4$id),]
  
  df4$CAMPO.x <- NULL
  df4$CAMPO.y <- NULL
  
  # Save dataframe as .CSV
  write.csv(df4, 
            paste0('/home/diego/GITHUP_REPO/Master_Thesis_EAGLE/Original_data/Final_values/fn_',file_name), 
            row.names = F, quote = F)
}

################################################################################
# Do not reproduce / This code is not necessary.
# Quimetal
Value_bands_q_m <- list.files(path = '/home/diego/GITHUP_REPO/Master_Thesis_EAGLE/Original_data/Merge_values',
                               full.names = TRUE,
                               pattern = 'Merge_Quimetal_')

for (k in 1:length(Value_bands_q_m)){
  # File name
  file_name <- strsplit(Value_bands_q_m[k],'/')[[1]][8]
  
  # Read the dataframe merged before
  df1 <- read.csv(Value_bands_q_m[k])
  
  # Merge with Productive summary of each field in Quimetal farm
  df2 <- read.csv('/home/diego/GITHUP_REPO/Master_Thesis_EAGLE/Original_data/Productive_summary.csv')
  
  # Merge by two fields
  df3 <- merge(df1, df2, by.x=c('Year', 'Field'), by.y=c('Year', 'Field'))
  
  # Clean the df3
  df3$Week <- NULL
  
  # Reorden the dataframe
  df4 <- df3 %>%
    relocate("id", "Year", "Field", "Variety", "Kg_He" , "Area_He")
  
  df4 <- df4[order(df4$id),]
  
  df4$CAMPO.x <- NULL
  df4$CAMPO.y <- NULL
  
  # Save dataframe as .CSV
  write.csv(df4, 
            paste0('/home/diego/GITHUP_REPO/Master_Thesis_EAGLE/Original_data/Final_values/fn_',file_name), 
            row.names = F, quote = F)
}

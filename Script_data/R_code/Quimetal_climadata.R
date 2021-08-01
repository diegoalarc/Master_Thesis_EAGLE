library(tidyr)

setwd("/home/diego/Dropbox/Diego/Master_Thesis/Clima/Quimetal")

Precipitation_csv <- "./Precipitation/"

# Call the .csv file from folder
files.names_p <- list.files(path = Precipitation_csv, pattern=c("\\.csv$"), 
                    full.names=T)

# Aplycation of a lapply
theList_p <- lapply(files.names_p,function(x){
  theData <- read.csv(x, sep = ";")
  # bind the region, cluster, and point data and return
  cbind(theData)
})

# rbind the data frames in theList_p into a single data frame  
Precipitation_2019_csv <- do.call(rbind, theList_p)

# Clean NA columns
Precipitation_2019_csv <- Precipitation_2019_csv[, colSums(
  is.na(Precipitation_2019_csv)) != nrow(Precipitation_2019_csv)]

# Separation of column momento in to Date and Hours
Precipitation_2019_csv <- Precipitation_2019_csv %>% 
  separate(momento, c("Date", "Hour"), " ")

Precipitation_2019_csv <- transform(Precipitation_2019_csv, Date = as.Date(Date))

# Save CSV
write.csv(Precipitation_2019_csv,"./Precipitation/Precipitation_2019.csv", row.names = FALSE)

################################################################################

Presion_csv <- "./PresionHumedad/"

files.names_pp <- list.files(path = Presion_csv, pattern=c("\\.csv$"), 
                          full.names=T)

theList_pp <- lapply(files.names_pp,function(x){
  theData <- read.csv(x, sep = ";", skipNul = T)
  # bind the region, cluster, and point data and return
  cbind(theData)
})

# rbind the data frames in theList_pp into a single data frame  
Presion_2019_csv <- do.call(rbind,theList_pp)

# Clean NA columns
Presion_2019_csv <- Presion_2019_csv[, colSums(
  is.na(Presion_2019_csv)) != nrow(Presion_2019_csv)]

# Separation of column momento in to Date and Hours
Presion_2019_csv <- Presion_2019_csv %>% 
  separate(momento, c("Date", "Hour"), " ")

Presion_2019_csv <- transform(Presion_2019_csv, Date = as.Date(Date))

# Save CSV
write.csv(Presion_2019_csv,"./PresionHumedad/Presion_2019.csv", row.names = FALSE)

################################################################################

Temperature_csv <- "./Temperature/"

files.names_t <- list.files(path = Temperature_csv, pattern=c("\\.csv$"), 
                          full.names=T)

theList_t <- lapply(files.names_t,function(x){
  theData <- read.csv(x, sep = ";", skipNul = T)
  # bind the region, cluster, and point data and return
  cbind(theData)
})
# rbind the data frames in theList_t into a single data frame  
Temperature_2019_csv <- do.call(rbind,theList_t)

# Clean NA columns
Temperature_2019_csv <- Temperature_2019_csv[, colSums(
  is.na(Temperature_2019_csv)) != nrow(Temperature_2019_csv)]

# Separation of column momento in to Date and Hours
Temperature_2019_csv <- Temperature_2019_csv %>% 
  separate(momento, c("Date", "Hour"), " ")

Temperature_2019_csv <- transform(Temperature_2019_csv, Date = as.Date(Date))

# Save CSV
write.csv(Temperature_2019_csv,"./Temperature/Temperature_2019.csv", row.names = FALSE)
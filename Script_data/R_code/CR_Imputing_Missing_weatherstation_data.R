# https://datascienceplus.com/imputing-missing-data-with-r-mice-package/
# https://www.researchgate.net/post/Is_there_an_approach_or_R-package_for_imputing_missing_values_in_time_series_data
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3074241/
# http://juliejosse.com/wp-content/uploads/2018/06/DataAnalysisMissingR.html
# Used for Test https://datascienceplus.com/imputing-missing-data-with-r-mice-package/ 
# Imputing Missing data with Mice package
#install.packages("mice")
#install.packages("VIM")
library(mice)
library(VIM)

setwd('/home/diego/GITHUP_REPO/Master_Thesis_EAGLE/Original_data/weather_station/Carmen_Rosa_imputation')

# Temperature
Temperature <- read.csv("./Temperature.csv", na.strings = c("NA", " ", "-999"))
Temperature <- transform(Temperature, 
                         Time_UTC_Chile =  as.Date(Time_UTC_Chile, format =  "%m/%d/%Y"),
                         Los_Tilos = as.numeric(Los_Tilos),
                         San_Antonio_de_Naltahua = as.numeric(San_Antonio_de_Naltahua),
                         San_Pedro_de_Melipilla = as.numeric(San_Pedro_de_Melipilla),
                         Carmen_Rosa = as.numeric(Carmen_Rosa))

# Evapotranspiration
Evapo <- read.csv("./Evapotranpiration.csv", na.strings = c("NA", " ", "-999"))
Evapo <- transform(Evapo,
                   Time_UTC_Chile =  as.Date(Time_UTC_Chile, format =  "%m/%d/%Y"),
                   Los_Tilos = as.numeric(Los_Tilos),
                   San_Antonio_de_Naltahua = as.numeric(San_Antonio_de_Naltahua),
                   San_Pedro_de_Melipilla = as.numeric(San_Pedro_de_Melipilla),
                   Carmen_Rosa = as.numeric(Carmen_Rosa))

# Relative Humidity
Hum <- read.csv("./relative_humidity.csv", na.strings = c("NA", " ", "-999"))
Hum <- transform(Hum, 
                 Time_UTC_Chile =  as.Date(Time_UTC_Chile, format =  "%m/%d/%Y"),
                 Los_Tilos = as.numeric(Los_Tilos),
                 San_Antonio_de_Naltahua = as.numeric(San_Antonio_de_Naltahua),
                 San_Pedro_de_Melipilla = as.numeric(San_Pedro_de_Melipilla),
                 Carmen_Rosa = as.numeric(Carmen_Rosa))

# Growing degree-day
GGD <- read.csv("./grade_day.csv", na.strings = c("NA", " ", "-999"))
GGD <- transform(GGD, 
                 Time_UTC_Chile =  as.Date(Time_UTC_Chile, format =  "%m/%d/%Y"),
                 Los_Tilos = as.numeric(Los_Tilos),
                 San_Antonio_de_Naltahua = as.numeric(San_Antonio_de_Naltahua),
                 San_Pedro_de_Melipilla = as.numeric(San_Pedro_de_Melipilla),
                 Carmen_Rosa = as.numeric(Carmen_Rosa))


# Summary to obsevate the data distribution
summary(Temperature)
summary(Evapo)
summary(Hum)
summary(GGD)

# MCAR: missing completely at random. 
pMiss <- function(x){sum(is.na(x))/length(x)*100}

# Observation of data missing for:
# Temperature
apply(Temperature,2,pMiss)
apply(Temperature,1,pMiss)

# Evapotranspiration
apply(Evapo,2,pMiss)
apply(Evapo,1,pMiss)

# Relative Humidity
apply(Hum,2,pMiss)
apply(Hum,1,pMiss)

# Growing degree-day
apply(GGD,2,pMiss)
apply(GGD,1,pMiss)

# Looking at missing data pattern
md.pattern(Temperature)
md.pattern(Evapo)
md.pattern(Hum)
md.pattern(GGD)

# Set the folder location to write the final .csv
setwd('/home/diego/GITHUP_REPO/Master_Thesis_EAGLE')

################################################################################
# Working just with Temperature data
aggr_plot_temp <- aggr(Temperature[2:5], col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(Temperature[2:5]), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing Temperature","Pattern"))

# Bloxplot of Carmen Rosa ws V/s the other weathe station
marginplot(Temperature[c(2,5)])
marginplot(Temperature[c(3,5)])
marginplot(Temperature[c(4,5)])

# Random Forest for Matching
#tempData <- mice(Temperature[2:5], meth='rf', ntree = 10)

# Predictive Mean Matching
tempData <- mice(Temperature[2:5],m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)

completedData_Temp <- cbind(Temperature[1],complete(tempData,1))

Temp_2018 <- completedData_Temp[completedData_Temp$Time_UTC_Chile >= "17-10-01" & 
                                  completedData_Temp$Time_UTC_Chile <= "17-12-31",]

Temp_2018 <- data.frame(
  min_Temp = c(min(Temp_2018[["Los_Tilos"]]), min(Temp_2018[["San_Antonio_de_Naltahua"]]),
          min(Temp_2018[["San_Pedro_de_Melipilla"]]), min(Temp_2018[["Carmen_Rosa"]])),
  max_Temp = c(max(Temp_2018[["Los_Tilos"]]), max(Temp_2018[["San_Antonio_de_Naltahua"]]),
          max(Temp_2018[["San_Pedro_de_Melipilla"]]), max(Temp_2018[["Carmen_Rosa"]])),
  mean_Temp = c(mean(Temp_2018[["Los_Tilos"]]), mean(Temp_2018[["San_Antonio_de_Naltahua"]]),
           mean(Temp_2018[["San_Pedro_de_Melipilla"]]), mean(Temp_2018[["Carmen_Rosa"]])),
  sum_Temp = c(sum(Temp_2018[["Los_Tilos"]]), sum(Temp_2018[["San_Antonio_de_Naltahua"]]),
          sum(Temp_2018[["San_Pedro_de_Melipilla"]]), sum(Temp_2018[["Carmen_Rosa"]])),
  median_Temp = c(median(Temp_2018[["Los_Tilos"]]), median(Temp_2018[["San_Antonio_de_Naltahua"]]),
             median(Temp_2018[["San_Pedro_de_Melipilla"]]), median(Temp_2018[["Carmen_Rosa"]])),
  stdv_Temp = c(sd(Temp_2018[["Los_Tilos"]]), sd(Temp_2018[["San_Antonio_de_Naltahua"]]),
           sd(Temp_2018[["San_Pedro_de_Melipilla"]]), sd(Temp_2018[["Carmen_Rosa"]])),
  Year = '2018',
  Weather_station = c("Los_Tilos", "San_Antonio_de_Naltahua",
                      "San_Pedro_de_Melipilla", "Carmen_Rosa"))

Temp_2019 <- completedData_Temp[completedData_Temp$Time_UTC_Chile >= "18-10-01" & 
                                  completedData_Temp$Time_UTC_Chile <= "18-12-31",]

Temp_2019 <- data.frame(
  min_Temp = c(min(Temp_2019[["Los_Tilos"]]), min(Temp_2019[["San_Antonio_de_Naltahua"]]),
          min(Temp_2019[["San_Pedro_de_Melipilla"]]), min(Temp_2019[["Carmen_Rosa"]])),
  max_Temp = c(max(Temp_2019[["Los_Tilos"]]), max(Temp_2019[["San_Antonio_de_Naltahua"]]),
          max(Temp_2019[["San_Pedro_de_Melipilla"]]), max(Temp_2019[["Carmen_Rosa"]])),
  mean_Temp = c(mean(Temp_2019[["Los_Tilos"]]), mean(Temp_2019[["San_Antonio_de_Naltahua"]]),
           mean(Temp_2019[["San_Pedro_de_Melipilla"]]), mean(Temp_2019[["Carmen_Rosa"]])),
  sum_Temp = c(sum(Temp_2019[["Los_Tilos"]]), sum(Temp_2019[["San_Antonio_de_Naltahua"]]),
          sum(Temp_2019[["San_Pedro_de_Melipilla"]]), sum(Temp_2019[["Carmen_Rosa"]])),
  median_Temp = c(median(Temp_2019[["Los_Tilos"]]), median(Temp_2019[["San_Antonio_de_Naltahua"]]),
             median(Temp_2019[["San_Pedro_de_Melipilla"]]), median(Temp_2019[["Carmen_Rosa"]])),
  stdv_Temp = c(sd(Temp_2019[["Los_Tilos"]]), sd(Temp_2019[["San_Antonio_de_Naltahua"]]),
           sd(Temp_2019[["San_Pedro_de_Melipilla"]]), sd(Temp_2019[["Carmen_Rosa"]])),
  Year = '2019',
  Weather_station = c("Los_Tilos", "San_Antonio_de_Naltahua",
                      "San_Pedro_de_Melipilla", "Carmen_Rosa"))

Temp_2020 <- completedData_Temp[completedData_Temp$Time_UTC_Chile >= "19-10-01" & 
                                  completedData_Temp$Time_UTC_Chile <= "19-12-31",]

Temp_2020 <- data.frame(
  min_Temp = c(min(Temp_2020[["Los_Tilos"]]), min(Temp_2020[["San_Antonio_de_Naltahua"]]),
          min(Temp_2020[["San_Pedro_de_Melipilla"]]), min(Temp_2020[["Carmen_Rosa"]])),
  max_Temp = c(max(Temp_2020[["Los_Tilos"]]), max(Temp_2020[["San_Antonio_de_Naltahua"]]),
          max(Temp_2020[["San_Pedro_de_Melipilla"]]), max(Temp_2020[["Carmen_Rosa"]])),
  mean_Temp = c(mean(Temp_2020[["Los_Tilos"]]), mean(Temp_2020[["San_Antonio_de_Naltahua"]]),
           mean(Temp_2020[["San_Pedro_de_Melipilla"]]), mean(Temp_2020[["Carmen_Rosa"]])),
  sum_Temp = c(sum(Temp_2020[["Los_Tilos"]]), sum(Temp_2020[["San_Antonio_de_Naltahua"]]),
          sum(Temp_2020[["San_Pedro_de_Melipilla"]]), sum(Temp_2020[["Carmen_Rosa"]])),
  median_Temp = c(median(Temp_2020[["Los_Tilos"]]), median(Temp_2020[["San_Antonio_de_Naltahua"]]),
             median(Temp_2020[["San_Pedro_de_Melipilla"]]), median(Temp_2020[["Carmen_Rosa"]])),
  stdv_Temp = c(sd(Temp_2020[["Los_Tilos"]]), sd(Temp_2020[["San_Antonio_de_Naltahua"]]),
           sd(Temp_2020[["San_Pedro_de_Melipilla"]]), sd(Temp_2020[["Carmen_Rosa"]])),
  Year = '2020',
  Weather_station = c("Los_Tilos", "San_Antonio_de_Naltahua",
                      "San_Pedro_de_Melipilla", "Carmen_Rosa"))

temperature_fn <- rbind(Temp_2018, Temp_2019, Temp_2020)

# Save dataframe as .CSV
write.csv(temperature_fn, 
          paste0('./Original_data/weather_station/Carmen_Rosa/Temperature.csv'), 
          row.names = F, quote = F)

xyplot_Temp <- xyplot(tempData,Carmen_Rosa ~ Los_Tilos + San_Antonio_de_Naltahua +
                        San_Pedro_de_Melipilla,pch=18,cex=1)
xyplot_Temp

### All combined in one panel
### density plot of head circumference per imputation
### blue is observed, red is imputed
densityplot_Temp <- densityplot(tempData, ~ Carmen_Rosa)
densityplot_Temp

densityplot_Temp <- densityplot(tempData, ~ Carmen_Rosa | .imp)
densityplot_Temp

par(mfrow=c(2,2))
plot(Temperature$Time_UTC_Chile, Temperature$Carmen_Rosa)
plot(completedData_Temp$Time_UTC_Chile, completedData_Temp$Carmen_Rosa)

# Pooling
# The variable modelFit1 containts the results of the fitting performed over 
# the imputed datasets, while the pool() function pools them all together
modelFit_Temp <- with(tempData,lm(Carmen_Rosa ~ Los_Tilos + 
                                    San_Antonio_de_Naltahua +
                                    San_Pedro_de_Melipilla))
summary(pool(modelFit_Temp))

################################################################################
# Working just with Evapotranspiration data
aggr_plot_evapo <- aggr(Evapo[2:5], col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(Evapo[2:5]), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing Evapo","Pattern"))

# Bloxplot of Carmen Rosa ws V/s the other weathe station
marginplot(Evapo[c(2,5)])
marginplot(Evapo[c(3,5)])
marginplot(Evapo[c(4,5)])

# Random Forest for Matching
#EvapoData <- mice(Evapo[2:5], meth='rf', ntree = 10)

# Predictive Mean Matching
EvapoData <- mice(Evapo[2:5], m=5, maxit=50, meth='pmm', seed=500)
summary(EvapoData)

completedData_Evapo <- cbind(Evapo[1],complete(EvapoData,1))

Evapo_2018 <- completedData_Evapo[completedData_Evapo$Time_UTC_Chile >= "17-10-01" & 
                                    completedData_Evapo$Time_UTC_Chile <= "17-12-31",]

Evapo_2018 <- data.frame(
  min_Evapo = c(min(Evapo_2018[["Los_Tilos"]]), min(Evapo_2018[["San_Antonio_de_Naltahua"]]),
          min(Evapo_2018[["San_Pedro_de_Melipilla"]]), min(Evapo_2018[["Carmen_Rosa"]])),
  max_Evapo = c(max(Evapo_2018[["Los_Tilos"]]), max(Evapo_2018[["San_Antonio_de_Naltahua"]]),
          max(Evapo_2018[["San_Pedro_de_Melipilla"]]), max(Evapo_2018[["Carmen_Rosa"]])),
  mean_Evapo = c(mean(Evapo_2018[["Los_Tilos"]]), mean(Evapo_2018[["San_Antonio_de_Naltahua"]]),
           mean(Evapo_2018[["San_Pedro_de_Melipilla"]]), mean(Evapo_2018[["Carmen_Rosa"]])),
  sum_Evapo = c(sum(Evapo_2018[["Los_Tilos"]]), sum(Evapo_2018[["San_Antonio_de_Naltahua"]]),
          sum(Evapo_2018[["San_Pedro_de_Melipilla"]]), sum(Evapo_2018[["Carmen_Rosa"]])),
  median_Evapo = c(median(Evapo_2018[["Los_Tilos"]]), median(Evapo_2018[["San_Antonio_de_Naltahua"]]),
             median(Evapo_2018[["San_Pedro_de_Melipilla"]]), median(Evapo_2018[["Carmen_Rosa"]])),
  stdv_Evapo = c(sd(Evapo_2018[["Los_Tilos"]]), sd(Evapo_2018[["San_Antonio_de_Naltahua"]]),
           sd(Evapo_2018[["San_Pedro_de_Melipilla"]]), sd(Evapo_2018[["Carmen_Rosa"]])),
  Year = '2018',
  Weather_station = c("Los_Tilos", "San_Antonio_de_Naltahua",
                      "San_Pedro_de_Melipilla", "Carmen_Rosa"))

Evapo_2019 <- completedData_Evapo[completedData_Evapo$Time_UTC_Chile >= "18-10-01" & 
                                    completedData_Evapo$Time_UTC_Chile <= "18-12-31",]

Evapo_2019 <- data.frame(
  min_Evapo = c(min(Evapo_2019[["Los_Tilos"]]), min(Evapo_2019[["San_Antonio_de_Naltahua"]]),
          min(Evapo_2019[["San_Pedro_de_Melipilla"]]), min(Evapo_2019[["Carmen_Rosa"]])),
  max_Evapo = c(max(Evapo_2019[["Los_Tilos"]]), max(Evapo_2019[["San_Antonio_de_Naltahua"]]),
          max(Evapo_2019[["San_Pedro_de_Melipilla"]]), max(Evapo_2019[["Carmen_Rosa"]])),
  mean_Evapo = c(mean(Evapo_2019[["Los_Tilos"]]), mean(Evapo_2019[["San_Antonio_de_Naltahua"]]),
           mean(Evapo_2019[["San_Pedro_de_Melipilla"]]), mean(Evapo_2019[["Carmen_Rosa"]])),
  sum_Evapo = c(sum(Evapo_2019[["Los_Tilos"]]), sum(Evapo_2019[["San_Antonio_de_Naltahua"]]),
          sum(Evapo_2019[["San_Pedro_de_Melipilla"]]), sum(Evapo_2019[["Carmen_Rosa"]])),
  median_Evapo = c(median(Evapo_2019[["Los_Tilos"]]), median(Evapo_2019[["San_Antonio_de_Naltahua"]]),
             median(Evapo_2019[["San_Pedro_de_Melipilla"]]), median(Evapo_2019[["Carmen_Rosa"]])),
  stdv_Evapo = c(sd(Evapo_2019[["Los_Tilos"]]), sd(Evapo_2019[["San_Antonio_de_Naltahua"]]),
           sd(Evapo_2019[["San_Pedro_de_Melipilla"]]), sd(Evapo_2019[["Carmen_Rosa"]])),
  Year = '2019',
  Weather_station = c("Los_Tilos", "San_Antonio_de_Naltahua",
                      "San_Pedro_de_Melipilla", "Carmen_Rosa"))

Evapo_2020 <- completedData_Evapo[completedData_Evapo$Time_UTC_Chile >= "19-10-01" & 
                                    completedData_Evapo$Time_UTC_Chile <= "19-12-31",]

Evapo_2020 <- data.frame(
  min_Evapo = c(min(Evapo_2020[["Los_Tilos"]]), min(Evapo_2020[["San_Antonio_de_Naltahua"]]),
          min(Evapo_2020[["San_Pedro_de_Melipilla"]]), min(Evapo_2020[["Carmen_Rosa"]])),
  max_Evapo = c(max(Evapo_2020[["Los_Tilos"]]), max(Evapo_2020[["San_Antonio_de_Naltahua"]]),
          max(Evapo_2020[["San_Pedro_de_Melipilla"]]), max(Evapo_2020[["Carmen_Rosa"]])),
  mean_Evapo = c(mean(Evapo_2020[["Los_Tilos"]]), mean(Evapo_2020[["San_Antonio_de_Naltahua"]]),
           mean(Evapo_2020[["San_Pedro_de_Melipilla"]]), mean(Evapo_2020[["Carmen_Rosa"]])),
  sum_Evapo = c(sum(Evapo_2020[["Los_Tilos"]]), sum(Evapo_2020[["San_Antonio_de_Naltahua"]]),
          sum(Evapo_2020[["San_Pedro_de_Melipilla"]]), sum(Evapo_2020[["Carmen_Rosa"]])),
  median_Evapo = c(median(Evapo_2020[["Los_Tilos"]]), median(Evapo_2020[["San_Antonio_de_Naltahua"]]),
             median(Evapo_2020[["San_Pedro_de_Melipilla"]]), median(Evapo_2020[["Carmen_Rosa"]])),
  stdv_Evapo = c(sd(Evapo_2020[["Los_Tilos"]]), sd(Evapo_2020[["San_Antonio_de_Naltahua"]]),
           sd(Evapo_2020[["San_Pedro_de_Melipilla"]]), sd(Evapo_2020[["Carmen_Rosa"]])),
  Year = '2020',
  Weather_station = c("Los_Tilos", "San_Antonio_de_Naltahua",
                      "San_Pedro_de_Melipilla", "Carmen_Rosa"))

Evapo_fn <- rbind(Evapo_2018, Evapo_2019, Evapo_2020)

# Save dataframe as .CSV
write.csv(Evapo_fn, 
          paste0('./Original_data/weather_station/Carmen_Rosa/Evapotranspiration.csv'), 
          row.names = F, quote = F)

xyplot_Evapo <- xyplot(EvapoData,Carmen_Rosa ~ Los_Tilos + San_Antonio_de_Naltahua +
                         San_Pedro_de_Melipilla,pch=18,cex=1)
xyplot_Evapo

### All combined in one panel
### density plot of head circumference per imputation
### blue is observed, red is imputed
densityplot_Evapo <- densityplot(EvapoData, ~ Carmen_Rosa)
densityplot_Evapo

densityplot_Evapo <- densityplot(EvapoData, ~ Carmen_Rosa | .imp)
densityplot_Evapo

par(mfrow=c(2,2))
plot(Evapo$Time_UTC_Chile, Evapo$Carmen_Rosa)
plot(completedData_Evapo$Time_UTC_Chile, completedData_Evapo$Carmen_Rosa)

# Pooling
# The variable modelFit1 containts the results of the fitting performed over 
# the imputed datasets, while the pool() function pools them all together
modelFit_Evapo <- with(EvapoData,lm(Carmen_Rosa ~ Los_Tilos + 
                                      San_Antonio_de_Naltahua +
                                      San_Pedro_de_Melipilla))
summary(pool(modelFit_Evapo))

################################################################################
# Working just with Humidity data
aggr_plot_hum <- aggr(Hum[2:5], col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                        labels=names(Hum[2:5]), cex.axis=.7, gap=3, 
                        ylab=c("Histogram of missing Hum","Pattern"))

# Bloxplot of Carmen Rosa ws V/s the other weathe station
marginplot(Hum[c(2,5)])
marginplot(Hum[c(3,5)])
marginplot(Hum[c(4,5)])

#HumData <- mice(Hum[2:5], meth='rf', ntree = 10)
# Predictive Mean Matching
HumData <- mice(Hum[2:5], m=5, maxit=50, meth='pmm', seed=500)
summary(HumData)

completedData_Hum <- cbind(Hum[1], complete(HumData,1))

Hum_2018 <- completedData_Hum[completedData_Hum$Time_UTC_Chile >= "17-10-01" & 
                                completedData_Hum$Time_UTC_Chile <= "17-12-31",]

Hum_2018 <- data.frame(
  min_Hum = c(min(Hum_2018[["Los_Tilos"]]), min(Hum_2018[["San_Antonio_de_Naltahua"]]),
          min(Hum_2018[["San_Pedro_de_Melipilla"]]), min(Hum_2018[["Carmen_Rosa"]])),
  max_Hum = c(max(Hum_2018[["Los_Tilos"]]), max(Hum_2018[["San_Antonio_de_Naltahua"]]),
          max(Hum_2018[["San_Pedro_de_Melipilla"]]), max(Hum_2018[["Carmen_Rosa"]])),
  mean_Hum = c(mean(Hum_2018[["Los_Tilos"]]), mean(Hum_2018[["San_Antonio_de_Naltahua"]]),
           mean(Hum_2018[["San_Pedro_de_Melipilla"]]), mean(Hum_2018[["Carmen_Rosa"]])),
  sum_Hum = c(sum(Hum_2018[["Los_Tilos"]]), sum(Hum_2018[["San_Antonio_de_Naltahua"]]),
          sum(Hum_2018[["San_Pedro_de_Melipilla"]]), sum(Hum_2018[["Carmen_Rosa"]])),
  median_Hum = c(median(Hum_2018[["Los_Tilos"]]), median(Hum_2018[["San_Antonio_de_Naltahua"]]),
             median(Hum_2018[["San_Pedro_de_Melipilla"]]), median(Hum_2018[["Carmen_Rosa"]])),
  stdv_Hum = c(sd(Hum_2018[["Los_Tilos"]]), sd(Hum_2018[["San_Antonio_de_Naltahua"]]),
           sd(Hum_2018[["San_Pedro_de_Melipilla"]]), sd(Hum_2018[["Carmen_Rosa"]])),
  Year = '2018',
  Weather_station = c("Los_Tilos", "San_Antonio_de_Naltahua",
                      "San_Pedro_de_Melipilla", "Carmen_Rosa"))

Hum_2019 <- completedData_Hum[completedData_Hum$Time_UTC_Chile >= "18-10-01" & 
                                completedData_Hum$Time_UTC_Chile <= "18-12-31",]

Hum_2019 <- data.frame(
  min_Hum = c(min(Hum_2019[["Los_Tilos"]]), min(Hum_2019[["San_Antonio_de_Naltahua"]]),
          min(Hum_2019[["San_Pedro_de_Melipilla"]]), min(Hum_2019[["Carmen_Rosa"]])),
  max_Hum = c(max(Hum_2019[["Los_Tilos"]]), max(Hum_2019[["San_Antonio_de_Naltahua"]]),
          max(Hum_2019[["San_Pedro_de_Melipilla"]]), max(Hum_2019[["Carmen_Rosa"]])),
  mean_Hum = c(mean(Hum_2019[["Los_Tilos"]]), mean(Hum_2019[["San_Antonio_de_Naltahua"]]),
           mean(Hum_2019[["San_Pedro_de_Melipilla"]]), mean(Hum_2019[["Carmen_Rosa"]])),
  sum_Hum = c(sum(Hum_2019[["Los_Tilos"]]), sum(Hum_2019[["San_Antonio_de_Naltahua"]]),
          sum(Hum_2019[["San_Pedro_de_Melipilla"]]), sum(Hum_2019[["Carmen_Rosa"]])),
  median_Hum = c(median(Hum_2019[["Los_Tilos"]]), median(Hum_2019[["San_Antonio_de_Naltahua"]]),
             median(Hum_2019[["San_Pedro_de_Melipilla"]]), median(Hum_2019[["Carmen_Rosa"]])),
  stdv_Hum = c(sd(Hum_2019[["Los_Tilos"]]), sd(Hum_2019[["San_Antonio_de_Naltahua"]]),
           sd(Hum_2019[["San_Pedro_de_Melipilla"]]), sd(Hum_2019[["Carmen_Rosa"]])),
  Year = '2019',
  Weather_station = c("Los_Tilos", "San_Antonio_de_Naltahua",
                      "San_Pedro_de_Melipilla", "Carmen_Rosa"))

Hum_2020 <- completedData_Hum[completedData_Hum$Time_UTC_Chile >= "19-10-01" & 
                                completedData_Hum$Time_UTC_Chile <= "19-12-31",]

Hum_2020 <- data.frame(
  min_Hum = c(min(Hum_2020[["Los_Tilos"]]), min(Hum_2020[["San_Antonio_de_Naltahua"]]),
          min(Hum_2020[["San_Pedro_de_Melipilla"]]), min(Hum_2020[["Carmen_Rosa"]])),
  max_Hum = c(max(Hum_2020[["Los_Tilos"]]), max(Hum_2020[["San_Antonio_de_Naltahua"]]),
          max(Hum_2020[["San_Pedro_de_Melipilla"]]), max(Hum_2020[["Carmen_Rosa"]])),
  mean_Hum = c(mean(Hum_2020[["Los_Tilos"]]), mean(Hum_2020[["San_Antonio_de_Naltahua"]]),
           mean(Hum_2020[["San_Pedro_de_Melipilla"]]), mean(Hum_2020[["Carmen_Rosa"]])),
  sum_Hum = c(sum(Hum_2020[["Los_Tilos"]]), sum(Hum_2020[["San_Antonio_de_Naltahua"]]),
          sum(Hum_2020[["San_Pedro_de_Melipilla"]]), sum(Hum_2020[["Carmen_Rosa"]])),
  median_Hum = c(median(Hum_2020[["Los_Tilos"]]), median(Hum_2020[["San_Antonio_de_Naltahua"]]),
             median(Hum_2020[["San_Pedro_de_Melipilla"]]), median(Hum_2020[["Carmen_Rosa"]])),
  stdv_Hum = c(sd(Hum_2020[["Los_Tilos"]]), sd(Hum_2020[["San_Antonio_de_Naltahua"]]),
           sd(Hum_2020[["San_Pedro_de_Melipilla"]]), sd(Hum_2020[["Carmen_Rosa"]])),
  Year = '2020',
  Weather_station = c("Los_Tilos", "San_Antonio_de_Naltahua",
                      "San_Pedro_de_Melipilla", "Carmen_Rosa"))

Hum_fn <- rbind(Hum_2018, Hum_2019, Hum_2020)

# Save dataframe as .CSV
write.csv(Hum_fn, 
          paste0('./Original_data/weather_station/Carmen_Rosa/Humidity.csv'), 
          row.names = F, quote = F)

xyplot_Hum <- xyplot(HumData,Carmen_Rosa ~ Los_Tilos + San_Antonio_de_Naltahua +
                       San_Pedro_de_Melipilla,pch=18,cex=1)
xyplot_Hum

### All combined in one panel
### density plot of head circumference per imputation
### blue is observed, red is imputed
densityplot_Hum <- densityplot(HumData, ~ Carmen_Rosa)
densityplot_Hum

densityplot_Hum <- densityplot(HumData, ~ Carmen_Rosa | .imp)
densityplot_Hum

par(mfrow=c(2,2))
plot(Hum$Time_UTC_Chile, Hum$Carmen_Rosa)
plot(completedData_Hum$Time_UTC_Chile, completedData_Hum$Carmen_Rosa)

# Pooling
# The variable modelFit1 containts the results of the fitting performed over 
# the imputed datasets, while the pool() function pools them all together
modelFit_Hum <- with(HumData,lm(Carmen_Rosa ~ Los_Tilos + 
                                  San_Antonio_de_Naltahua +
                                  San_Pedro_de_Melipilla))
summary(pool(modelFit_Hum))

################################################################################
# Working just with Growing degree-day data
aggr_plot_GGD <- aggr(GGD[2:5], col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                      labels=names(GGD[2:5]), cex.axis=.7, gap=3, 
                      ylab=c("Histogram of missing Growing degree-day","Pattern"))

# Bloxplot of Carmen Rosa ws V/s the other weathe station
marginplot(GGD[c(2,5)])
marginplot(GGD[c(3,5)])
marginplot(GGD[c(4,5)])

# Random Forest for Matching
#GGDData <- mice(GGD[2:5], meth='rf', ntree = 10)

# Predictive Mean Matching
GGDData <- mice(GGD[2:5], m=5, maxit=50, meth='pmm', seed=500)
summary(GGDData)

completedData_GGD <- cbind(GGD[1], complete(GGDData,1))

GGD_2018 <- completedData_GGD[completedData_GGD$Time_UTC_Chile >= "17-10-01" & 
                                completedData_GGD$Time_UTC_Chile <= "17-12-31",]

GGD_2018 <- data.frame(
  min_GGD = c(min(GGD_2018[["Los_Tilos"]]), min(GGD_2018[["San_Antonio_de_Naltahua"]]),
          min(GGD_2018[["San_Pedro_de_Melipilla"]]), min(GGD_2018[["Carmen_Rosa"]])),
  max_GGD = c(max(GGD_2018[["Los_Tilos"]]), max(GGD_2018[["San_Antonio_de_Naltahua"]]),
          max(GGD_2018[["San_Pedro_de_Melipilla"]]), max(GGD_2018[["Carmen_Rosa"]])),
  mean_GGD = c(mean(GGD_2018[["Los_Tilos"]]), mean(GGD_2018[["San_Antonio_de_Naltahua"]]),
           mean(GGD_2018[["San_Pedro_de_Melipilla"]]), mean(GGD_2018[["Carmen_Rosa"]])),
  sum_GGD = c(sum(GGD_2018[["Los_Tilos"]]), sum(GGD_2018[["San_Antonio_de_Naltahua"]]),
          sum(GGD_2018[["San_Pedro_de_Melipilla"]]), sum(GGD_2018[["Carmen_Rosa"]])),
  median_GGD = c(median(GGD_2018[["Los_Tilos"]]), median(GGD_2018[["San_Antonio_de_Naltahua"]]),
             median(GGD_2018[["San_Pedro_de_Melipilla"]]), median(GGD_2018[["Carmen_Rosa"]])),
  stdv_GGD = c(sd(GGD_2018[["Los_Tilos"]]), sd(GGD_2018[["San_Antonio_de_Naltahua"]]),
           sd(GGD_2018[["San_Pedro_de_Melipilla"]]), sd(GGD_2018[["Carmen_Rosa"]])),
  Year = '2018',
  Weather_station = c("Los_Tilos", "San_Antonio_de_Naltahua",
                      "San_Pedro_de_Melipilla", "Carmen_Rosa"))

GGD_2019 <- completedData_GGD[completedData_GGD$Time_UTC_Chile >= "18-10-01" & 
                                completedData_GGD$Time_UTC_Chile <= "18-12-31",]

GGD_2019 <- data.frame(
  min_GGD = c(min(GGD_2019[["Los_Tilos"]]), min(GGD_2019[["San_Antonio_de_Naltahua"]]),
          min(GGD_2019[["San_Pedro_de_Melipilla"]]), min(GGD_2019[["Carmen_Rosa"]])),
  max_GGD = c(max(GGD_2019[["Los_Tilos"]]), max(GGD_2019[["San_Antonio_de_Naltahua"]]),
          max(GGD_2019[["San_Pedro_de_Melipilla"]]), max(GGD_2019[["Carmen_Rosa"]])),
  mean_GGD = c(mean(GGD_2019[["Los_Tilos"]]), mean(GGD_2019[["San_Antonio_de_Naltahua"]]),
           mean(GGD_2019[["San_Pedro_de_Melipilla"]]), mean(GGD_2019[["Carmen_Rosa"]])),
  sum_GGD = c(sum(GGD_2019[["Los_Tilos"]]), sum(GGD_2019[["San_Antonio_de_Naltahua"]]),
          sum(GGD_2019[["San_Pedro_de_Melipilla"]]), sum(GGD_2019[["Carmen_Rosa"]])),
  median_GGD = c(median(GGD_2019[["Los_Tilos"]]), median(GGD_2019[["San_Antonio_de_Naltahua"]]),
             median(GGD_2019[["San_Pedro_de_Melipilla"]]), median(GGD_2019[["Carmen_Rosa"]])),
  stdv_GGD = c(sd(GGD_2019[["Los_Tilos"]]), sd(GGD_2019[["San_Antonio_de_Naltahua"]]),
           sd(GGD_2019[["San_Pedro_de_Melipilla"]]), sd(GGD_2019[["Carmen_Rosa"]])),
  Year = '2019',
  Weather_station = c("Los_Tilos", "San_Antonio_de_Naltahua",
                      "San_Pedro_de_Melipilla", "Carmen_Rosa"))

GGD_2020 <- completedData_GGD[completedData_GGD$Time_UTC_Chile >= "19-10-01" & 
                                completedData_GGD$Time_UTC_Chile <= "19-12-31",]

GGD_2020 <- data.frame(
  min_GGD = c(min(GGD_2020[["Los_Tilos"]]), min(GGD_2020[["San_Antonio_de_Naltahua"]]),
          min(GGD_2020[["San_Pedro_de_Melipilla"]]), min(GGD_2020[["Carmen_Rosa"]])),
  max_GGD = c(max(GGD_2020[["Los_Tilos"]]), max(GGD_2020[["San_Antonio_de_Naltahua"]]),
          max(GGD_2020[["San_Pedro_de_Melipilla"]]), max(GGD_2020[["Carmen_Rosa"]])),
  mean_GGD = c(mean(GGD_2020[["Los_Tilos"]]), mean(GGD_2020[["San_Antonio_de_Naltahua"]]),
           mean(GGD_2020[["San_Pedro_de_Melipilla"]]), mean(GGD_2020[["Carmen_Rosa"]])),
  sum_GGD = c(sum(GGD_2020[["Los_Tilos"]]), sum(GGD_2020[["San_Antonio_de_Naltahua"]]),
          sum(GGD_2020[["San_Pedro_de_Melipilla"]]), sum(GGD_2020[["Carmen_Rosa"]])),
  median_GGD = c(median(GGD_2020[["Los_Tilos"]]), median(GGD_2020[["San_Antonio_de_Naltahua"]]),
             median(GGD_2020[["San_Pedro_de_Melipilla"]]), median(GGD_2020[["Carmen_Rosa"]])),
  stdv_GGD = c(sd(GGD_2020[["Los_Tilos"]]), sd(GGD_2020[["San_Antonio_de_Naltahua"]]),
           sd(GGD_2020[["San_Pedro_de_Melipilla"]]), sd(GGD_2020[["Carmen_Rosa"]])),
  Year = '2020',
  Weather_station = c("Los_Tilos", "San_Antonio_de_Naltahua",
                      "San_Pedro_de_Melipilla", "Carmen_Rosa"))

GGD_fn <- rbind(GGD_2018, GGD_2019, GGD_2020)

# Save dataframe as .CSV
write.csv(GGD_fn, 
          paste0('./Original_data/weather_station/Carmen_Rosa/GGD.csv'), 
          row.names = F, quote = F)

xyplot_GGD <- xyplot(GGDData,Carmen_Rosa ~ Los_Tilos +San_Antonio_de_Naltahua +
                       San_Pedro_de_Melipilla, pch=18,cex=1)
xyplot_GGD

### All combined in one panel
### density plot of head circumference per imputation
### blue is observed, red is imputed
densityplot_GGD <- densityplot(GGDData, ~ Carmen_Rosa)
densityplot_GGD
densityplot_GGD <- densityplot(GGDData, ~ Carmen_Rosa | .imp)
densityplot_GGD

par(mfrow=c(2,2))
plot(GGD$Time_UTC_Chile, GGD$Carmen_Rosa)
plot(completedData_GGD$Time_UTC_Chile, completedData_GGD$Carmen_Rosa)

# Pooling
# The variable modelFit1 containts the results of the fitting performed over 
# the imputed datasets, while the pool() function pools them all together
modelFit_GGD <- with(GGDData,lm(Carmen_Rosa ~ Los_Tilos + 
                                  San_Antonio_de_Naltahua +
                                  San_Pedro_de_Melipilla))
summary(pool(modelFit_GGD))

# https://datascienceplus.com/imputing-missing-data-with-r-mice-package/
# https://www.researchgate.net/post/Is_there_an_approach_or_R-package_for_imputing_missing_values_in_time_series_data
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3074241/
# http://juliejosse.com/wp-content/uploads/2018/06/DataAnalysisMissingR.html
# https://www.gerkovink.com/miceVignettes/Convergence_pooling/Convergence_and_pooling.html
# https://stefvanbuuren.name/fimd/index.html#section
# 
# Used for Test https://datascienceplus.com/imputing-missing-data-with-r-mice-package/ 
# Imputing Missing data with Mice package
#install.packages("mice")
#install.packages("VIM")
library(mice)
library(VIM)

setwd('/home/diego/GITHUP_REPO/Master_Thesis_EAGLE/Original_data/weather_station/Quimetal_imputation')

save_to <- '/home/diego/GITHUP_REPO/Master_Thesis_EAGLE/Original_data/weather_station/Quimetal'

# Temperature
Temperature <- read.csv("./Temperature.csv", na.strings = c("NA", " ", "-999"))

Temperature <- transform(Temperature, 
                         Time_UTC_Chile =  as.Date(Time_UTC_Chile, format =  "%m/%d/%Y"),
                         Talagante_Talagante = as.numeric(Talagante_Talagante))

Temp_2020 <- Temperature[Temperature$Time_UTC_Chile >= "19-10-01" &
                           Temperature$Time_UTC_Chile <= "19-12-31",]

Temp_2020 <- data.frame(
  min_Temp = min(Temp_2020[["Talagante_Talagante"]]),
  max_Temp = max(Temp_2020[["Talagante_Talagante"]]),
  mean_Temp = mean(Temp_2020[["Talagante_Talagante"]]),
  sum_Temp = sum(Temp_2020[["Talagante_Talagante"]]),
  median_Temp = median(Temp_2020[["Talagante_Talagante"]]),
  stdv_Temp = sd(Temp_2020[["Talagante_Talagante"]]),
  Year = '2020',
  Weather_station = "Talagante_Talagante")

# Save dataframe as .CSV
write.csv(Temp_2020, 
          paste0(save_to,'/Temperature.csv'), 
          row.names = F, quote = F)

################################################################################

# Relative Humidity
Hum <- read.csv("./relative_humidity.csv", na.strings = c("NA", " ", "-999"))

Hum <- transform(Hum, 
                 Time_UTC_Chile =  as.Date(Time_UTC_Chile, format =  "%m/%d/%Y"),
                 Talagante_Talagante = as.numeric(Talagante_Talagante))

Hum_2020 <- Hum[Hum$Time_UTC_Chile >= "19-10-01" & 
                  Hum$Time_UTC_Chile <= "19-12-31",]

Hum_2020 <- data.frame(
  min_Hum = min(Hum_2020[["Talagante_Talagante"]]),
  max_Hum = max(Hum_2020[["Talagante_Talagante"]]),
  mean_Hum = mean(Hum_2020[["Talagante_Talagante"]]),
  sum_Hum = sum(Hum_2020[["Talagante_Talagante"]]),
  median_Hum = median(Hum_2020[["Talagante_Talagante"]]),
  stdv_Hum = sd(Hum_2020[["Talagante_Talagante"]]),
  Year = '2020',
  Weather_station = "Talagante_Talagante")

# Save dataframe as .CSV
write.csv(Hum_2020, 
          paste0(save_to,'/Humidity.csv'), 
          row.names = F, quote = F)

################################################################################

# Growing degree-day
GGD <- read.csv("./grade_day.csv", na.strings = c("NA", " ", "-999"))

GGD <- transform(GGD, 
                 Time_UTC_Chile =  as.Date(Time_UTC_Chile, format =  "%m/%d/%Y"),
                 Talagante_Talagante = as.numeric(Talagante_Talagante))

GGD_2020 <- GGD[GGD$Time_UTC_Chile >= "19-10-01" & 
                  GGD$Time_UTC_Chile <= "19-12-31",]

GGD_2020 <- data.frame(
  min_GGD = min(GGD_2020[["Talagante_Talagante"]]),
  max_GGD = max(GGD_2020[["Talagante_Talagante"]]),
  mean_GGD = mean(GGD_2020[["Talagante_Talagante"]]),
  sum_GGD = sum(GGD_2020[["Talagante_Talagante"]]),
  median_GGD = median(GGD_2020[["Talagante_Talagante"]]),
  stdv_GGD = sd(GGD_2020[["Talagante_Talagante"]]),
  Year = '2020',
  Weather_station = "Talagante_Talagante")

# Save dataframe as .CSV
write.csv(GGD_2020, 
          paste0(save_to,'/GGD.csv'), 
          row.names = F, quote = F)

################################################################################

# Evapotranspiration
Evapo <- read.csv("./Evapotranpiration.csv", na.strings = c("NA", " ", "-999"))
Evapo <- transform(Evapo,
                   Time_UTC_Chile =  as.Date(Time_UTC_Chile, format =  "%m/%d/%Y"),
                   Los_Tilos = as.numeric(Los_Tilos),
                   San_Antonio_de_Naltahua = as.numeric(San_Antonio_de_Naltahua),
                   San_Pedro_de_Melipilla = as.numeric(San_Pedro_de_Melipilla),
                   Talagante_Talagante = as.numeric(Talagante_Talagante))

# Summary to obsevate the data distribution
summary(Temperature)
summary(Evapo)
summary(Hum)
summary(GGD)

# MCAR: missing completely at random. 
pMiss <- function(x){sum(is.na(x))/length(x)*100}

# Observation of data missing for:
# Evapotranspiration
apply(Evapo,2,pMiss)
apply(Evapo,1,pMiss)

# Looking at missing data pattern
md.pattern(Evapo)

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

Evapo_2020 <- completedData_Evapo[completedData_Evapo$Time_UTC_Chile >= "19-10-01" & 
                                    completedData_Evapo$Time_UTC_Chile <= "19-12-31",]

Evapo_2020 <- data.frame(
  min_Evapo = c(min(Evapo_2020[["Los_Tilos"]]), min(Evapo_2020[["San_Antonio_de_Naltahua"]]),
                min(Evapo_2020[["San_Pedro_de_Melipilla"]]), min(Evapo_2020[["Talagante_Talagante"]])),
  max_Evapo = c(max(Evapo_2020[["Los_Tilos"]]), max(Evapo_2020[["San_Antonio_de_Naltahua"]]),
                max(Evapo_2020[["San_Pedro_de_Melipilla"]]), max(Evapo_2020[["Talagante_Talagante"]])),
  mean_Evapo = c(mean(Evapo_2020[["Los_Tilos"]]), mean(Evapo_2020[["San_Antonio_de_Naltahua"]]),
                 mean(Evapo_2020[["San_Pedro_de_Melipilla"]]), mean(Evapo_2020[["Talagante_Talagante"]])),
  sum_Evapo = c(sum(Evapo_2020[["Los_Tilos"]]), sum(Evapo_2020[["San_Antonio_de_Naltahua"]]),
                sum(Evapo_2020[["San_Pedro_de_Melipilla"]]), sum(Evapo_2020[["Talagante_Talagante"]])),
  median_Evapo = c(median(Evapo_2020[["Los_Tilos"]]), median(Evapo_2020[["San_Antonio_de_Naltahua"]]),
                   median(Evapo_2020[["San_Pedro_de_Melipilla"]]), median(Evapo_2020[["Talagante_Talagante"]])),
  stdv_Evapo = c(sd(Evapo_2020[["Los_Tilos"]]), sd(Evapo_2020[["San_Antonio_de_Naltahua"]]),
                 sd(Evapo_2020[["San_Pedro_de_Melipilla"]]), sd(Evapo_2020[["Talagante_Talagante"]])),
  Year = '2020',
  Weather_station = c("Los_Tilos", "San_Antonio_de_Naltahua",
                      "San_Pedro_de_Melipilla", "Talagante_Talagante"))

Evapo_fn <- rbind(Evapo_2020)

# Save dataframe as .CSV
write.csv(Evapo_fn, 
          paste0(save_to,'/Evapotranspiration.csv'), 
          row.names = F, quote = F)

xyplot_Evapo <- xyplot(EvapoData,Talagante_Talagante ~ Los_Tilos + San_Antonio_de_Naltahua +
                         San_Pedro_de_Melipilla,pch=18,cex=1)
xyplot_Evapo

### All combined in one panel
### density plot of head circumference per imputation
### blue is observed, red is imputed
densityplot_Evapo <- densityplot(EvapoData, ~ Talagante_Talagante)
densityplot_Evapo

densityplot_Evapo <- densityplot(EvapoData, ~ Talagante_Talagante | .imp)
densityplot_Evapo

par(mfrow=c(2,2))
plot(Evapo$Time_UTC_Chile, Evapo$Talagante_Talagante)
plot(completedData_Evapo$Time_UTC_Chile, completedData_Evapo$Talagante_Talagante)

# Pooling
# The variable modelFit1 containts the results of the fitting performed over 
# the imputed datasets, while the pool() function pools them all together
modelFit_Evapo <- with(EvapoData,lm(Talagante_Talagante ~ Los_Tilos + 
                                      San_Antonio_de_Naltahua +
                                      San_Pedro_de_Melipilla))
summary(pool(modelFit_Evapo))

pool.r.squared(modelFit_Evapo)

################################################################################
# plot zone

# Evapo
par(mfrow=c(2,2))
plot(Evapo$Time_UTC_Chile, Evapo$Talagante_Talagante,
     main = "Evapotranspiration Loreto farm - Before imputing",
     xlab = "Month",
     ylab = expression('ET'[c]*'(mm'^.*'d'^-1*')'),
     col = "brown")
plot(completedData_Evapo$Time_UTC_Chile, completedData_Evapo$Talagante_Talagante,
     main = "Evapotranspiration Loreto farm - After impute",
     xlab = "Month",
     ylab = expression('ET'[c]*'(mm'^.*'d'^-1*')'),
     col = "brown")


library(dplyr)
library(ggplot2)
library(ggpmisc)
library(PerformanceAnalytics)
library(ggthemes)
library(corrplot)
library(psych)
library(caret)
library(caretEnsemble)
library(doParallel)
library(tidyverse)
library(kernlab)
library(skimr)
library(vip)

# set a seed
seed <- 7
set.seed(seed)

# set color corrplot
col <- colorRampPalette(c("red", "green", "blue"))(20)

# Set the folder location
setwd('/home/diego/GITHUP_REPO/Master_Thesis_EAGLE/')

# Import dataset
# The dataset used is summary.csv
Field_Carmen <- read.csv('./Original_data/Final_values/fn_Merge_Carmen_Rosa_Field_Band_median_2017_to_2019.csv')
Field_Carmen$Field <- NULL
Field_Carmen$Variety <- NULL

# Observation of the dataset
glimpse(Field_Carmen)
summary(Field_Carmen$Kg_He)
summary(Field_Carmen)

# Clean column 'Week' & 'Year' in the data frame
Field_Carmen$id <- NULL
Field_Carmen$Year <- NULL

# Transform to dummy variable
dmy <- dummyVars(' ~ .',data = Field_Carmen, fullRank = T)
Field_Carmen <- data.frame(predict(dmy, newdata = Field_Carmen))
#print(Field_Carmen)

# Correlation plot
corrplot(cor(Field_Carmen), method = 'square')

# Revision of outliers
Radar_outliers <- Field_Carmen[,19:20]
Weather_Station_outliers <- Field_Carmen[,21:45]
Fertilizers <- Field_Carmen[,46:50]

# Boxplot for the outliers to visual check
boxplot(Radar_outliers, col = 'orange', main = 'Features Boxplot')
boxplot(Weather_Station_outliers, col = 'orange', main = 'Features Boxplot')
boxplot(Fertilizers, col = 'orange', main = 'Features Boxplot')

# Analysis of outliers
boxplot(Field_Carmen$VV_median, col = 'red', main = 'Features Boxplot - VV_median')
boxplot(Fertilizers$N.Unit.He, col = 'orange', main = 'Features Boxplot - N.Unit.He')
boxplot(Fertilizers$P.Unit.He, col = 'orange', main = 'Features Boxplot - P.Unit.He')
boxplot(Fertilizers$CaO.Unit.He, col = 'orange', main = 'Features Boxplot - CaO.Unit.He')

# Numerical analysis of outliers
VV_median_outliers <- which(Field_Carmen$VV_median > -9.5 |
                              Field_Carmen$VV_median < -10.8)
Field_Carmen[VV_median_outliers, 'VV_median']
#[1] -11.437621 -11.317013  -9.246067  -9.166364

N.Unit.He_outliers <- which(Field_Carmen$N.Unit.He > 100 |
                              Field_Carmen$N.Unit.He < 40)
Field_Carmen[N.Unit.He_outliers, 'N.Unit.He']
#[1] 160 113 158 160 140 140   0 140

P.Unit.He_outliers <- which(Field_Carmen$P.Unit.He > 40 |
                              Field_Carmen$P.Unit.He < 20)
Field_Carmen[P.Unit.He_outliers, 'P.Unit.He']
#[1] 60 60 70

CaO.Unit.He_outliers <- which(Field_Carmen$CaO.Unit.He > 40 |
                                Field_Carmen$CaO.Unit.He < 25)
Field_Carmen[CaO.Unit.He_outliers, 'CaO.Unit.He']
#[1] 14.4 64.0 14.4 62.4 64.0 48.0 48.0  0.0  0.0 48.0

# Because the outliers for each variable represent a number that can affect the
# calculation, we have decided not to remove.

################################################################################
# Check for multicollinearity in numeric data. 
# Variance Inflation Factors
simple_glm <- glm(Kg_He ~ ., data = Field_Carmen)
vif(simple_glm)
# there are aliased coefficients in the model

# Find Aliases (Dependencies) in a Model
alias(glm(Kg_He ~ ., data = Field_Carmen))

# Calculate correlation matrix
correlationMatrix <- cor(Field_Carmen)

# Summarize the correlation matrix
print(correlationMatrix)

# Find attributes that are highly corrected (ideally >0.7)
highlyCorrelated <- findCorrelation(correlationMatrix, 
                                    cutoff=0.7)

# print indexes of highly correlated attributes
print(highlyCorrelated)

# The above piece of code gives the variables where the correaltion is higher 
# then 70%. We will eliminate these variables from our dataset.
Field_Carmen_nocorr <- Field_Carmen[-highlyCorrelated]
Field_Carmen_nocorr

# Adding Dependent varible in the dataset.
Field_Carmen_nocorr <- cbind(VH_median = Field_Carmen$VH_median,
                             Field_Carmen_nocorr)

# New correlation analysis
corrplot(cor(Field_Carmen_nocorr), method = 'number', tl.srt=45,
         type = "lower", col=col)

# Reorden the dataframe
Field_Carmen_nocorr <- Field_Carmen_nocorr %>%
  relocate('Kg_He', 'VH_median', 'VV_median')

################################################################################
# Data preparation for Feature selection using Caret 
# the Recursive Feature Elimination (RFE)

variables_nocorr <- Field_Carmen_nocorr
variables_nocorr$id <- NULL
variables_nocorr$Year <- NULL
variables_nocorr$VarietyINIAGRAPEONE <- NULL
variables_nocorr$VarietySABLE <- NULL
variables_nocorr$VarietyTHOMPSON <- NULL
variables_nocorr$VarietyTIMCO <- NULL

# New correlation analysis
corrplot(cor(variables_nocorr), method = 'number')

# Tuning parameters for feature selection
number = 10
n_repeats = 4
#metric_rfe <- 'Rsquared'
#metric_rfe <- 'RMSE'
metric_rfe <- 'MAE'
subsets <- c(1:9)

set.seed(seed)

# Recursive Feature Elimination
# https://topepo.github.io/caret/recursive-feature-elimination.html
# Define the control using a random forest selection function
control <- rfeControl(functions = rfFuncs,
                      method = 'repeatedcv',
                      repeats = n_repeats,
                      number = number,
                      verbose = FALSE)

# Run the RFE algorithm
results <- rfe(variables_nocorr[,-1], 
               variables_nocorr[,1],
               sizes = subsets, 
               metric = metric_rfe,
               rfeControl = control)

# Summarize the results
print(results)

# Plot the results
plot(results, type=c('g', 'o'))

# List the chosen variables
List_variables <- predictors(results)
List_variables

# Include variables
List_variables <- append(c('Kg_He'), List_variables)
List_variables

# Variable selection
var_sel <- Field_Carmen %>% select(List_variables)

# New correlation analysis but now for the selected variables
corrplot(cor(var_sel), method = 'number')

# https://statisticsbyjim.com/regression/no-p-values-nonlinear-regression/
################################################################################
# Split the data into training and test set
train_fraction <- 0.66

set.seed(seed)

training.samples <- var_sel$Kg_He %>%
  createDataPartition(p = train_fraction, list = FALSE)

train.data  <- var_sel[training.samples, ]
train.data$id <- NULL

test.data <- var_sel[-training.samples, ]
test.data$id <- NULL

# show key descriptive stats for each column.
skimmed <- skim(train.data)
skimmed[, c(1:12)]

################################################################################
# Machine Learning method

# Tuning parameters
method <- 'LOOCV'
#method <- 'repeatedcv'
number_m = 10 # number of folds
n_repeats_m = 4 # repetitions doing cross validation process
#metric <- 'Rsquared' # We want to obtaind the best Rsquared in each ML model
#metric <- 'RMSE' # We want to obtaind the lowes RMSE in each ML model
metric <- 'MAE'
tunegrid <- expand.grid(.mtry=c(1:12))
ntree <- 1000
sigma <- c(0:1)

# Preprocess from Caret
preprocess <- c('center', 'scale')
#preprocess <- c('range')

# Calculate Tunlength with values around
# rule of thumb
tunlength <- round(sqrt(nrow(train.data)), digits = 0)

# Define training control for all the three methods
train.control <- trainControl(method = method,
                              #repeats = n_repeats_m,
                              #number = number_m,
                              allowParallel = T,
                              savePredictions = T)

# Train the model
# Random Forest
set.seed(seed)
model_rf <- train(Kg_He ~., 
                  data = train.data, 
                  method = 'rf',
                  #ntree = ntree,
                  metric = metric,
                  #tuneGrid = tunegrid,
                  tunlength = tunlength,
                  preProcess = preprocess,
                  trControl = train.control)

# Conditional Random Forest
set.seed(seed)
model_crf <- train(Kg_He ~., 
                   data = train.data, 
                   method = 'cforest',
                   #ntree = ntree,
                   metric = metric,
                   #tuneGrid = tunegrid,
                   preProcess = preprocess,
                   trControl = train.control)

# Gaussian Process with Radial Basis Function Kernel
set.seed(seed)
model_gau <- train(Kg_He ~., 
                   data = train.data, 
                   method = 'gaussprRadial',
                   #sigma = sigma,
                   metric = metric,
                   preProcess = preprocess,
                   trControl = train.control)

################################################################################
# Summarize the results for each model:
# Random Forest
print(model_rf)

# Conditional Random Forest
print(model_crf)

# Gaussian Process
print(model_gau)

################################################################################
# Checking variable importance for:

# Variable Importance Random Forest
varImp(model_rf)

# Plotting Variable Importance
plot(varImp(model_rf),main = 'RF - Variable Importance S1')

# Variable importance
varimp_data_rf <- data.frame(feature = row.names(varImp(model_rf)$importance)[1:10],
                             importance = varImp(model_rf)$importance[1:10, 1])

ggplot(data = varimp_data_rf, 
       aes(x = reorder(feature, -importance), y = importance, fill = feature)) +
  geom_bar(stat="identity") + labs(x = "Features", y = "Variable Importance (%)") + 
  geom_text(aes(label = round(importance, 2)), vjust=1.6, color="white", size=4) + 
  #theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) +
  theme(axis.text.x=element_text(colour="black")) +
  theme(legend.position = "none")

# Variable Importance Condition Random Forest
varImp(model_crf)

# Plotting Variable Importance
plot(varImp(model_crf),main = 'CRF - Variable Importance S1')

# Variable importance
varimp_data_crf <- data.frame(feature = row.names(varImp(model_crf)$importance)[1:10],
                              importance = varImp(model_crf)$importance[1:10, 1])

ggplot(data = varimp_data_crf, 
       aes(x = reorder(feature, -importance), y = importance, fill = feature)) +
  geom_bar(stat="identity") + labs(x = "Features", y = "Variable Importance (%)") + 
  geom_text(aes(label = round(importance, 2)), vjust=1.6, color="white", size=4) + 
  #theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) +
  theme(axis.text.x=element_text(colour="black")) +
  theme(legend.position = "none")

# Variable Importance Gaussian Process
varImp(model_gau)

# Plotting Variable Importance
plot(varImp(model_gau),main = 'Gaussian - Variable Importance S1')

# Variable importance
varimp_data_gau <- data.frame(feature = row.names(varImp(model_gau)$importance)[1:10],
                              importance = varImp(model_gau)$importance[1:10, 1])

ggplot(data = varimp_data_gau, 
       aes(x = reorder(feature, -importance), y = importance, fill = feature)) +
  geom_bar(stat="identity") + labs(x = "Features", y = "Variable Importance (%)") + 
  geom_text(aes(label = round(importance, 2)), vjust=1.6, color="white", size=4) + 
  #theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) +
  theme(axis.text.x=element_text(colour="black")) +
  theme(legend.position = "none")

################################################################################
# We predict the Model on Train as well as Test to understand how the model is
# performing on both the Training and the Unseen data.

# With Training data
# Random Forest
train_predictions_rf <- predict(model_rf, train.data)

# Conditional Random Forest
train_predictions_crf <- predict(model_crf, train.data)

# Gaussian Process
train_predictions_gau <- predict(model_gau, train.data)

# Let's look at the predicted results with test data
train_predictions_rf
train_predictions_crf
train_predictions_gau

# Creating postResample on Training data.
# Random Forest
postResample(train_predictions_rf, train.data$Kg_He)
# Conditional Random Forest
postResample(train_predictions_crf, train.data$Kg_He)
# Gaussian Process
postResample(train_predictions_gau, train.data$Kg_He)


# With Test data
# Random Forest
predictions_rf <- predict(model_rf, test.data)

# Conditional Random Forest
predictions_crf <- predict(model_crf, test.data)

# Gaussian Process
predictions_gau <- predict(model_gau, test.data)

# Let's look at the predicted results with test data
predictions_rf
predictions_crf
predictions_gau

# Creating postResample on Test data.
# Random Forest
postResample(predictions_rf, test.data$Kg_He)
# Conditional Random Forest
postResample(predictions_crf, test.data$Kg_He)
# Gaussian Process
postResample(predictions_gau, test.data$Kg_He)

################################################################################
# The dataset used is for_pred.csv

# Import dataset
# The dataset used is summary.csv
Field_Carmen_1 <- read.csv('./Original_data/Final_values/fn_Merge_Carmen_Rosa_Field_Band_median_2017_to_2019.csv')
Field_Carmen_1$Field <- NULL
Quimetal <- read.csv('./Original_data/Final_values/fn_Merge_Quimetal_Band_median_2019.csv')
Quimetal$Field <- NULL
Quimetal$Weather_station.y <- NULL

Field_C_Q <- rbind(Quimetal,Field_Carmen_1)

# Transform to dummy variable in Quimetal due to is just one vector
dmy <- dummyVars(' ~ .',data = Field_C_Q, fullRank = T)
Quimetal <- data.frame(predict(dmy, newdata = Field_C_Q))
#print(Quimetal)

Quimetal <- Quimetal[1,]

# Variable selection
var_sel_qu_s1 <- Quimetal %>% select(List_variables)

# Make prediction for the year 2020
# Random Forest
predictions_rf_qu_s1 <- predict(model_rf, var_sel_qu_s1)

# Conditional Random Forest
predictions_crf_qu_s1 <- predict(model_crf, var_sel_qu_s1)

# Gaussian Process
predictions_gau_qu_s1 <- predict(model_gau, var_sel_qu_s1)

# Let's look at the predicted results with test data
# Original value
var_sel_qu_s1$Kg_He

# Predicted values
predictions_rf_qu_s1
predictions_crf_qu_s1
predictions_gau_qu_s1

################################################################################
# data fram with R2, RMSE & MAE
rf <- data.frame(Satellite = 'S1',
                 Model = 'Random Forest',
                 R2 = R2(predictions_rf, test.data$Kg_He),
                 RMSE = RMSE(predictions_rf, test.data$Kg_He),
                 MAE = MAE(predictions_rf, test.data$Kg_He),
                 Predicted = predictions_rf_qu_s1,
                 Original = var_sel_qu_s1$Kg_He)

crf <- data.frame(Satellite = 'S1',
                  Model = 'CForest',
                  R2 = R2(predictions_crf, test.data$Kg_He),
                  RMSE = RMSE(predictions_crf, test.data$Kg_He),
                  MAE = MAE(predictions_crf, test.data$Kg_He),
                  Predicted = predictions_crf_qu_s1,
                  Original = var_sel_qu_s1$Kg_He)

gau <- data.frame(Satellite = 'S1',
                  Model = 'Gaussian Radial Basis',
                  R2 = R2(predictions_gau, test.data$Kg_He),
                  RMSE = RMSE(predictions_gau, test.data$Kg_He),
                  MAE = MAE(predictions_gau, test.data$Kg_He),
                  Predicted = predictions_gau_qu_s1,
                  Original = var_sel_qu_s1$Kg_He)

total_result_S1 <- rbind(rf, crf, gau)

################################################################################
# Basic scatter plot

# Make prediction for the year 2020
# Random Forest
total_predictions_rf_qu_s1 <- predict(model_rf, var_sel)

# Conditional Random Forest
total_predictions_crf_qu_s1 <- predict(model_crf, var_sel)

# Gaussian Process
total_predictions_gau_qu_s1 <- predict(model_gau, var_sel)

# Data predicted & original
values_rf <- data.frame(Satellite = 'S1',
                        Predicted = total_predictions_rf_qu_s1,
                        Observed = var_sel$Kg_He)

values_crf <- data.frame(Satellite = 'S1',
                         Predicted = total_predictions_crf_qu_s1,
                         Observed = var_sel$Kg_He)

values_gau <- data.frame(Satellite = 'S1',
                         Predicted = total_predictions_gau_qu_s1,
                         Observed = var_sel$Kg_He)

ggplot(values_rf, aes(x = Observed, y = Predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = 'blue', size = 1) +
  #  stat_poly_eq(aes(label = paste0("atop(", ..eq.label.., ",", ..rr.label.., ")")), 
  #               formula = y ~ x, parse = TRUE) +
  xlim(0,80000) + ylim(0,80000) +
  labs(title = "Random Forest - Comparison S1",
       x = "Observed", y = "Predicted ") +
  theme_classic()

ggplot(values_crf, aes(x = Observed, y = Predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = 'blue', size = 1) +
  #  stat_poly_eq(aes(label = paste0("atop(", ..eq.label.., ",", ..rr.label.., ")")), 
  #               formula = y ~ x, parse = TRUE) +
  xlim(0,80000) + ylim(0,80000) +
  labs(title = "Conditional Random Forest - Comparison S1",
       x = "Observed", y = "Predicted ") +
  theme_classic()

ggplot(values_gau, aes(x = Observed, y = Predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = 'blue', size = 1) +
  #  stat_poly_eq(aes(label = paste0("atop(", ..eq.label.., ",", ..rr.label.., ")")), 
  #               formula = y ~ x, parse = TRUE) +
  xlim(0,80000) + ylim(0,80000) +
  labs(title = "Gaussian Process with Radial Basis Function Kernel - Comparison S1",
       x = "Observed", y = "Predicted ") +
  theme_classic()

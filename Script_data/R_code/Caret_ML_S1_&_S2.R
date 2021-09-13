pkgs <- c("car", "vip", "caret", "skimr", "psych", "dplyr", "kernlab", "ggplot2", "ggpmisc", 
          "corrplot", "ggthemes", "tidyverse", "doParallel", "caretEnsemble", 
          "PerformanceAnalytics", "randomForest")

for (i in pkgs){
  if (!require(i, character.only = TRUE)){
    install.packages(i, dependencies = TRUE)
    library(i, dependencies=TRUE)
  }
}

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
Field_Carmen$id <- NULL
Field_Carmen$Year <- NULL
Field_Carmen$Field <- NULL
Field_Carmen$Variety <- NULL

################################################################################
# Observation of the dataset
glimpse(Field_Carmen)
summary(Field_Carmen$Kg_He)
summary(Field_Carmen)

# Correlation plot
corrplot(cor(Field_Carmen), method = 'color', tl.srt=45,
         type = "lower", col=col)

################################################################################
# Revision of outliers
Bands_outliers <- Field_Carmen[,3:15]
Index_outliers <- Field_Carmen[,16:19]
Radar_outliers <- Field_Carmen[,20:21]
Weather_Station_outliers <- Field_Carmen[,22:45]
Fertilizers <- Field_Carmen[,46:50]

# Boxplot for the outliers to visual check
boxplot(Bands_outliers, col = 'orange', main = 'Features Boxplot')
boxplot(Index_outliers, col = 'orange', main = 'Features Boxplot')
boxplot(Radar_outliers, col = 'orange', main = 'Features Boxplot')
boxplot(Weather_Station_outliers, col = 'orange', main = 'Features Boxplot')
boxplot(Fertilizers, col = 'orange', main = 'Features Boxplot')

# Analysis of outliers
boxplot(Field_Carmen$B10_median, col = 'red', main = 'Features Boxplot - B10_median')
boxplot(Field_Carmen$VV_median, col = 'red', main = 'Features Boxplot - VV_median')
boxplot(Fertilizers$N.Unit.He, col = 'orange', main = 'Features Boxplot - N.Unit.He')
boxplot(Fertilizers$P.Unit.He, col = 'orange', main = 'Features Boxplot - P.Unit.He')
boxplot(Fertilizers$CaO.Unit.He, col = 'orange', main = 'Features Boxplot - CaO.Unit.He')

# Numerical analysis of outliers
B10_median_outliers <- which(Field_Carmen$B10_median > 0.07835|
                               Field_Carmen$B10_median < 0)
Field_Carmen[B10_median_outliers, 'B10_median']

VV_median_outliers <- which(Field_Carmen$VV_median > -9.5 |
                              Field_Carmen$VV_median < -10.8)
Field_Carmen[VV_median_outliers, 'VV_median']

N.Unit.He_outliers <- which(Field_Carmen$N.Unit.He > 100 |
                                Field_Carmen$N.Unit.He < 40)
Field_Carmen[N.Unit.He_outliers, 'N.Unit.He']

P.Unit.He_outliers <- which(Field_Carmen$P.Unit.He > 40 |
                                Field_Carmen$P.Unit.He < 20)
Field_Carmen[P.Unit.He_outliers, 'P.Unit.He']

CaO.Unit.He_outliers <- which(Field_Carmen$CaO.Unit.He > 40 |
                              Field_Carmen$CaO.Unit.He < 25)
Field_Carmen[CaO.Unit.He_outliers, 'CaO.Unit.He']

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
correlationMatrix <- cor(Field_Carmen[,-1])

# Find attributes that are highly corrected (ideally >0.7)
highlyCorrelated <- findCorrelation(correlationMatrix, 
                                    cutoff=0.7)

# The above piece of code gives the variables where the correaltion is higher 
# then 70%. We will eliminate these variables from our dataset.
Field_Carmen_nocorr <- Field_Carmen[-highlyCorrelated]


# Adding Dependent varible in the dataset.
Field_Carmen_nocorr <- cbind(Index_outliers,
                             #VV_median = Radar_outliers$VV_median,
                             Field_Carmen_nocorr)

# Reorden the dataframe
Field_Carmen_nocorr <- Field_Carmen_nocorr %>%
  relocate('Kg_He')

# New correlation analysis
corrplot(cor(Field_Carmen_nocorr), method = 'color', tl.srt=45,
         type = "lower", col=col)

################################################################################
# Data preparation for Feature selection using Caret 
# the Recursive Feature Elimination (RFE)
variables_nocorr <- Field_Carmen_nocorr

# Tuning parameters for feature selection
number = 10
n_repeats = 4
metric_rfe <- 'MAE'

set.seed(seed)

# Recursive Feature Elimination
# Define the control using a random forest selection function
control <- rfeControl(functions = rfFuncs,
                      method = 'repeatedcv',
                      repeats = n_repeats,
                      number = number,
                      verbose = FALSE)

# Run the RFE algorithm
results <- rfe(variables_nocorr[,-1], 
               variables_nocorr[,1],
               metric = metric_rfe,
               rfeControl = control)

# Summarize the results
print(results)

# Save plot
png(file='/home/diego/GITHUP_REPO/Master_Thesis_EAGLE/Plots/RFE_S1_S2.png',
    width=500, height=400)

# Plot the results
plot(results, type=c('g', 'o'))
dev.off()

# List the chosen variables
List_variables <- predictors(results)
List_variables

# Include variables
List_variables <- append(c('Kg_He', 'VH_median', 'VV_median'), List_variables)
List_variables

# Variable selection
var_sel <- Field_Carmen %>% select(List_variables)

################################################################################
# Split the data into training and test set
train_fraction <- 0.66

set.seed(seed)

training.samples <- var_sel$Kg_He %>%
  createDataPartition(p = train_fraction, list = FALSE)

train.data  <- var_sel[training.samples, ]

test.data <- var_sel[-training.samples, ]

# show key descriptive stats for each column.
skimmed <- skim(train.data)
skimmed[, c(1:12)]

################################################################################
# Machine Learning method

# Tuning parameters
method <- 'LOOCV'
number_m = 10 # number of folds
metric <- 'MAE'
tunlength <- round(sqrt(nrow(train.data)), digits = 0)

# Preprocess from Caret
preprocess <- c('center', 'scale')

# Define training control for all the three methods
train.control <- trainControl(method = method,
                              number = number_m,
                              allowParallel = T,
                              savePredictions = T)

# Train the model
# Random Forest
set.seed(seed)
model_rf <- train(Kg_He ~., 
                  data = train.data, 
                  method = 'rf',
                  metric = metric,
                  tunlength = tunlength,
                  preProcess = preprocess,
                  trControl = train.control)

# Conditional inference forests
set.seed(seed)
model_crf <- train(Kg_He ~., 
                   data = train.data, 
                   method = 'cforest',
                   metric = metric,
                   preProcess = preprocess,
                   trControl = train.control)

# Gaussian Process Regression
set.seed(seed)
model_gau <- train(Kg_He ~., 
                   data = train.data, 
                   method = 'gaussprRadial',
                   metric = metric,
                   preProcess = preprocess,
                   trControl = train.control)

################################################################################
# Summarize the results for each model:
# Random Forest
print(model_rf)

# Conditional inference forests
print(model_crf)

# Gaussian Process Regression
print(model_gau)

################################################################################
# Checking variable importance for:

# Colors ggplot
colors <- c("S1" = "#d66f99", "S2" = "#6777c9", "Farm" = "#48ab66")

# Variable Importance Random Forest
varImp(model_rf)

# Plotting Variable Importance
#plot(varImp(model_rf, scale=FALSE), main = 'RF - Variable Importance S1 & S2')

# Variable importance
varimp_data_rf <- data.frame(feature = row.names(varImp(model_rf)$importance)[1:10],
                          importance = varImp(model_rf)$importance[1:10, 1],
                          Data = c('S1','S1', 'S2', 'S2', 'S2', 'Farm', 'S2', 
                                   'S2', 'S2', 'S2'))

ggplot(data = varimp_data_rf,
       aes(x = reorder(feature, importance), y = importance, fill = Data)) +
  geom_bar(stat = "identity") + labs(x = "Features", y = "Importance") + 
  scale_fill_manual(values = colors) +
  theme(axis.text.x=element_text(colour="black", angle = 30, size = 10),
        axis.text.y=element_text(colour="black")) +
  labs(title = 'RF - Variable Importance ML S1 & S2') + 
  coord_flip() +  theme(plot.title = element_text(hjust = 0.5))

# Save varImp_S1_S2_RF plot
ggsave('/home/diego/GITHUP_REPO/Master_Thesis_EAGLE/Plots/varImp_S1_S2_RF.png')

# Variable Importance Conditional inference forests
varImp(model_crf)

# Plotting Variable Importance
#plot(varImp(model_crf, scale=FALSE),main = 'CIF - Variable Importance S1 & S2')

# Variable importance
varimp_data_crf <- data.frame(feature = row.names(varImp(model_crf)$importance)[1:10],
                          importance = varImp(model_crf)$importance[1:10, 1],
                          Data = c('S1','S1', 'S2', 'S2', 'S2', 'Farm', 'S2', 
                                   'S2', 'S2', 'S2'))

ggplot(data = varimp_data_crf, 
       aes(x = reorder(feature, importance), y = importance, fill = Data)) +
  geom_bar(stat = "identity") + labs(x = "Features", y = "Importance") + 
  scale_fill_manual(values = colors) +
  theme(axis.text.x=element_text(colour="black", angle = 30, size = 10),
        axis.text.y=element_text(colour="black")) +
  labs(title = 'CIF - Variable Importance ML S1 & S2') + 
  coord_flip() + theme(plot.title = element_text(hjust = 0.5))

# Save varImp_S1_S2_CIF plot
ggsave('/home/diego/GITHUP_REPO/Master_Thesis_EAGLE/Plots/varImp_S1_S2_CIF.png')

# Variable Importance Gaussian Process Regression
varImp(model_gau)

# Plotting Variable Importance
#plot(varImp(model_gau, scale=FALSE),main = 'GPR - Variable Importance S1 & S2')

# Variable importance
varimp_data_gau <- data.frame(feature = row.names(varImp(model_gau)$importance)[1:10],
                              importance = varImp(model_gau)$importance[1:10, 1],
                              Data = c('S1','S1', 'S2', 'S2', 'S2', 'Farm', 'S2', 
                                       'S2', 'S2', 'S2'))

ggplot(data = varimp_data_gau, 
       aes(x = reorder(feature, importance), y = importance, fill = Data)) +
  geom_bar(stat = "identity") + labs(x = "Features", y = "Importance") + 
  scale_fill_manual(values = colors) +
  theme(axis.text.x=element_text(colour="black", angle = 30, size = 10),
        axis.text.y=element_text(colour="black")) +
  labs(title = 'GPR - Variable Importance ML S1 & S2') + 
  coord_flip() + theme(plot.title = element_text(hjust = 0.5))

# Save varImp_S1_S2_GPR plot
ggsave('/home/diego/GITHUP_REPO/Master_Thesis_EAGLE/Plots/varImp_S1_S2_GPR.png')

################################################################################
# Test data predictions
# Random Forest
predictions_rf <- predict(model_rf, test.data)

# Conditional inference forests
predictions_crf <- predict(model_crf, test.data)

# Gaussian Process Regression
predictions_gau <- predict(model_gau, test.data)

# Let's look at the predicted results with test data
predictions_rf
predictions_crf
predictions_gau

# Creating postResample on Test data.
# Random Forest
postResample(predictions_rf, test.data$Kg_He)

# Conditional inference forests
postResample(predictions_crf, test.data$Kg_He)

# Gaussian Process Regression
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

# Variable selection
var_sel_qu_s1_s2 <- Quimetal %>% select(List_variables)

# Make prediction for the year 2020
# Random Forest
predictions_rf_qu_s1_s2 <- predict(model_rf, var_sel_qu_s1_s2)

# Conditional inference forests
predictions_crf_qu_s1_s2 <- predict(model_crf, var_sel_qu_s1_s2)

# Gaussian Process Regression
predictions_gau_qu_s1_s2 <- predict(model_gau, var_sel_qu_s1_s2)

# Let's look at the predicted results with test data
# Original value
var_sel_qu_s1_s2$Kg_He

# Predicted values
predictions_rf_qu_s1_s2
predictions_crf_qu_s1_s2
predictions_gau_qu_s1_s2

################################################################################
# data fram with R2, RMSE & MAE
rf <- data.frame(Satellite = 'S1 & S2',
                 Model = 'Random Forest',
                 R2 = R2(predictions_rf, test.data$Kg_He),
                 RMSE = RMSE(predictions_rf, test.data$Kg_He),
                 MAE = MAE(predictions_rf, test.data$Kg_He),
                 Predicted = predictions_rf_qu_s1_s2,
                 Original = var_sel_qu_s1_s2$Kg_He)

crf <- data.frame(Satellite = 'S1 & S2',
                  Model = 'CForest',
                  R2 = R2(predictions_crf, test.data$Kg_He),
                  RMSE = RMSE(predictions_crf, test.data$Kg_He),
                  MAE = MAE(predictions_crf, test.data$Kg_He),
                  Predicted = predictions_crf_qu_s1_s2,
                  Original = var_sel_qu_s1_s2$Kg_He)

gau <- data.frame(Satellite = 'S1 & S2',
                  Model = 'Gaussian Radial Basis',
                  R2 = R2(predictions_gau, test.data$Kg_He),
                  RMSE = RMSE(predictions_gau, test.data$Kg_He),
                  MAE = MAE(predictions_gau, test.data$Kg_He),
                  Predicted = predictions_gau_qu_s1_s2,
                  Original = var_sel_qu_s1_s2$Kg_He)

total_result_S1_S2 <- rbind(rf, crf, gau)

################################################################################
# Basic scatter plot

# Make prediction for the year 2020
# Random Forest
total_predictions_rf_qu_s1_s2 <- predict(model_rf, var_sel)

# Conditional inference forests
total_predictions_crf_qu_s1_s2 <- predict(model_crf, var_sel)

# Gaussian Process Regression
total_predictions_gau_qu_s1_s2 <- predict(model_gau, var_sel)

# Data predicted & original
values_rf <- data.frame(Satellite = 'S1 & S2',
                        Predicted = total_predictions_rf_qu_s1_s2,
                        Observed = var_sel$Kg_He)

values_crf <- data.frame(Satellite = 'S1 & S2',
                         Predicted = total_predictions_crf_qu_s1_s2,
                         Observed = var_sel$Kg_He)

values_gau <- data.frame(Satellite = 'S1 & S2',
                         Predicted = total_predictions_gau_qu_s1_s2,
                         Observed = var_sel$Kg_He)

ggplot(values_rf, aes(x = Observed, y = Predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = 'blue') +
  xlim(0,80000) + ylim(0,80000) +
  labs(title = "Random Forest - ML S1 & S2",
       x = "Observed", y = "Predicted ") +
  theme_classic()

# Save RF_S1_S2_scatter_plot plot
ggsave('/home/diego/GITHUP_REPO/Master_Thesis_EAGLE/Plots/RF_S1_S2_scatter_plot.png')

ggplot(values_crf, aes(x = Observed, y = Predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = 'blue') +
  xlim(0,80000) + ylim(0,80000) +
  labs(title = "Conditional inference forests - ML S1 & S2",
       x = "Observed", y = "Predicted ") +
  theme_classic()

# Save CIF_S1_S2_scatter_plot plot
ggsave('/home/diego/GITHUP_REPO/Master_Thesis_EAGLE/Plots/CIF_S1_S2_scatter_plot.png')

ggplot(values_gau, aes(x = Observed, y = Predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = 'blue') +
  xlim(0,80000) + ylim(0,80000) +
  labs(title = "Gaussian Process Regression - ML S1 & S2",
       x = "Observed", y = "Predicted ") +
  theme_classic()

# Save GPR_S1_S2_scatter_plot plot
ggsave('/home/diego/GITHUP_REPO/Master_Thesis_EAGLE/Plots/GPR_S1_S2_scatter_plot.png')

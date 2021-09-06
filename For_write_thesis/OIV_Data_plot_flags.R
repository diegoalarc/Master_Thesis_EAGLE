# https://lynleyaldridge.netlify.app/2021/03/01/visualizing-data-using-ggplot2-and-ggflags/
#load packages
library(tidyverse)
library(ggplot2)
library(ggflags) 
library(countrycode) # to convert country names to country codes
library(tidytext) # for reorder_within
library(scales) # for application of common formats to scale labels (e.g., comma, percent, dollar)

# Load the csv from OIV with the table grapes production in the world
oiv_data <- read.csv("./For_write_thesis/OIV_Data/oiv_export-StatData-2021-06-11-09-05-26.csv")

# Selection of the 10 first which come all in descending order
oiv_data <- as.data.frame(oiv_data[1:10,])

# Percentage of each country production using the total amount of tonnes in the world
Percentage <- oiv_data[,6]/27639358
Percentage <- as.numeric(format(round(Percentage, 3), nsmall = 2))

# Creation of a dataframe with the Country, Values in Tonnes and the percentage of the production
oiv_data <- data.frame(cbind(oiv_data[,2], oiv_data[,6], Percentage))
colnames(oiv_data) <- c("Country","Value", "Percentage")

# Transform as.numeric columns 2:3
oiv_data <- transform(oiv_data, Value = as.numeric(Value), 
                      Percentage = as.numeric(Percentage))

# standardized format to iso2c
oiv_data$Country <- countrycode(oiv_data$Country, "country.name", "iso2c")

mycolors <- c("#F8B195", "#F67280", "#C06C84", "#6C5B7B", "#355C7D")

# Plot
oiv_data %>%
  mutate(code = tolower(Country)) %>%
  ggplot(aes(y = reorder(Country, Percentage), x = Value)) + 
  geom_col(colour = "black", show.legend = F) + 
  geom_flag(x = 0, aes(country = code), size = 10) +
  geom_text(aes(label = paste(format(Percentage*100, nsmall=0), "%")),
            hjust = 1, size = 5, color="white", position = position_dodge(width = 1)) +
  scale_x_continuous(name = "Tons produced", labels = scales::comma) +
  labs(y = "Countries", caption = "Source: OIV (2016)") +
  ggtitle("Major world producers year 2016") + 
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(2)))

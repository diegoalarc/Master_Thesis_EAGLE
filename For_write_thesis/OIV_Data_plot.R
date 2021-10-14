# Data stat from https://www.oiv.int/en/statistiques/recherche
library(ggplot2)
#library(ggthemes)
#library(hrbrthemes)
#library(gghighlight)

# Load the csv from OIV with the table grapes production in the world
oiv_data <- read.csv("./For_write_thesis/OIV_Data/oiv_export-StatData-2021-06-11-09-05-26.csv")
#oiv_data <- read.csv("./For_write_thesis/OIV_Data/OIV_Data_20Sep.csv", sep = ";")
total_tonnes <- sum(as.numeric(oiv_data[,6]))

# Selection of the 10 first which come all in descending order
oiv_data <- as.data.frame(oiv_data[1:10,])

# Percentage of each country production using the total amount of tonnes in the world
Percentage <- oiv_data[,6]/total_tonnes
Percentage <- as.numeric(format(round(Percentage, 3), nsmall = 2))

# Creation of a dataframe with the Country, Values in Tonnes and the percentage of the production
oiv_data <- data.frame(cbind(oiv_data[,2], oiv_data[,6], Percentage))
colnames(oiv_data) <- c("Country","Value", "Percentage")

# Transform as.numeric columns 2:3
oiv_data <- transform(oiv_data, Value = as.numeric(Value), 
                      Percentage = as.numeric(Percentage))

# Plotting a graph with the information of the dataframe from OIV
# https://htmlcolorcodes.com/es/
p <- ggplot(data = oiv_data, aes(y = reorder(Country, Percentage), x = Value)) + 
  geom_bar(aes(fill = Percentage), stat="identity", position = "dodge") +
  scale_fill_gradient(low = "#D2B4DE", high = "#16A085", na.value = NA) +
  geom_text(aes(label = paste(format(Percentage*100, nsmall=0), "%")),
                hjust = 1.2, size = 5, position = position_dodge(0.9)) +
  scale_x_continuous(name = "Tons produced", labels = scales::comma) +
  labs(y = "Countries", caption = "Source: OIV (2016)") +
  ggtitle("Major world producers year 2016") + theme(plot.title = element_text(hjust = 0.5),
                                           legend.position = "none")

plot(p)

#png("Major_world_producers_table_grape_2016.png")
ggsave("Major_world_producers_table_grape_2016.png", width = 30, height = 20, units = "cm")
print(p)
dev.off()
library(zoo)
library(lubridate)
library(ggplot2)
library(hrbrthemes)
library(patchwork)

setwd('/home/diego/GITHUP_REPO/Master_Thesis_EAGLE/Original_data/weather_station/Carmen_Rosa')

GGD_total <- read.csv('./GGD_complete_CR.csv')

GGD_total <- transform(GGD_total,
                       Time_UTC_Chile =  as.Date(Time_UTC_Chile),
                       Los_Tilos = as.numeric(Los_Tilos),
                       San_Antonio_de_Naltahua = as.numeric(San_Antonio_de_Naltahua),
                       San_Pedro_de_Melipilla = as.numeric(San_Pedro_de_Melipilla),
                       Carmen_Rosa = as.numeric(Carmen_Rosa))

GGD_total$Los_Tilos <- NULL
GGD_total$San_Antonio_de_Naltahua <- NULL
GGD_total$San_Pedro_de_Melipilla <- NULL

GGD_2015 <- GGD_total[GGD_total$Time_UTC_Chile >= "15-01-01" & 
                                GGD_total$Time_UTC_Chile <= "15-12-31",]

GGD_2016 <- GGD_total[GGD_total$Time_UTC_Chile >= "16-01-01" & 
                                GGD_total$Time_UTC_Chile <= "16-12-31",]

GGD_2017 <- GGD_total[GGD_total$Time_UTC_Chile >= "17-01-01" & 
                                GGD_total$Time_UTC_Chile <= "17-12-31",]

GGD_2018 <- GGD_total[GGD_total$Time_UTC_Chile >= "18-01-01" & 
                                GGD_total$Time_UTC_Chile <= "18-12-31",]

GGD_2019 <- GGD_total[GGD_total$Time_UTC_Chile >= "19-01-01" & 
                                GGD_total$Time_UTC_Chile <= "19-12-31",]

GGD_2015$Year <- 2015
GGD_2016$Year <- 2016
GGD_2017$Year <- 2017
GGD_2018$Year <- 2018
GGD_2019$Year <- 2019

GGD_2015$Time_UTC_Chile <- as.numeric(format(as.Date(GGD_2015$Time_UTC_Chile), "%j"))
GGD_2016$Time_UTC_Chile <- as.numeric(format(as.Date(GGD_2016$Time_UTC_Chile), "%j"))
GGD_2017$Time_UTC_Chile <- as.numeric(format(as.Date(GGD_2017$Time_UTC_Chile), "%j"))
GGD_2018$Time_UTC_Chile <- as.numeric(format(as.Date(GGD_2018$Time_UTC_Chile), "%j"))
GGD_2019$Time_UTC_Chile <- as.numeric(format(as.Date(GGD_2019$Time_UTC_Chile), "%j"))

#GGD_2015$Month <- month(GGD_2015$Time_UTC_Chile, label = TRUE)
#GGD_2016$Month <- month(GGD_2016$Time_UTC_Chile, label = TRUE)
#GGD_2017$Month <- month(GGD_2017$Time_UTC_Chile, label = TRUE)
#GGD_2018$Month <- month(GGD_2018$Time_UTC_Chile, label = TRUE)
#GGD_2019$Month <- month(GGD_2019$Time_UTC_Chile, label = TRUE)

GGD_total <- rbind(GGD_2015, GGD_2016, GGD_2017, GGD_2018, GGD_2019)

GGD_total$Year <- as.factor(GGD_total$Year)

GGD_total %>%
  mutate(month = format(Time_UTC_Chile, "%m"), year = format(Time_UTC_Chile, "%Y")) %>%
  group_by(month, year) %>%
  summarise(total = sum(Carmen_Rosa))

par(mfrow=c(2,3))
plot(GGD_2015$Time_UTC_Chile, GGD_2015$Carmen_Rosa)
plot(GGD_2016$Time_UTC_Chile, GGD_2016$Carmen_Rosa)
plot(GGD_2017$Time_UTC_Chile, GGD_2017$Carmen_Rosa)
plot(GGD_2018$Time_UTC_Chile, GGD_2018$Carmen_Rosa)
plot(GGD_2019$Time_UTC_Chile, GGD_2019$Carmen_Rosa)
plot(GGD_total$Time_UTC_Chile, GGD_total$Carmen_Rosa)
title(main="main title", sub="sub-title",
      xlab="x-axis label", ylab="y-axis label")

################################################################################
# line in 274 doy represent Oct. 1st
ggplot(data = GGD_total) + 
  geom_smooth(aes(x = Time_UTC_Chile, y = Carmen_Rosa, color = Year), size = 1, 
              method = "loess", formula = y ~ x, se = FALSE) +
  geom_vline(xintercept = 274) + 
#  geom_hline(yintercept = 10) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  scale_x_continuous(breaks = seq(from = 0, to = 366, by = 10)) +
#  xlim(0, 366) +
  labs(x = "Day of the year", y = "Growing degree-day (°C)") +
  ggtitle("Phenological analysis of the Carmen Rosa farm")

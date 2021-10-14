# DOY images and Satellites
library(ggplot2)
library(lattice)
library(lubridate)

Sentinel <- read.csv('/home/diego/GITHUP_REPO/Master_Thesis_EAGLE/Original_data/DOY_Sentinel_table.csv')
Sentinel$Date <- as.Date(Sentinel$Date)
Sentinel$doy <- yday(Sentinel$Date)
Sentinel$Year <- format(Sentinel$Date, format = "%Y")

# Colors ggplot
#colors <- c("Sentinel-1 GRD" = "#d66f99", "Sentinel-2A" = "#6777c9", "Sentinel-2B" = "#48ab66")
colors <- c("2017" = "#d66f99", "2018" = "#6777c9", "2019" = "#48ab66")

# Plot DOY of images used
ggplot(Sentinel, aes(x = doy, y = Satellite, shape = Year, color = Year)) +
  geom_point(size = 5) + scale_colour_manual(values = colors) +
  labs(x = "DOY", y = "Satellite ") + 
  scale_x_continuous(breaks = seq(0, 365, by = 5)) +
  theme(axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black")) +
  theme_classic() #+ guides(shape = "none")

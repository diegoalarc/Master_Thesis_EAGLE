# ODEPA data Farm information in Chile
# https://www.odepa.gob.cl/wp-content/uploads/2019/09/panorama2019Final.pdf
# page 27
library(dplyr)
library(gtable)
library(gridExtra)
library(grid)
library(ggplot2)

Farm_he <- c("Landless", "0.1 to 4.9", "5 to 9.9", "10 to 19.9","20 to 49.9", 
             "50 to 99.9", "100 to 499,9", "500 to 999.9", "1000 and more")
N_farms <- c(1824, 125334, 48711, 45338, 40275, 16972, 16741, 2722, 3459)
sum(N_farms)

perc_total <- c(N_farms*100/sum(N_farms))

cum_perc_total <- c(perc_total[1], sum(perc_total[1:2]), sum(perc_total[1:3]),
                    sum(perc_total[1:4]), sum(perc_total[1:5]), sum(perc_total[1:6]),
                    sum(perc_total[1:7]), sum(perc_total[1:8]), sum(perc_total[1:9]))

# Creation of the final dataframe for
#"NUMBER OF AGRICULTURAL, LIVESTOCK AND FORESTRY FARMS BY PROPERTY SIZE RANGE"
# Information ODEPA-INE 2007.
N_farms_size_range <- data.frame(cbind(Farm_he, 
                                       N_farms, 
                                       perc_total = as.numeric(format(round(perc_total, 1), nsmall = 2)), 
                                       cum_perc_total = as.numeric(format(round(cum_perc_total, 1), nsmall = 2))))
# Transform as.numeric columns 2:3
N_farms_size_range <- transform(N_farms_size_range, 
                                N_farms = as.numeric(N_farms),
                                perc_total = as.numeric(perc_total),
                                cum_perc_total = as.numeric(cum_perc_total))

# Column names in the data frame
colnames(N_farms_size_range) <- c("Farm size categories (ha)","Number of farms", 
                                  "% total", "Accumulated %")

write.csv(N_farms_size_range,'/home/diego/Downloads/N_farms_size_range.csv')
#grid.table(N_farms_size_range)

g <- tableGrob(N_farms_size_range, rows = NULL)
g <- gtable_add_grob(g,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t = 2, b = nrow(g), l = 1, r = ncol(g))
g <- gtable_add_grob(g,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t = 1, l = 1, r = ncol(g))
grid.draw(g)

# save the plot using the botton in the plot seccion
# namely AGRICULTURAL_SIZE_RANGE_2007

################################################################################
# page 29

Region_n <- c('XV', 'I', 'II', 'III', 'IV', 'V', 'RM', 'VI', 'VII', 'XVI | VII', 
              'IX', 'XIV', 'X', 'XI', 'XII')

Region <- c("Arica y Parinacota", "Tarapacá", "Antofagasta", "Atacama", "Coquimbo", 
            "Valparaíso", "Metropolitana", "O’Higgins", "Maule", "Ñuble | Biobío",
            "La Araucanía", "Los Ríos", "Los Lagos", "Aysén", "Magallanes")
N_farms_reg <- c(2497, 1979, 2000, 2925, 15777, 17734, 12805, 25249, 41904, 62797,
                 58069, 16529, 35717, 4002, 1392)
Agro_area <- c(175111, 501476, 668335, 109273, 3259519, 506860, 329631, 775708,
               1752936, 1948737, 1783300, 697124, 961795, 775799, 4197632)

# Creation of the final dataframe for
# "NUMBER OF FARMS AND AGRICULTURAL, LIVESTOCK AND FORESTRY AREA BY REGION"
# Information ODEPA-INE 2007.
N_farms_area <- data.frame(cbind(Region_n, Region, N_farms_reg, Agro_area))
# Transform as.numeric columns 2:3
N_farms_area <- transform(N_farms_area, N_farms_reg = as.numeric(N_farms_reg),
                          Agro_area = as.numeric(Agro_area))
# Column names in the data frame
colnames(N_farms_area) <- c("Region Number", "Region", "Number of farms",
                            "Agricultural, livestock and forestry area (ha)")


sum(N_farms_area[5:13,3])*100/sum(N_farms_area[,3])

write.csv(N_farms_area,'/home/diego/Downloads/N_farms_area.csv')

g1 <- tableGrob(N_farms_area, rows = NULL)

separators <- replicate(ncol(g1) - 2,
                        segmentsGrob(x1 = unit(0, "npc"), gp=gpar(lty=2)),
                        simplify=FALSE)

g1 <- gtable_add_grob(g1,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t = 2, b = nrow(g1), l = 1, r = ncol(g1))
g1 <- gtable_add_grob(g1, 
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t = 1, l = 1, r = ncol(g1))

grid.draw(g1)

# save the plot using the botton in the plot seccion
# namely AGRICULTURAL_AREA_BY_REGION_2007

################################################################################
# page 79

fruit_crops <- c("Almonds", "Blueberries", "Hazelnut", "Cherries", "Prunus salicina", 
                 "Prunus domestica", "Apricots", "Fresh peaches", "Peaches for tinning", "Raspberry", 
                 "Kiwis", "Lemons", " Red apples", "Green apples", "Mandarins", "Oranges", 
                 "Nectarines", "Walnuts", "Olives", "Avocados", "Pears", "Table grapes", "Other fruits")

AREA_2008 <- c(6192, 5953, 3439, 10054, 8061, 6575, 1906, 5257, 8257, 4692, 8670, 
               7935, 27725, 7237, 2826, 8868, 6621, 11128, 8596, 33837, 6061, 52185, 7470)

AREA_2018 <- c(8863, 15707, 13103, 30179, 12932, 4800, 665, 2108, 8327, 3204, 8679, 
               6516, 28260, 6167, 7725, 6263, 5320, 36819, 22210, 29166, 8217, 47800, 8560)


# Creation of the final dataframe for
# PLANTED AREA WITH FRUIT CROPS (HECTARES)"
Planted_area <- data.frame(cbind(fruit_crops, AREA_2008, AREA_2018))
# Transform as.numeric columns 2:3
Planted_area <- transform(Planted_area, AREA_2008 = as.numeric(AREA_2008),
                          AREA_2018 = as.numeric(AREA_2018))
# Column names in the data frame
colnames(Planted_area) <- c("Fruit Crops", 2008, 2018)

library(reshape2)

df.long<-melt(Planted_area)
colnames(df.long) <- c("Fruit Crops", "Years", "value")

area_fruit <- ggplot(df.long,aes(`Fruit Crops`,value,fill=Years)) +
  geom_bar(stat="identity",position="dodge") +
  labs(y = "Planted Area (he)", x = "Fruit Crops", caption = "Source: ODEPA (2019)") +
  ggtitle("Planted area with Fruit Crops (He)") + 
  theme(plot.title = element_text(hjust = 0.5))

plot(area_fruit)

#png("Planted_area_with_Fruit_Crops_2019.png")
ggsave("Planted_area_with_Fruit_Crops_2019.png", width = 60, height = 20, units = "cm")
print(area_fruit)
dev.off()

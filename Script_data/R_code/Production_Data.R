# Production Data

library(dplyr)
library(gtable)
library(gridExtra)
library(grid)
library(ggplot2)

Prod_data <- read.csv('/home/diego/GITHUP_REPO/Master_Thesis_EAGLE/Original_data/Productive_summary.csv')

grid.table(Prod_data)

g <- tableGrob(Prod_data, rows = NULL)
g <- gtable_add_grob(g,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t = 2, b = nrow(g), l = 1, r = ncol(g))
g <- gtable_add_grob(g,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t = 1, l = 1, r = ncol(g))
grid.draw(g)

# save the plot using the botton in the plot seccion
# namely AGRICULTURAL_SIZE_RANGE_2007
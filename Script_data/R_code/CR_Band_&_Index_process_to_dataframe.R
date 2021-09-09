# Libraries loading
library(rgdal)
library(raster)
library(cluster)
library(tidyverse)
library(RStoolbox)
library(data.table)
library(exactextractr)

################################################################################

# Set the folder location
setwd('/home/diego/GITHUP_REPO/Master_Thesis_EAGLE')

# Read Shapefile with the roi
roi <- readOGR(dsn=file.path('./ROI/Carmen_Rosa_Field.shp'))
#proj4string(roi) <- crs("+init=epsg:32719")
roi <- spTransform(roi, CRS('+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs '))
#crs(roi)
#plot(roi)

# Name of the farm
farm_name <- 'Carmen_Rosa_Field'

# Stat
stat_type <- 'median'
#stat_type <- c('max', 'mean', 'median', 'min', 'stdev', 'sum')

# Activation of the cores in the device and focus these in the following process
beginCluster()

# Start counting time
start_time <- Sys.time()

for (i in 1:length(stat_type)){
  
  type_now <- stat_type[i]
  
  # Change path to folder containing rasters
  rasdir <- file.path(paste0('./Satellite_data/Carmen_Rosa/'))
  
  print(paste0('Working with: ', type_now))
  
  # Activation of the cores in the device and focus these in the following process
  #beginCluster()
  
  year <- '2018'
  year2 <- '2019'
  year3 <- '2020'
  
  # List all GeoTIFF files in folder, change extension in pattern if different format
  fllst <- list.files(path = rasdir,
                      full.names = TRUE,
                      pattern = paste0(type_now, '_', year,'.tif$'))
  
  fllst_stack <- stack(fllst)
  
  fllst2 <- list.files(path = rasdir,
                       full.names = TRUE,
                       pattern = paste0(type_now, '_', year2,'.tif$'))
  
  fllst_stack2 <- stack(fllst2)
  
  fllst3 <- list.files(path = rasdir,
                       full.names = TRUE,
                       pattern = paste0(type_now, '_', year3,'.tif$'))
  
  fllst_stack3 <- stack(fllst3)
  
  # Create a list
  df_band <- list()
  
  print(paste0('ROI total numbers: ', length(roi)))
  
  for (x in 1:length(roi)){
    # Subset the shapefile by Number
    roi_1 <- subset(roi, Number == x)
    #plot(roi_1)
    print(paste0('ROI: ', roi_1$Name, ', Number: ', x))
    
    ##############################################################################
    # Combine Values Vector
    bands_ins <- c('B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B8A', 
                   'B9', 'B10', 'B11', 'B12', 'EVI', 'GNDVI', 'NDWI', 'NDVI', 
                   'VH', 'VV')
    
    # Columns names
    columns <- c('bands', year, year2, year3)
    
    # Create the data frame with the data by Band
    my_df <- data.frame(matrix(nrow = length(bands_ins), ncol = length(columns))) 
    
    # Assign column names
    colnames(my_df) <- columns
    
    # Add values to the column bands
    my_df$bands <- bands_ins
    
    for (i in 1:nlayers(fllst_stack)){
      #print(paste0('extract: ', names(fllst_stack[[i]])))
      
      my_df[i,2] <- as.numeric(exact_extract(fllst_stack[[i]], roi_1, 'mean', 
                                             progress = TRUE))
      
      #print(paste0('extract: ', names(fllst_stack2[[i]])))
      
      my_df[i,3] <- as.numeric(exact_extract(fllst_stack2[[i]], roi_1, 'mean', 
                                             progress = TRUE))
      
      #print(paste0('extract: ', names(fllst_stack3[[i]])))
      
      my_df[i,4] <- as.numeric(exact_extract(fllst_stack3[[i]], roi_1, 'mean', 
                                             progress = TRUE))
    }
    #print(paste0('Ready with the roi: ', roi_1$Name))
    
    # Complete the dataframe process
    my_df_or <- transpose(my_df)
    colnames(my_df_or) <- rownames(my_df)
    rownames(my_df_or) <- colnames(my_df)
    my_df_or <- my_df_or[-1,]
    my_df_or[,20] <- c(year, year2, year3)
    my_df_or[,21] <- c(roi_1$Name, roi_1$Name, roi_1$Name)

    names(my_df_or) <- c(paste0('B1_', type_now), paste0('B2_', type_now), paste0('B3_', type_now),
                         paste0('B4_', type_now), paste0('B5_', type_now), paste0('B6_', type_now),
                         paste0('B7_', type_now), paste0('B8_', type_now), paste0('B8A_', type_now), 
                         paste0('B9_', type_now), paste0('B10_', type_now), paste0('B11_', type_now),
                         paste0('B12_', type_now), paste0('EVI_', type_now), paste0('GNDVI_', type_now),
                         paste0('NDWI_', type_now), paste0('NDVI_', type_now), paste0('VH_', type_now),
                         paste0('VV_', type_now), 'Year', 'Field')
    
    # Create a List using the iterator
    df_band[[x]] <- my_df_or
  }
  
  # Execute a Function rbind
  band_big_data <- do.call(rbind, df_band)
  
  # Save dataframe as .CSV
  write.csv(band_big_data, 
            paste0('./Original_data/Values_bands/', 
                   farm_name, '_Band_', type_now, '_2017_to_2019.csv'), 
            row.names = F, quote = F)
  
  # Print the document
  print(paste0('Document created: ', farm_name,'_Band_', type_now, '_2017_to_2019.csv'))
}

end_time <- Sys.time()
t_process <- end_time - start_time

# Print the process is ready
print(paste0('The process is ready and took: ', t_process))

# Disabling the cores on the device when the process ends
endCluster()
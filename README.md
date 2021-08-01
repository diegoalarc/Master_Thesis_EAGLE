# Master Thesis EAGLE
Repository for master's thesis Diego Alarcón

In this repository you can find the folders:

[Agenda](https://github.com/diegoalarc/Master_Thesis_EAGLE/tree/main/Agenda): Here you can find a basic plan of how the weeks to complete the master's thesis are organized.

[Original data](https://github.com/diegoalarc/Master_Thesis_EAGLE/tree/main/Original_data): Here you will find the information preprocessed in .csv tables

[Plots](https://github.com/diegoalarc/Master_Thesis_EAGLE/tree/main/Plots): Here you can see the plots made to complete the information of the master's thesis (It is under construction).

[ROI](https://github.com/diegoalarc/Master_Thesis_EAGLE/tree/main/ROI): Here you can find the files in Shapefile format for the Carmen Rosa and Loreto farms.

[Satellite_data](https://github.com/diegoalarc/Master_Thesis_EAGLE/tree/main/Satellite_data): Here you can find the information from the Sentinel 1 and 2 satellites pre-processed for use on both farms.

### Field data preprocessing
Before starting with the Machine Learning processes and others, it is important to have an idea of what data we have and see if they are complete in order to start the process of analysis of variables.

This is why when analyzing the field data obtained, which in our case are data from two weather stations for the different fields, it is necessary to carry out a data imputation process, since it was observed in the original data that there are days without information due to failures in the continuous data collection by the weather stations.

This is why we have made these two codes in R to be able to reliably complete the missing data.

The codes are:

[Carmen Rosa imputing missing weather station data](https://github.com/diegoalarc/Master_Thesis_EAGLE/blob/main/CR_Imputing_Missing_weatherstation_data.R)

[Loreto imputing missing weather station data](https://github.com/diegoalarc/Master_Thesis_EAGLE/blob/main/Qui_Imputing_Missing_weatherstation_data.R)

### Satellite data
To carry out this thesis, satellite data from the Sentinel-1 (S1) and Sentinel-2 (S2) satellites were used. In both, the temporal space of the months between October until the end of December, in the years 2017 to 2019, was used. These dates correspond to the phenological stages that we care about to carry out our machine learning process.

The satellite images were obtained through the use of Google Earth Engine, and the scripts were made with the help of Dr. Tobias Ullman.

The codes are:

[Sentinel-1](https://code.earthengine.google.com/eaa2d79f440964846921aac3b567c164)

[Sentinel-2](https://code.earthengine.google.com/2e547a69163a7a0cb9d84a183a229fae)

After obtaining the images, by means of a code created in R, we were able to obtain the average value of each field (by variety of table grape) in each farm.

The codes to carry out this process are:

[Carmen Rosa process of obtaining Band & Index values to a dataframe](https://github.com/diegoalarc/Master_Thesis_EAGLE/blob/main/CR_Band_%26_Index_process_to_dataframe.R)

[Loreto process of obtaining Band & Index values to a dataframe](https://github.com/diegoalarc/Master_Thesis_EAGLE/blob/main/Qui_Band_%26_Index_process_to_dataframe.R)

### Machine learning process
In this section we begin with the Machine Learning (ML) process, in which an analysis of each information obtained was carried out and models were generated using 3 main combinations:

- S1 + Field Data
- S2 + Field data
- S1 & S2 + Field data

The codes for the machine learning process were generated through R and mainly using the package called Caret.

The codes can be observed in the following links:

[S1 + Field Data](https://github.com/diegoalarc/Master_Thesis_EAGLE/blob/main/Caret_ML_S1.R)
[S2 + Field data](https://github.com/diegoalarc/Master_Thesis_EAGLE/blob/main/Caret_ML_S2.R)
[S1 & S2 + Field data](https://github.com/diegoalarc/Master_Thesis_EAGLE/blob/main/Caret_ML_S1_%26_S2.R)

After running the codes in the same section of R, to better observe its results, it is recommended to use the following code:

[Final Results](https://github.com/diegoalarc/Master_Thesis_EAGLE/blob/main/Final_result.R)

###
It is important to mention that for Copyright reasons, it has been indicated that the Quimetal Farm, be renamed Loreto, since Quimetal is a registered trademark.
# Master Thesis EAGLE
Repository for master's thesis Diego Alarcón

In this repository you can find the folders:

[Agenda](https://github.com/diegoalarc/Master_Thesis_EAGLE/tree/main/Agenda): Here you can find a basic plan of how the weeks to complete the master's thesis are organized.

[Original data](https://github.com/diegoalarc/Master_Thesis_EAGLE/tree/main/Original_data): Here you will find the information preprocessed in .csv tables

[Plots](https://github.com/diegoalarc/Master_Thesis_EAGLE/tree/main/Plots): Here you can see the plots made to complete the information of the master's thesis (It is under construction).

[ROI](https://github.com/diegoalarc/Master_Thesis_EAGLE/tree/main/ROI): Here you can find the files in Shapefile format for the Carmen Rosa and Loreto farms.

[Satellite_data](https://github.com/diegoalarc/Master_Thesis_EAGLE/tree/main/Satellite_data): Here you can find the information from the Sentinel 1 and 2 satellites pre-processed for use on both farms.

[Script_data](https://github.com/diegoalarc/Master_Thesis_EAGLE/tree/main/Script_data): Here you can find two folders with the codes created for this master thesis project. The codes are in Python and R languages.

### Field data preprocessing
Before starting with the Machine Learning processes and others, it is important to have an idea of what data we have and see if they are complete in order to start the process of analysis of variables.

This is why when analyzing the field data obtained, which in our case are data from two weather stations for the different fields, it is necessary to carry out a data imputation process, since it was observed in the original data that there are days without information due to failures in the continuous data collection by the weather stations.

This is why we have made these two codes in R to be able to reliably complete the missing data.

The codes are:

[Carmen Rosa imputing missing weather station data](https://github.com/diegoalarc/Master_Thesis_EAGLE/blob/main/Script_data/R_code/CR_Imputing_Missing_weatherstation_data.R)

[Loreto imputing missing weather station data](https://github.com/diegoalarc/Master_Thesis_EAGLE/blob/main/Script_data/R_code/Qui_Imputing_Missing_weatherstation_data.R)

### Satellite data
To carry out this thesis, satellite data from the Sentinel-1 (S1) and Sentinel-2 (S2) satellites were used. In both, the temporal space of the months between October until the end of December, in the years 2017 to 2019, was used. These dates correspond to the phenological stages that we care about to carry out our machine learning process.

The satellite images for S2 were obtained using Google Earth Engine (GEE) first by means of the use of Python language to make a correction top of atmosphere (TOA) to bottom of atmosphere (BOA).

The code used, it is possible to see it here:

[Sentinel-2 atmospheric correction](https://github.com/diegoalarc/Master_Thesis_EAGLE/blob/main/Script_data/Python_code/sentinel2_atmospheric_correction.ipynb)

The satellite images for S1 were also obtained through the use of GEE, and the scripts were made with the help of Dr. Tobias Ullman.

The codes are:

[Sentinel-1](https://code.earthengine.google.com/99fa1791b727e4e8207eb552ac9269db)

[Sentinel-2](https://code.earthengine.google.com/74b5a8daf30287f8a6928d6ef7d56e7a)

After obtaining the images, by means of a code created in R, we were able to obtain the average value of each field (by variety of table grape) in each farm.

The codes to carry out this process are:

[Carmen Rosa process of obtaining Band & Index values to a dataframe](https://github.com/diegoalarc/Master_Thesis_EAGLE/blob/main/Script_data/R_code/CR_Band_%26_Index_process_to_dataframe.R)

[Loreto process of obtaining Band & Index values to a dataframe](https://github.com/diegoalarc/Master_Thesis_EAGLE/blob/main/Script_data/R_code/Qui_Band_%26_Index_process_to_dataframe.R)

### Machine learning process
In this section we begin with the Machine Learning (ML) process, in which an analysis of each information obtained was carried out and models were generated using 3 main combinations:

- S1 + Field Data
- S2 + Field data
- S1 & S2 + Field data

The codes for the machine learning process were generated through R and mainly using the package called Caret.

The codes can be observed in the following links:

[S1 + Field Data](https://github.com/diegoalarc/Master_Thesis_EAGLE/blob/main/Script_data/R_code/Caret_ML_S1.R)

[S2 + Field data](https://github.com/diegoalarc/Master_Thesis_EAGLE/blob/main/Script_data/R_code/Caret_ML_S2.R)

[S1 & S2 + Field data](https://github.com/diegoalarc/Master_Thesis_EAGLE/blob/main/Script_data/R_code/Caret_ML_S1_%26_S2.R)

After running the codes in the same session of R, to better observe its results, it is recommended to use the following code:

[Final Results](https://github.com/diegoalarc/Master_Thesis_EAGLE/blob/main/Script_data/R_code/Final_result.R)

Machine Learning results:

| Satellite | Model                 | R2   | RMSE    | MAE     |
| --------- | --------------------- | ---- | ------- | ------- |
| S1        | Random Forest         | 0.32 | 8865.11 | 8028.55 |
| S1        | CForest               | 0.45 | 7002.26 | 6389.34 |
| S1        | Gaussian Radial Basis | 0.28 | 7277.00 | 6712.89 |
| S2        | Random Forest         | 0.82 | 4348.98 | 3508.50 |
| S2        | CForest               | 0.79 | 7229.16 | 6308.70 |
| S2        | Gaussian Radial Basis | 0.31 | 7064.41 | 6395.24 |
| S1 & S2   | Random Forest         | 0.87 | 4324.09 | 3844.76 |
| S1 & S2   | CForest               | 0.68 | 6978.33 | 6094.64 |
| S1 & S2   | Gaussian Radial Basis | 0.50 | 5983.58 | 5312.87 |

The trained models were used to make a prediction of the production yield of the Loreto farm for the year 2020 and the following results were obtained:

| Satellite | Model                 | Predicted | Original | Error (Kg) | Error (%) |
| --------- | --------------------- | --------- | -------- | ---------- | --------- |
| S1        | Random Forest         | 44838.72  | 35878    | 8960.72    | 24.98     |
| S1        | CForest               | 38320.79  | 35878    | 2442.79    | 6.81      |
| S1        | Gaussian Radial Basis | 41287.56  | 35878    | 5409.56    | 15.08     |
| S2        | Random Forest         | 30940.26  | 35878    | 4937.74    | 13.76     |
| S2        | CForest               | 39262.94  | 35878    | 3384.94    | 9.43      |
| S2        | Gaussian Radial Basis | 41287.48  | 35878    | 5409.48    | 15.08     |
| S1 & S2   | Random Forest         | 34559.53  | 35878    | 1318.47    | 3.67      |
| S1 & S2   | CForest               | 38306.04  | 35878    | 2428.04    | 6.77      |
| S1 & S2   | Gaussian Radial Basis | 41287.57  | 35878    | 5409.57    | 15.08     |

Where clearly we can observe that the best and closest to reality was generated by the best performing model, this was Random Forest with the use of the Sentinel-1 and Sentinel-2 data principal.

###
It is important to mention that for Copyright reasons, it has been indicated that the Quimetal Farm, be renamed Loreto, since Quimetal is a registered trademark.
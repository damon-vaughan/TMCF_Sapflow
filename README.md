# TMCF_Sapflow
Sapflow data from TMCF epi-feedbacks project

Folders in Github:
Sapflow_data_supporting: Metadata files used in the various processing steps
Sapflow_data_L1: Processed sapflow data, not cleaned or baselined
Sapflow_data_L2: Cleaned with outliers removed
Sapflow_data_L3: Baselined

Folders (stored elsewhere; ignored by github because of large file size):
Sapflow_data_import: .dat files downloaded from field dataloggers
Sapflow_data_raw: .csv files for every tree

Scripts:
Import_and_process_sapflow_data.R: All functions to move data from raw to L2
SF_Functions.R: Functions that are called in the other scripts
Shiny_Sapflow: Shiny app to view the data

Units:
All velocity readings are in cm/hr
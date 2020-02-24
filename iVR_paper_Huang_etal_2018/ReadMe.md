This folder contains the LANDIS output and R scripts needed to convert LANDIS output to iVR.

The maps that LANDIS-II generated are:
50yHistoric.img
50y_CC.img

The R script used to generate the regression equations to do the conversions is here:
FIA_Relationships_iVR.R
The output from that R script is:
Regression_Equations.csv

The R script that uses the regression coefficients to convert the maps to the csv files for iVR is:
Convert_LANDIS_to_iVR.R

The final two output files for iVR are:
community-input-file-Historic_iVR_04.03.18.csv
community-input-file-ClimateChange_iVR_04.04.18.csv


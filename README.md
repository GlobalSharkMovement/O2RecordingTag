# A new animal-attached archival tag recording in situ dissolved oxygen, temperature, fine-scale movements and behaviour 
This repository contains codes, files and metadata used on da Costa et al., (submitted to Methods in Ecology and Evolution) where we describe, a new animal-attached (Dissolved Oxygen Measuring, DOME) 
with an optical oxygen sensor for recording DO, in addition to sensors for temperature and depth, a triaxial accelerometer for fine-scale movements and activity, and a GPS for tag recovery. 
Here, you can reproduce analysis for calculating recovery time of blue sharks, evaluate shark swimming metrics and compare in situ oxygen and temperature data recorded by the tags with extracted modelled (CMEMS)data.

## cmems
Folder that contains csv files with extracted modelled data (profiles) from CMEMS, for each tagging and pop-off date/position of each tagged shark (n = 6).
Oxygen data from the Global Ocean Biogeochemistry Analysis and Forecast product (https://doi.org/10.48670/moi-00015).
Temperature data from Global Ocean Physics Analysis and Forecast product (https://doi.org/10.48670/moi-00016).

## files
Folder that contains csv files with environmental (oxygen and temperature) and acceleration-derived swimming metrics (e.g., tailbeat frequency, pitch, odba) previously extracted from IGOR analysis, for all tagged 
sharks (n =3). 

## scripts
Folder that contains all scrits (codes) used to: calculate recovery times of tagged sharks (RecoveryTime), calculate and compare discriptive statistics of environmental and shark swimming metrics (SummaryTables)
and compare in situ vs modelled DO/temperature data (CMEMS_comparison).

#### RecoveryTime

## tables
Folder that contains generated tables from the analysis (e.g., discriptive statistics).

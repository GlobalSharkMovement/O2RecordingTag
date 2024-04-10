# A new animal-attached archival tag recording in situ dissolved oxygen
This repository contains codes, files and metadata used in da Costa et al., (submitted to Methods in Ecology and Evolution) where we describe, a new animal-attached (Dissolved Oxygen Measuring, DOME) 
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
Folder that contains all scrits (codes) used to: calculate recovery times of tagged sharks (RecoveryTime), calculate and compare descriptive statistics of environmental and shark swimming metrics (SummaryTables)
and compare in situ vs modelled DO/temperature data (CMEMS_comparison).

#### CMEMS_comparison
This subfolder contains 2 R codes used for: (1) calculating recovery time using 5 minutes means and build figure 8 (TBF vs post release time) of the paper; defining (2) overall and (3) dive phase (ascents vs descents) descriptive statistics for environmental and shark swimming metrics; (4) applying statistical tests comparisons (Mann-Whitney U test ) between recovery vs post-recovery (normal) periods. 

#### RecoveryTime
This subfolder contains 4 R codes used for: (1) calculating recovery time using 5 minutes means and build figure 8 (TBF vs post release time) of the paper; defining (2) overall and (3) dive phase (ascents vs descents) descriptive statistics for environmental and shark swimming metrics; (4) applying statistical tests comparisons (Mann-Whitney U test ) between recovery vs post-recovery (normal) periods. 

#### SummaryTables
This subfolder contains 4 R codes used for: (1) calculating recovery time using 5 minutes means and build figure 8 (TBF vs post release time) of the paper; defining (2) overall and (3) dive phase (ascents vs descents) descriptive statistics for environmental and shark swimming metrics; (4) applying statistical tests comparisons (Mann-Whitney U test ) between recovery vs post-recovery (normal) periods. 

## tables
Folder that contains tables generated during analysis (e.g., descriptive statistics).

## metadata
Folder that contains tables generated during analysis (e.g., descriptive statistics).

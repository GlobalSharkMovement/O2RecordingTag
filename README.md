# A new animal-attached archival tag recording in situ dissolved oxygen
This repository contains codes, files and metadata used in da Costa et al., (submitted to Methods in Ecology and Evolution) where we describe, a new animal-attached (Dissolved Oxygen Measuring, DOME) tag 
with an optical oxygen sensor for recording DO, in addition to sensors for temperature and depth, a triaxial accelerometer for fine-scale movements and activity, and a GPS for tag recovery. 
Here, you can reproduce analysis for calculating recovery time of blue sharks, evaluate shark swimming metrics and compare in situ oxygen and temperature data recorded by the tags with extracted modelled (CMEMS)data.

## cmems
Folder that contains csv files with extracted modelled data (profiles) from CMEMS, for each tagging and pop-off date/position of each tagged shark (n = 6).
Oxygen data (DO) from the Global Ocean Biogeochemistry Analysis and Forecast product (https://doi.org/10.48670/moi-00015).
Temperature data (thetao) from Global Ocean Physics Analysis and Forecast product (https://doi.org/10.48670/moi-00016).

## files
Folder that contains csv files with environmental (oxygen and temperature) and acceleration-derived swimming metrics (e.g., tailbeat frequency, pitch, odba) previously extracted from IGOR analysis, for all tagged 
sharks (n =3). 

## tables
Folder that contains tables generated during analysis (e.g., descriptive statistics).

## figures
Folder that contains figures generated during analysis (e.g., Modelled vs in situ DO - Figure 9).

## scripts
Folder that contains all scrits (codes) used to: calculate recovery times of tagged sharks (RecoveryTime), calculate and compare descriptive statistics of environmental and shark swimming metrics (SummaryTables)
and compare in situ vs modelled DO/temperature data (CMEMS_comparison).

- the [CMEMS_comparison](scripts/CMEMS_comparison) contains 2 R codes used for evaluating the relationship between in situ (DOME tags) vs modelled (CMEMS) dissolved oxygen [cmems_comparison_o2.R](scripts/CMEMS_comparison/cmems_comparison_o2.R) and temperature [cmems_comparison_tc.R](scripts/CMEMS_comparison/cmems_comparison_tc.R) values. The codes calculate mean differences and relative errors between in situ vs modelled profiles and it creates 3 figures that together constitute figure 9 of the paper.

- RecoveryTime
This subfolder contains four R scripts:
  - [1.Calculate_RecoveryTime.R](RecoveryTime/1.Calculate_RecoveryTime.R): calculating recovery time using 5 minutes means and building figure 8 (TBF vs post-release time) of the paper;
  - [2.Calculate_DescriptiveStat_Overall_RecoveryTime.R](RecoveryTime/2.Calculate_DescriptiveStat_Overall_RecoveryTime.R): defining overall descriptive statistics for environmental and shark swimming metrics;
  - [3.Calculate_DescriptiveStat_DivePhase_RecoveryTime.R](RecoveryTime/3.Calculate_DescriptiveStat_DivePhase_RecoveryTime.R) defining dive phase (ascents vs descents) descriptive statistics;
  - [4.StatisticalTests_RecoveryTime](RecoveryTime/3.Calculate_DescriptiveStat_DivePhase_RecoveryTime.R): applying statistical comparisons (Mann-Whitney U test ) between recovery vs post-recovery (normal) periods. 

- SummaryTables
This subfolder contains 5 R codes used for: (1) calculating recovery time

## metadata file
Table containing all information regarding the attachment of the DOME archival tags (e.g., tagging date) including shark measurements.

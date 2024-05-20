# A new animal-attached archival tag recording in situ dissolved oxygen
[![DOI](https://zenodo.org/badge/722950610.svg)](https://zenodo.org/doi/10.5281/zenodo.11222137)
This repository contains codes, files and metadata used in da Costa et al., manuscript (published in Methods in Ecology and Evolution) where we describe, a new animal-attached (Dissolved Oxygen Measuring, DOME) tag with an optical oxygen sensor for recording DO, in addition to sensors for temperature and depth, a triaxial accelerometer for fine-scale movements and activity, and a GPS for tag recovery. 
Here, you can reproduce analysis for calculating the post-release transition period of blue sharks, evaluate shark swimming metrics and compare in situ oxygen and temperature data recorded by the tags with extracted modeled (CMEMS)data.

## cmems
This folder  contains csv files with extracted modeled data (profiles) from CMEMS, for each tagging and pop-off date/position of each tagged shark (n = 6).
Oxygen data (DO) from the Global Ocean Biogeochemistry Analysis and Forecast product (https://doi.org/10.48670/moi-00015).
Temperature data (thetao) from Global Ocean Physics Analysis and Forecast product (https://doi.org/10.48670/moi-00016).

## files
Folder that contains csv files with environmental (oxygen and temperature) and acceleration-derived swimming metrics (e.g., tailbeat frequency, pitch, ODBA) at 1Hz, previously extracted from IGOR analysis, for all tagged 
sharks (n =3). 

## tables
This folder contains tables with the descriptive statistics of environmental and swimming metrics recorded for blue sharks, generated during analysis.

## figures
Folder that contains figures generated during analysis (e.g., Modelled vs in situ DO - Figure 9).

## scripts
Folder that contains all scripts (codes) used to: calculate post-release transition times of tagged sharks (RecoveryTime), calculate and compare descriptive statistics of environmental and shark swimming metrics (SummaryTables)
and compare in situ vs modeled DO/temperature data (CMEMS_comparison).

- the subfolder [CMEMS_comparison](scripts/CMEMS_comparison) contains 2 R codes used for evaluating the relationship between in situ (DOME tags) vs modeled (CMEMS) dissolved oxygen [cmems_comparison_o2.R](scripts/CMEMS_comparison/cmems_comparison_o2.R) and temperature [cmems_comparison_tc.R](scripts/CMEMS_comparison/cmems_comparison_tc.R) values. The codes calculate mean differences and relative errors between in situ vs modeled profiles and it creates 3 figures that together constitute figure 9 of the paper.

- the subfolder [TransitionTime](scripts/TransitionTime) contains four R scripts:
  - [1.Calculate_TransitionTime.R](scripts/TransitionTime/1.Calculate_TransitionTime.R): defines post-release transition (recovery) time using 5 minutes means and building figure 8 (TBF vs post-release time) of the paper;
  - [2.Calculate_DescriptiveStat_Overall_TransitionTime.R](scripts/TransitionTime/2.Calculate_DescriptiveStat_Overall_TransitionTime.R): calculates overall descriptive statistics for environmental and shark swimming metrics for both pre and post-transition periods;
  - [3.Calculate_DescriptiveStat_DivePhase_TransitionTime.R](scripts/TransitionTime/3.Calculate_DescriptiveStat_DivePhase_TransitionTime.R) calculates dive phase (ascents vs descents) descriptive statistics for both pre and post-transition periods;
  - [4.StatisticalTests_TransitionTime.R](scripts/TransitionTime/3.Calculate_DescriptiveStat_DivePhase_TransitionTime.R): applying statistical comparisons (Mann-Whitney U test ) between pre-transition (recovery) vs post-transition (normal) periods. 

- the subfolder [SummaryTables](scripts/SummaryTables) contains five R scripts:
  - [1.Define_Depth_Threshold.R](scripts/SummaryTables/1.Define_Depth_Threshold.R): define depth threshold that defines top (high DO) region;
  - [2.Calculate_DescritiveStats_Overall.R](scripts/SummaryTables/2.Calculate_DescritiveStats_Overall.R): calculates overall descriptive statistics for environmental and shark swimming metrics;
  - [3.Calculate_DescritiveStats_DivePhase.R](scripts/SummaryTables/3.Calculate_DescritiveStats_DivePhase.R): calculates dive phase (ascents vs descents) descriptive statistics;
  - [4.Calculate_DescritiveStats_Top_vs_Below85m.R](scripts/SummaryTables/4.Calculate_DescritiveStats_Top_vs_Below85m.R): calculates descriptive statistics on top (high DO region) and below 85 meters;
  - [5.StatisticalTests_Phase&Profile.R](scripts/SummaryTables/5.StatisticalTests_Phase&Profile.R): statistical comparisons (Mann-Whitney U test ) between dive phase (ascents vs descents) and profile depth (top vs below 85 meters). 

## metadata file
Table containing all information regarding the attachment of the DOME archival tags, including shark measurements.

## rationale
Climate-driven deoxygenation poses a significant threat to marine ecosystems, potentially altering predator-prey dynamics (particularly for air-breathing mammals) and impacting the physiology, distribution, and abundance of obligate water-breathing predators, such as commercially and ecologically important fish species (sharks, tunas). For instance, shoaling of hypoxic layers can limit available habitat, concentrating these fish in surface waters where they are more vulnerable to fisheries. However, the specific effects of deoxygenation on animal behavior remain poorly understood, with most studies relying on modeled oxygen concentrations. This study addresses this knowledge gap by introducing a novel animal-attached tag – the Dissolved Oxygen Measuring (DOME) tag. This innovative technology integrates sensors for in situ dissolved oxygen (using an optical sensor), temperature, depth, 3D body movements, and activity levels onto a single circuit board, enabling the direct measurement of oxygen tolerance in free-ranging oceanic fauna. This capability is crucial for determining the combined effects of climate-driven deoxygenation and warming on marine predators.

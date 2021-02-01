# NIMG_Pares-Pujolras_etal_2021

This repository contains data and analysis code for the following paper: Parés-Pujolràs E., Travers, E., Ahmetoglu, Y., Haggard, P. (2021) Evidence accumulation under uncertainty - a neural marker of choice and urgency. NeuroImage 

The scripts and data files included in the repository enable the replication of the data analysis and figures reported in the paper.

The folder DATA contains several data files in various formats that allow the replication of all the results and figures reported in the paper. 

      • NIMG_EPP_2021_behaviouralData.csv - contains summary data for each trial for behavioural analysis
  
      • NIMG_EPP_2021_shortEEGData.csv - contains summary EEG measures for statistical analysis
    
      • NIMG_EPP_2021_longEEGData.csv - contains full EEG single-trials, single-sample data to allow ERP plotting

The folder SCRIPTS contains two main scripts:

      • NIMG_EPP_2021_Analysis.r - this is a master script that reproduces and saves a local copy of the main statistics and figures of the main manuscript. It requires several R packages and custom functions in the SCRIPTS folder of this repository. 
  
      • NIMG_EPP_2021_Modelling.r - this script contains the models used to fit the computational models in the paper. It relies on functions from the Evidently package, which you can find at https://github.com/EoinTravers/Evidently.
 
  
  


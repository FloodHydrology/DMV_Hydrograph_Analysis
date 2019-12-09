#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Initial Analysis of Water Level Data
#Coder: C. Nathan Jones (cnjones7@ua.edu)
#Date: 12/4/2019  
#Purpose: Examine drivers of water level data using CART analysis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup workspace------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear memory
rm(list=ls(all=TRUE))

#Defind relevant working directories
data_dir<-"C://choptank/"
output_dir<-"C://choptank/output/"

#Load relevant packages
library(tidyverse)
library(mvpart)

#download data
hydro<-read_csv(paste0(data_dir,"output/hydro_metrics.csv"))
geo<-read_csv(paste0(data_dir, "spatial_data.csv"))
df<-left_join(hydro, geo %>% rename(WetID = Wetland))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Hydrologic Regime Plots----------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Multivariate model
m<-lm(recession_rate~wetland_invert + watershed_area_m2 + wetland_hsc_cm + mean_elevation_m, data=df)

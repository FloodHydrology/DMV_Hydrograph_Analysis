#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Distributions
#Coder: C. Nathan Jones (cnjones7@ua.edu)
#Date: 11/5/2019  
#Purpose: Plot distributions of spatial data
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
library('scales')
library('readxl')
library('lubridate')
library('segmented')
library('tidyverse')
library('vegan')

#download data
geo_sites<-read_csv(paste0(data_dir, "spatial_data.csv"))
geo_all<-read_csv(paste0(data_dir, 'spatial_data_all.csv'))

#Clean up data a bit
geo_sites<-geo_sites %>% mutate(cat='sites')
geo_all<-geo_all %>% mutate(cat='all') %>% rename(wetland_invert = z) %>% 
  filter(perimeter_m<2000) %>% 
  filter(watershed_area_m2<1e6) %>% 
  filter(volume_m3<5000) %>% 
  filter(wetland_hsc_cm<30) %>% 
  filter(watershed_hsc_cm>2) 
geo<-bind_rows(geo_sites, geo_all)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Plot-----------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
g<-geo %>%
  select(-c('flow_to', 'merge_to', 'merged_hsc_cm','root_giw','spawned', 'WetID')) %>% 
  #Pivot to long format
  pivot_longer(-c('Wetland','cat'), 
               names_to='metric', 
               values_to='val') %>% 
  #Start ggplot object
  ggplot(aes(val)) +
  #add density plot
  geom_density(aes(fill= cat), alpha=0.6) + 
  #Facet object
  facet_wrap(.~metric, scales='free') +
  #add black/white them
  theme_bw() 

#start plotting device
png(paste0(output_dir,'distributions/hydrogeomorphic_characteristics.png'), width=13, height=7, units = 'in', res=300)
print(g)
dev.off()

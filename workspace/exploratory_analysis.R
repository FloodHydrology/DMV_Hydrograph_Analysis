#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Initial Analysis of Water Level Data
#Coder: C. Nathan Jones (cnjones7@ua.edu)
#Date: 11/2/2019  
#Purpose: Explore Water Level Data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup workspace------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear memory
rm(list=ls(all=TRUE))

#Defind relevant working directories
data_dir<-"C://choptank/"

#Load relevant packages
library('readxl')
library('lubridate')
library('segmented')
library('tidyverse')

#download data
df<-read_xlsx(paste0(data_dir, 'Choptank_Wetlands_WY2019.xlsx'), sheet = 'SWL',  na='NA')

#Clean up data a bit
df<-df %>% 
  #simplify collumn headers
  rename_if(str_detect(colnames(df),"Wetland"), substr,2,3) %>% 
  #Isolate day
  mutate(day = date(Timestamp)) %>% select(-Timestamp) %>% 
  #Group all datat by day
  group_by(day) %>% summarise_all(., mean, na.rm=T) %>% 
  #Subset to 2018
  filter(year(day)==2018)

#Missing Data (Chase down later)
#BB Spring 2018
df<-df %>% select(-'BB')
#mising all data
df<-df %>% select(-'DV')
df<-df %>% select(-'GB')
df<-df %>% select(-'JU')
df<-df %>% select(-'QB')
#There appears to be an issue with correction to JA
df<-df %>% select(-'JA')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Exame annual metrics-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 Create function------------------------------------------------------------
fun<-function(n){
  
  #Isolate time series
  ts<-df[,c(1,n)] 
  colnames(ts)<-c("time", "waterLevel")
  
  #Recession Analysis~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Isolate recession dayes
  temp<-ts %>% mutate(dwL = waterLevel - lag(waterLevel)) %>% filter(dwL<0) %>% filter(waterLevel>0)
  
  #Create Segmented Model
  lin_mod <- lm(dwL~waterLevel,data=temp)
  segmented_mod <- segmented(lin_mod, seg.Z = ~waterLevel, psi=quantile(temp$waterLevel, 0.75))
  
  #Estimate spill threshold
  wL_spill<-segmented_mod$psi[2]
  
  #Esitmate recession rate
  recession_rate<-segmented_mod$coefficients[2]
  
  #Create some rules to make sure segmented reggression makes sense
  if((segmented_mod$coefficients[2]+segmented_mod$coefficients[3])>0){
    wL_spill<-max(temp$waterLevel)
    recession_rate<-lin_mod$coefficients[2]
  }
  
  #Estimate water level metrics~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Characterize water level
  wL_max<-max(ts$waterLevel, na.rm=T)
  wL_mean<-mean(ts$waterLevel, na.rm=T)
  wL_mean_norm<- wL_mean/wL_spill
  wL_var<-var(ts$waterLevel, na.rm = T)
  wL_var_norm<-wL_var/wL_spill
  
  #Characterize duration of different components
  dur_inun<-ts %>% filter(waterLevel>(0.05*wL_spill)) %>% count() %>% pull()
  dur_conn<-ts %>% filter(waterLevel>wL_spill) %>% count() %>% pull()
  
  #Create output tibble~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output<-tibble(
    WetID = names(df[,n]),
    wL_spill,
    recession_rate,
    wL_max,
    wL_mean,
    wL_mean_norm,
    wL_var,
    wL_var_norm,
    dur_inun = dur_inun,
    dur_conn = dur_conn
  )
  
  #Export output
  output
}


#Apply function to time series data---------------------------------------------
output<-lapply(seq(2,ncol(df)), fun) %>% bind_rows()

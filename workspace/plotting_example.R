#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Plotting Example
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 7/16/2019
#Purpose: Provide an example of how to interact with .squlite database
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup Worskspace-----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#clear environment
remove(list=ls())

#load relevant packages
#plotting libraries
library(xts)
library(dygraphs)
#database libraries
library(devtools)
devtools::install_github("khondula/rodm2")
library(RSQLite)
library(DBI)
library(rodm2)
#Data wrangling
library(lubridate)
library(readxl)
library(tidyverse)

#Read custom R functions
funs<-list.files("functions/", full.names = T)
for(i in 1:length(funs)){source(funs[i]);print(paste(i,"-", funs[i]))}

#Define working dir
working_dir<-"//nfs/palmer-group-data/Choptank/Nate/Hydrograph_Analysis/"

#Set system time zone 
Sys.setenv(TZ="America/New_York")

#Define database connection
db<-dbConnect(RSQLite::SQLite(), paste0(working_dir, "choptank.sqlite"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Query ts data for small number of sites------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 Compare "large" and "small" weltand----------------------------------------
#List sites in NB
db_get_sites(db)

#select time series (in this case, GB and NB to represent size distributions)
GB<-db_get_ts(db = db,site_code = "GB Wetland Well Shallow", variable_code_CV = "waterDepth")
NB<-db_get_ts(db = db,site_code = "NB Wetland Well Shallow", variable_code_CV = "waterDepth")

#Agregate to daily data
GB<-GB %>% 
  mutate(Timestamp=floor_date(Timestamp, "day")) %>% 
  group_by(Timestamp) %>% 
  summarise(waterDepth = mean(waterDepth, na.rm=T))
NB<-NB %>% 
  mutate(Timestamp=floor_date(Timestamp, "day")) %>% 
  group_by(Timestamp) %>% 
  summarise(waterDepth = mean(waterDepth, na.rm=T))

#Plot for funzies 
#plotting parameters
par(mgp=c(1.5,0.6,0)) 
par(ps=12)
par(cex.lab=14/12)
par(cex.axis=10/12)
#Start blank plot
plot(NB, type="n", ylab="Water Depth [m]", xlab="Timestamp")
#Add line for ground surface
abline(h=0, lty=2, lwd=2)
#Add ts data
points(GB, type="l", col="dodgerblue3", lwd=1.5)
points(NB, type="l", col="orangered3", lwd=1.5)
#Add legend
legend("bottomright", 
      c("Large Wetland", "Small Wetland"),
      col=c('dodgerblue3','orangered3'), 
      lty=c(1,1), 
      cex=12/12, 
      box.lty=0)
box()

#2.2 Plot Upland and Wetland Water ---------------------------------------------
#List sites in NB
db_get_sites(db)

#select time series (in this case, GB and NB to represent size distributions)
upland  <- db_get_ts(db = db, site_code = "QB Upland Well 1", variable_code_CV = "waterLevel")
wetland <- db_get_ts(db = db, site_code = "QB Wetland Well Shallow", variable_code_CV = "waterLevel")

#Agregate to daily data
upland<-upland %>% 
  mutate(Timestamp=floor_date(Timestamp, "day")) %>% 
  group_by(Timestamp) %>% 
  summarise(waterLevel = mean(waterLevel, na.rm=T))
wetland<-wetland %>% 
  mutate(Timestamp=floor_date(Timestamp, "day")) %>% 
  group_by(Timestamp) %>% 
  summarise(waterLevel = mean(waterLevel, na.rm=T))

#Plot for funzies 
#plotting parameters
par(mgp=c(1.5,0.6,0)) 
par(ps=12)
par(cex.lab=14/12)
par(cex.axis=10/12)
#Start blank plot
plot(upland, type="n", ylab="Water Depth [m]", xlab="Timestamp")
#Add line for ground surface
abline(h=0, lty=2, lwd=2)
#Add ts data
points(upland, type="l", col="orangered3", lwd=1.5)
points(wetland, type="l", col="dodgerblue3", lwd=1.5)
#Add legend
legend("bottomright", 
       c("Wetland", "Upland"),
       col=c('dodgerblue3','orangered3'), 
       lty=c(1,1), 
       cex=12/12, 
       box.lty=0)
box()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Query ts data for a large number of sites----------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create download function
fun<-function(site){
  #download data
  temp<-db_get_ts(db, paste(site), "waterDepth") %>% as_tibble(.)
  
  #add site collumn
  colnames(temp)<-c("Timestamp", "waterDepth")
  temp$site = paste(site)
  
  #Export to .GlovalEnv
  temp 
}

#3.1 Plot Aggregate Wetland Data------------------------------------------------
#Create list of wells to downlaod data for
site_names<-db_get_sites(db) %>% 
  enframe(.) %>%
  filter(str_detect(value,"Wetland Well Shallow")) %>%
  select(value) %>%
  as.matrix(.)

#Download Data
SWL<-lapply(site_names, fun) %>% bind_rows(.)

#Aggregate Data 
SWL<-SWL %>%
  mutate(Timestamp=floor_date(Timestamp, "day")) %>% 
  group_by(Timestamp) %>% 
  summarise(upr = quantile(waterDepth, 0.75, na.rm=T), 
            lwr = quantile(waterDepth, 0.25, na.rm=T), 
            waterDepth = mean(waterDepth, na.rm=T)) %>%
  filter(Timestamp>mdy("10/1/2017")) %>%
  filter(Timestamp<mdy("9/30/2018")) 

#Plot
SWL %>%
  ggplot(aes(x=Timestamp, y=waterDepth)) +
    geom_ribbon(aes(ymin=lwr, ymax=upr), fill="grey70") +
    geom_line() +
    scale_color_grey()+
    theme_bw() +
      labs(y="Water Depth [m]")

#Print
ggsave(paste0(working_dir,"output/aggregate_water_level.png"), width=7, height=5, units="in")

#Plot Aggregate Catchment Data--------------------------------------------------
#Create list of wells to downlaod data for
site_names<-db_get_sites(db) %>% 
  enframe(.) %>%
  filter(str_detect(value,"Catchment")) %>%
  select(value) %>%
  as.matrix(.)

#Download Data
SWL<-lapply(site_names, fun) %>% bind_rows(.)

#Aggregate Data 
SWL<-SWL %>%
  mutate(Timestamp=floor_date(Timestamp, "day")) %>% 
  mutate(waterDepth = if_else(waterDepth<0, 0, waterDepth)) %>%
  group_by(Timestamp) %>% 
  summarise(upr = quantile(waterDepth, 0.75, na.rm=T), 
            lwr = quantile(waterDepth, 0.25, na.rm=T), 
            waterDepth = mean(waterDepth, na.rm=T)) %>%
  filter(Timestamp>mdy("10/1/2017")) %>%
  filter(Timestamp<mdy("9/30/2018")) 

#Plot
SWL %>%
  ggplot(aes(x=Timestamp, y=waterDepth)) +
  geom_hline(aes(yintercept=0), lty=2) +
  geom_ribbon(aes(ymin=lwr, ymax=upr), fill="grey70") +
  geom_line() +
  scale_color_grey()+
  theme_bw() +
  labs(y="Water Depth [m]")

#Print
ggsave(paste0(working_dir,"output/aggregate_catchment_level.png"), width=3.5, height=3, units="in")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.0 Create interactive plot----------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create wide-format time series
SWL<-SWL %>%
  spread(site, waterDepth)

#plot
dygraph_ts_fun(SWL)
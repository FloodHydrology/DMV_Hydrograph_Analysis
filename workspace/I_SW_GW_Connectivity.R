#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: SW-GW Connectivity
#Coder: C. Nathan Jones (cnjones7@ua.edu)
#Date: 7/25/2020
#Purpose: Examine Patterns of SW-GW Connectivity
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup workspace------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear memory
rm(list=ls(all=TRUE))

#Defind relevant working directories
data_dir<-"C:\\Users\\cnjones7\\Box Sync\\My Folders\\Research Projects\\DMV_Hydro\\workspace\\"
output_dir<-"C:\\Users\\cnjones7\\Box Sync\\My Folders\\Research Projects\\DMV_Hydro\\workspaceoutput\\"

#Load relevant packages
library(tidyverse)
library(readxl)
library(lubridate)

#download data
swl<-readxl::read_xlsx(paste0(data_dir, "Choptank_Wetlands_WY2019.xlsx"), 
                      sheet="SWL", 
                      col_types = c("date",rep("numeric",17)))
gwl<-readxl::read_xlsx(paste0(data_dir, "Choptank_Wetlands_WY2019.xlsx"), 
                      sheet="GWL", 
                      col_types = c("date",rep("numeric",15)))
survey<-readxl::read_xlsx(paste0(data_dir, "well_relative_elevations.xlsx"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Tidy Data------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Add offset to gwl data
gwl<-gwl %>% 
  #Pivot Longer
  pivot_longer(-Timestamp, 
               values_to = "waterDepth") %>% 
  #Add Offset Data
  left_join(., survey %>% rename(name=well)) %>% 
  #Estiamte Water Level
  mutate(waterLevel = waterDepth+offset_m) %>% 
  #Select relevant collums
  select(Timestamp, name, waterLevel) %>% 
  #Group by Timestamp adn well
  mutate(Timestamp = floor_date(Timestamp, "day")) %>% 
  group_by(Timestamp, name) %>% 
  summarise(waterLevel=mean(waterLevel, na.rm = T))

#Reassign JC Wetland Well 1 to JB Wetland Well 3
gwl<-gwl %>% mutate(name = if_else(name == 'JC Upland Well 1','JB Upland Well 3', name))
  
#Tidy SWL
swl<-swl %>% 
  pivot_longer(-Timestamp,
               values_to = "waterLevel") %>% 
  #Group by Timestamp adn well
  mutate(Timestamp = floor_date(Timestamp, "day")) %>% 
  group_by(Timestamp, name) %>% 
  summarise(waterLevel=mean(waterLevel, na.rm = T))

#Combine datasets
df<-bind_rows(gwl, swl)

#Parse well name into wetland and well 
df<-df %>% 
  mutate(wetland = substr(name, 1,2)) %>% 
  mutate(well = substr(name, 4, nchar(name))) %>% 
  select(-name)

#Pivot Wider
df<-df %>% 
  pivot_wider(
    names_from = well,
    values_from = waterLevel) %>% 
  rename(
    sw   = `Wetland Well Shallow`, 
    up_1 = `Upland Well 1`, 
    up_2 = `Upland Well 2`,
    up_3 = `Upland Well 3`)

#Limite to 2019 Water Year
df<-df %>% 
  filter(Timestamp >= mdy("10-01-2017"), 
         Timestamp <= mdy("9-30-2018"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Plots of sw-gw connectivity ---------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df %>% filter(wetland=="ND") %>% 
  ggplot()+
  geom_line(aes(x=Timestamp, y=sw),
            col="dark blue", 
            lwd = 1.5) +
  geom_line(aes(x=Timestamp, y=up_1), 
            col="#D95F02", 
            lwd = 1) +
  geom_line(aes(x=Timestamp, y=up_2), 
            col="#E6AB02", 
            lwd = 1) +
  #Theme
  theme_bw()+
  ylab("Water Level [m]") +
  xlab(NULL) +
  #Axes Options
  theme(
    axis.title = element_text(size=14),
    axis.text  = element_text(size = 10)) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.0 Examine Duration of SW-GW Connectivity Direction --------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Remove days without wetland water level
df<-df %>% filter(!is.na(sw))

# #For testing (DELETE LATER)
# df<-df %>% filter(wetland=='NB')

#Estimate the number of upland wells
df<-df %>% 
  mutate(dummy_1 = if_else(is.na(up_1), 0, 1),
         dummy_2 = if_else(is.na(up_2), 0, 1),
         dummy_3 = if_else(is.na(up_3), 0, 1)) %>% 
  mutate(n_wells = dummy_1+dummy_2 + dummy_3) %>% 
  select(-dummy_1, -dummy_2, -dummy_3)

#Wetland source to GW
df<-df %>% 
  mutate(source_1 = if_else((up_1 - sw)>0, 0, 1), 
         source_2 = if_else((up_2 - sw)>0, 0, 1),
         source_3 = if_else((up_3 - sw)>0, 0, 1)) %>% 
  replace_na(list(
    source_1 = 0, 
    source_2 = 0, 
    source_3 = 0)) %>% 
  mutate(sum = source_1 + source_2 + source_3) %>% 
  mutate(source = if_else(sum>0, 1, 0)) %>%
  select(-source_1, -source_2, -source_3, -sum)
  
#Wetland a sink of groundwater
df<-df %>% 
  mutate(sink_1 = if_else((up_1 - sw)>0, 1, 0), 
         sink_2 = if_else((up_2 - sw)>0, 1, 0),
         sink_3 = if_else((up_3 - sw)>0, 1, 0)) %>% 
  replace_na(list(
    sink_1 = 0, 
    sink_2 = 0, 
    sink_3 = 0)) %>% 
  mutate(sum = sink_1 + sink_2 + sink_3) %>% 
  mutate(sink = if_else(sum>0, 1, 0)) %>% 
  select(-sink_1, -sink_2, -sink_3, -sum) 

#define flow through conditions
df<-df %>%  
  mutate(flow_through = if_else((source+sink) == 2, 1, 0)) %>% 
  mutate(sink = if_else(flow_through==1, 0, sink),
         source = if_else(flow_through==1, 0, source)) 
  
#Estimate by well
df %>% 
  mutate(count = 1) %>% 
  group_by(wetland) %>%  
  summarise(
    n_days = sum(count),
    n_wells = mean(n_wells, na.rm=T),
    source = sum(source), 
    sink = sum(sink), 
    flow_through = sum(flow_through)) %>% 
  filter(n_wells>0)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5.0 Data vizulization ---------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5.1 Stacked bar plot-----------------------------------------------------------
#Tidy data
bars<-df %>% 
  #Summarise by wetland
  group_by(wetland) %>%  
  summarise(
    n_wells = mean(n_wells, na.rm=T),
    source = sum(source), 
    sink = sum(sink), 
    flow_through = sum(flow_through)) %>% 
  filter(n_wells>0, flow_through>0) %>%
  select(-n_wells) %>%
  arrange(source) %>% 
  #pivot longer
  pivot_longer(-wetland) 

#Create facotrs for soorting
bars$wetland<-factor(bars$wetland, levels=c("ND", "DK", "JB", "TB", "QB"))
bars$name<-factor(bars$name, levels=c("sink", "flow_through", "source"))

#Plot
bars %>% ggplot() + 
  #Bar Plot Options
  geom_bar(
    aes(fill = name,y = value, x = wetland),
    position = 'fill',
    stat='identity') +
  #Color Pallette
  scale_fill_manual(
    values=c("#6a3d9a", "#33a02c", "#1f78b4"),
    name = "Hydrologic State") +
  #Theme Options
  theme_bw() + 
    xlab("Wetland Code") + 
    ylab("Proportion of Time") +
    theme(
      axis.title = element_text(size=14),
      axis.text  = element_text(size = 10)) +
  #Legend Options
  theme(legend.position = "bottom", 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=10)) 

#5.2 Time Series Plot-----------------------------------------------------------
#Add color
#Add proportion

#Tidy Data
ts<-df %>% 
  #Filter to wetlands of interest
  filter(wetland %in% c("ND","DK","JB","TB","QB")) %>% 
  #select collumns of interest
  select(Timestamp, source, sink, flow_through) %>% 
  #Pivot longer
  pivot_longer(-Timestamp) %>% arrange(Timestamp) %>% 
  #Summarise by day and hydrologic state
  group_by(Timestamp, name) %>%
  summarise(value = sum(value)) %>% 
  #PIvot wider
  pivot_wider(names_from=name, values_from=value) %>% 
  #Estimate prop
  mutate(
    ft = flow_through/(flow_through + sink + source), 
    si = sink/(flow_through + sink + source),
    so = source/(flow_through + sink + source)) %>% 
  select(-flow_through, -sink, -source) %>% 
  rename(flow_through=ft, sink = si, source = so) %>% 
  #Pivot Longer
  pivot_longer(-Timestamp)


#Make hydro states a factor
ts$name<-factor(ts$name, levels=c("sink", "flow_through", "source"))

#Plot!
ggplot(ts, aes(x=Timestamp, y=value, fill=name)) + 
  geom_area() +
  scale_fill_manual(
    values=c("#6a3d9a", "#33a02c", "#1f78b4"), 
    name = "Hydrologic State") + 
  theme_bw() + 
    ylab('Proportion of Wetlands(') +
    xlab(NULL) +
    #Axes Options
    theme(
      axis.title = element_text(size=14),
      axis.text  = element_text(size = 10)) +
    #Legend Options
    theme(legend.position = "bottom", 
          legend.title = element_text(size=14), 
          legend.text = element_text(size=10)) 
    
    
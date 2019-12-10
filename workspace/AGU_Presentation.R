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
library(readxl)
library(lubridate)
library(segmented)

#download data
hydro<-read_csv(paste0(data_dir,"output/hydro_metrics.csv"))
geo<-read_csv(paste0(data_dir, "spatial_data.csv"))
df<-left_join(hydro, geo %>% rename(WetID = Wetland))
ts<-readxl::read_xlsx(paste0(data_dir, "Choptank_Wetlands_WY2019.xlsx"), 
                      sheet="SWL", 
                      col_types = c("date",rep("numeric",17)))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Hydrologic Regime Plots----------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 Annual Hydrograph ---------------------------------------------------------
ts %>%
  #pivot long
  pivot_longer(-Timestamp) %>% 
  mutate(Timestamp=floor_date(Timestamp, "day"), 
         value = value*100) %>% 
  group_by(Timestamp) %>% 
  summarise(upr = quantile(value, 0.75, na.rm=T), 
            lwr = quantile(value, 0.25, na.rm=T), 
            waterDepth = mean(value, na.rm=T)) %>%
  filter(Timestamp>mdy("10/1/2017")) %>%
  filter(Timestamp<mdy("9/30/2018")) %>% 
  ggplot(aes(x=Timestamp, y=waterDepth)) +
    geom_ribbon(aes(ymin=lwr, ymax=upr), fill="grey70") +
    geom_line() +
    scale_color_grey()+
    theme_bw() +
      theme(axis.text=element_text(size=14), 
            axis.title=element_text(size=18)) +
      labs(y="Water Depth [cm]", x=NULL)

ggsave(paste0(output_dir,"agu_wetland_hydro_1.png"), width=7, height=5, units="in")

#2.2 Annual Hydrograph (Rising Limb)--------------------------------------------
rect<-  tibble(
  start  = mdy("10/27/2017"),
  stop   = mdy("2/12/2018"),
  top    = 85,
  bottom = -20
)

ts_temp<-ts %>%
  #pivot long
  pivot_longer(-Timestamp) %>% 
  mutate(Timestamp=floor_date(Timestamp, "day"), 
         value = value*100) %>% 
  group_by(Timestamp) %>% 
  summarise(upr = quantile(value, 0.75, na.rm=T), 
            lwr = quantile(value, 0.25, na.rm=T), 
            waterDepth = mean(value, na.rm=T)) %>%
  filter(Timestamp>mdy("10/1/2017")) %>%
  filter(Timestamp<mdy("9/30/2018")) %>% 
  mutate(Timestamp = lubridate::as_date(Timestamp))
  
ggplot() +
    geom_rect(data = rect,
              aes(xmin=start, xmax=stop, ymax=top, ymin=bottom),
              fill="darkgoldenrod3", alpha=0.8) +
    geom_ribbon(data = ts_temp, 
                aes(ymin=lwr, ymax=upr, x = Timestamp), 
                fill="grey70") +
    geom_line(data = ts_temp, 
              aes(y=waterDepth, x = Timestamp)) +
    scale_color_grey()+
    theme_bw() +
      theme(axis.text=element_text(size=14), 
            axis.title=element_text(size=18)) +
      labs(y="Water Depth [cm]", x=NULL)

ggsave(paste0(output_dir,"agu_wetland_hydro_2.png"), width=7, height=5, units="in")

#2.3 Annual Hydrograph (Connected)--------------------------------------------
rect<-  tibble(
  start  = mdy("2/12/2018"),
  stop   = mdy("6/12/2018"),
  top    = 85,
  bottom = -20
)

ts_temp<-ts %>%
  #pivot long
  pivot_longer(-Timestamp) %>% 
  mutate(Timestamp=floor_date(Timestamp, "day"), 
         value = value*100) %>% 
  group_by(Timestamp) %>% 
  summarise(upr = quantile(value, 0.75, na.rm=T), 
            lwr = quantile(value, 0.25, na.rm=T), 
            waterDepth = mean(value, na.rm=T)) %>%
  filter(Timestamp>mdy("10/1/2017")) %>%
  filter(Timestamp<mdy("9/30/2018")) %>% 
  mutate(Timestamp = lubridate::as_date(Timestamp))

ggplot() +
  geom_rect(data = rect,
            aes(xmin=start, xmax=stop, ymax=top, ymin=bottom),
            fill="steelblue4", alpha=0.6) +
  geom_ribbon(data = ts_temp, 
              aes(ymin=lwr, ymax=upr, x = Timestamp), 
              fill="grey70") +
  geom_line(data = ts_temp, 
            aes(y=waterDepth, x = Timestamp)) +
  scale_color_grey()+
  theme_bw() +
  theme(axis.text=element_text(size=14), 
        axis.title=element_text(size=18)) +
  labs(y="Water Depth [cm]", x=NULL)

ggsave(paste0(output_dir,"agu_wetland_hydro_3.png"), width=7, height=5, units="in")

#2.4 Annual Hydrograph (Connected)--------------------------------------------
rect<-  tibble(
  start  = mdy("6/12/2018"),
  stop   = mdy("9/7/2018"),
  top    = 85,
  bottom = -20
)

ts_temp<-ts %>%
  #pivot long
  pivot_longer(-Timestamp) %>% 
  mutate(Timestamp=floor_date(Timestamp, "day"), 
         value = value*100) %>% 
  group_by(Timestamp) %>% 
  summarise(upr = quantile(value, 0.75, na.rm=T), 
            lwr = quantile(value, 0.25, na.rm=T), 
            waterDepth = mean(value, na.rm=T)) %>%
  filter(Timestamp>mdy("10/1/2017")) %>%
  filter(Timestamp<mdy("9/30/2018")) %>% 
  mutate(Timestamp = lubridate::as_date(Timestamp))

ggplot() +
  geom_rect(data = rect,
            aes(xmin=start, xmax=stop, ymax=top, ymin=bottom),
            fill="forestgreen", alpha=0.6) +
  geom_ribbon(data = ts_temp, 
              aes(ymin=lwr, ymax=upr, x = Timestamp), 
              fill="grey70") +
  geom_line(data = ts_temp, 
            aes(y=waterDepth, x = Timestamp)) +
  scale_color_grey()+
  theme_bw() +
  theme(axis.text=element_text(size=14), 
        axis.title=element_text(size=18)) +
  labs(y="Water Depth [cm]", x=NULL)

ggsave(paste0(output_dir,"agu_wetland_hydro_4.png"), width=7, height=5, units="in")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Recession Analysis---------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.1 Plot Example Recession Plot------------------------------------------------
temp<-ts %>% select(Timestamp, 'TB Wetland Well Shallow') %>% 
  rename(waterLevel = 'TB Wetland Well Shallow') %>% 
  mutate(Timestamp=floor_date(Timestamp, "day"), 
         waterLevel = waterLevel*100) %>% 
  group_by(Timestamp) %>% 
  summarise(waterLevel = mean(waterLevel, na.rm=T)) %>% 
  mutate(dwL = waterLevel - lag(waterLevel)) %>% 
  filter(dwL<0) %>% 
  filter(waterLevel>0)

#Create Segmented Model
lin_mod <- lm(dwL~waterLevel,data=temp)
segmented_mod <- segmented(lin_mod, seg.Z = ~waterLevel, psi=quantile(temp$waterLevel, 0.75))

#Estimate spill threshold
wL_spill<-segmented_mod$psi[2]

#Esitmate recession rate
recession_rate<-segmented_mod$coefficients[2]

#Export recession plot
png(paste0(output_dir,"agu_recession.png"), width=6.5, height=3.75, units = 'in', res=300)
par(mar=c(5, 6, 2, 2) + 0.1)
plot(segmented_mod, lty=2, lwd=2, col="red", rug=F, 
     #Y Limits
     ylim=c(-4,0),
     #Labels
     xlab = "Water Level [cm]", ylab= 'Recession Rate\n[cm/day]',
     #Label Size
     ps=12, cex.lab=18/12, cex.axis=14/12
)
points(temp$waterLevel, temp$dwL, pch=19, col=alpha("grey30", 0.3))
dev.off()  

#3.2 Plot Distribution----------------------------------------------------------
hydro %>% 
  select(recession_rate) %>% 
  mutate(recession_rate=recession_rate) %>% 
  ggplot(aes(recession_rate)) +
    #add density plot
    geom_density(color='black', fill="forestgreen", alpha=0.6) + 
    #add black/white them
    theme_bw()+
      labs(x = "Recession Coef [1/day]", y = "Density [%]") +
      theme(axis.text=element_text(size=14), 
            axis.title=element_text(size=18))

ggsave(paste0(output_dir,"agu_recession_density.png"), width=3.5, height=3, units="in")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.0 Connectivity Analysis------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.1 Hydroperiod----------------------------------------------------------------
hydro %>%
  select(inun_dur) %>% 
  #Start ggplot object
  ggplot(aes(inun_dur)) +
  #add density plot
    geom_density(color='black', fill="steelblue", alpha=0.6) + 
    #add black/white them
    theme_bw()+
      labs(x = "Inundation Duration [days]", y = "Density [%]") +
      theme(axis.text=element_text(size=14), 
            axis.title=element_text(size=18))
ggsave(paste0(output_dir,"agu_inun_dur.png"), width=3.5, height=3, units="in")

#4.2 Dry------------------------------------------------------------------------
hydro %>%
  #Start ggplot object
  ggplot(aes(dis_dur)) +
  #add density plot
  geom_density(color='black', fill="darkgoldenrod3", alpha=0.6) + 
  #add black/white them
  theme_bw()+
  labs(x = "Dry Duration [days]", y = "Density [%]") +
  theme(axis.text=element_text(size=14), 
        axis.title=element_text(size=18))
ggsave(paste0(output_dir,"agu_dry_dur.png"), width=3.5, height=3, units="in")

#4.3 Connectivity---------------------------------------------------------------
hydro %>%
  #Start ggplot object
  ggplot(aes(con_dur)) +
  #add density plot
  geom_density(color='black', fill="forestgreen", alpha=0.6) + 
  #add black/white them
  theme_bw()+
  labs(x = "Days of Connectivity", y = "Density [%]") +
  theme(axis.text=element_text(size=14), 
        axis.title=element_text(size=18))
ggsave(paste0(output_dir,"agu_conn_dur.png"), width=3.5, height=3, units="in")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5.0 SW-GW Exchange Analysis----------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#6.0 Hydrogeomorphic Analysis---------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#6.1 Wetland Size---------------------------------------------------------------
plot(df$area_m2, df$wL_mean, log="xy")
lm(log10(df$area_m2)~log10(df$wL_mean)) %>% summary()

plot(df$area_m2, df$wL_var, log="xy")
lm(log(df$area_m2)~log(df$wL_var)) %>% summary()

plot(df$area_m2, df$inun_dur, log="xy")
lm(log(df$area_m2)~(df$inun_dur)) %>% summary()

#6.2 Storage Capacity-----------------------------------------------------------
plot(df$wetland_hsc_cm, df$recession_rate, log="xy")
lm(log10(df$wetland_hsc_cm)~log10(df$recession_rate)) %>% summary()

plot(df$watershed_hsc_cm, df$recession_rate, log="xy")
lm(log10(df$watershed_hsc_cm)~log10(df$recession_rate)) %>% summary()

#6.3 Storage Capacity-----------------------------------------------------------
plot(df$mean_elevation_m,df$dis_n_events)
lm((df$mean_elevation_m)~(df$dis_n_events)) %>% summary()

plot(df$watershed_hsc_cm, df$recession_rate, log="xy")
lm(log10(df$watershed_hsc_cm)~log10(df$recession_rate)) %>% summary()


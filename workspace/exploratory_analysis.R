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
output_dir<-"C://choptank/output/"

#Load relevant packages
library('scales')
library('readxl')
library('lubridate')
library('segmented')
library('tidyverse')
library('vegan')

#download data
geo<-read_csv(paste0(data_dir, "spatial_data.csv"))
df<-read_xlsx(paste0(data_dir, 'Choptank_Wetlands_WY2019.xlsx'), sheet = 'SWL',  na='NA', col_types = c('date', rep('numeric',17)))

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
#JB A has a werid off set in recession plot
df<-df %>% select(-'JA')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Characterize hydrologic regime---------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 Create function------------------------------------------------------------
fun<-function(n){
  
  #A. Setup workspace~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Isolate time series
  ts<-df[,c(1,n)] 
  colnames(ts)<-c("time", "waterLevel")
  
  #Remove NA
  ts<-na.omit(ts)
  
  #Aquire wetland name
  WetID <- names(df[,n])
  
  #B. Recession Analysis~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Isolate recession dayes
  temp<-ts %>% mutate(dwL = waterLevel - lag(waterLevel)) %>% filter(dwL<0) %>% filter(waterLevel>0)
  
  #Create Segmented Model
  lin_mod <- lm(dwL~waterLevel,data=temp)
  segmented_mod <- segmented(lin_mod, seg.Z = ~waterLevel, psi=quantile(temp$waterLevel, 0.75))
  
  #Estimate spill threshold
  wL_spill<-segmented_mod$psi[2]
  
  #Esitmate recession rate
  recession_rate<-segmented_mod$coefficients[2]
  
  #Export recession plot
  png(paste0(output_dir,'recession/',WetID,".png"), width=6.5, height=4, units = 'in', res=300)
  par(mar=c(5, 5, 4, 2) + 0.1)
  plot(segmented_mod, lty=2, lwd=2, col="red", rug=F, 
       #Y Limits
       ylim=c(-0.05,0),
       #Labels
       title = WetID, xlab = "Water Level [m]", ylab= 'Recession Rate\n[m/day]',
       #Label Size
       ps=12, cex.lab=14/12, cex.axis=10/12
  )
  
  points(temp$waterLevel, temp$dwL, pch=19, col=alpha("grey30", 0.3))
  mtext(paste0(WetID, ' Wetland'), side = 3, line= 1, cex = 2)
  dev.off()  
  
  #Create some rules to make sure segmented reggression makes sense
  if((segmented_mod$coefficients[2]+segmented_mod$coefficients[3])>0){
    
    #Update coefficients
    wL_spill<-max(temp$waterLevel)
    recession_rate<-lin_mod$coefficients[2]
    
    #Export new recession plot
    png(paste0(output_dir,'recession/',WetID,".png"), width=6.5, height=4, units = 'in', res=300)
    par(mar=c(5, 5, 4, 2) + 0.1)
    plot(temp$waterLevel, temp$dwL, pch=19, col="grey30", 
         #Y Limits
         ylim=c(-0.05,0),
         #Labels
         title = WetID, xlab = "Water Level [m]", ylab= 'Recession Rate\n[m/day]',
         #Label Size
         ps=12, cex.lab=14/12, cex.axis=10/12
    )
    abline(lin_mod, lty=2, lwd=2, col="red")
    mtext(paste0(WetID, ' Wetland'), side = 3, line= 1, cex = 2)
    dev.off()  
  }
  
  #C. Estimate water level metrics~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Characterize water level
  wL_max<-max(ts$waterLevel, na.rm=T)
  wL_mean<-mean(ts$waterLevel, na.rm=T)
  wL_mean_norm<- wL_mean/wL_spill
  wL_var<-var(ts$waterLevel, na.rm = T)
  wL_var_norm<-wL_var/wL_spill
  
  #Characterize duration of different components
  inun_dur<-ts %>% filter(waterLevel>(0.05*wL_spill)) %>% count() %>% pull()
  
  #D. Chracterize Connectivity~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Isolate individual events
  con<-tibble(
    start  = ts %>% filter(lag(waterLevel)<wL_spill & waterLevel>=wL_spill) %>% select(time) %>% pull, 
    stop   = ts %>% filter(lag(waterLevel)>wL_spill & waterLevel<=wL_spill | time == max(time) & waterLevel>wL_spill) %>% select(time) %>% pull,
    top    = ts %>% select(waterLevel) %>% summarise(max(waterLevel)) %>% pull,
    bottom = ts %>% select(waterLevel) %>% summarise(min(waterLevel)) %>% pull
  )
  
  #Total duration of connectivity 
  con_dur<-ts %>% filter(waterLevel>wL_spill) %>% count() %>% pull()
  
  #Frequency of Connectivity
  con_n_events<-nrow(con)
  
  #Duration of typical event
  con_mean_dur<- con %>% mutate(dif = as.numeric(paste(stop - start))) %>% select(dif) %>% summarise(mean(dif)) %>% pull()
  
  #E. Chracterize disconnectivity~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dis<-tibble(
    start  = ts %>% filter(lag(waterLevel)>0 & waterLevel<=0 | time == min(time) & waterLevel<0) %>% select(time) %>% pull, 
    stop   = ts %>% filter(lag(waterLevel)<0 & waterLevel>=0) %>% select(time) %>% pull,
    top    = ts %>% select(waterLevel) %>% summarise(max(waterLevel)) %>% pull,
    bottom = ts %>% select(waterLevel) %>% summarise(min(waterLevel)) %>% pull
  )
  
  #Total duration of disnectivity 
  dis_dur<-ts %>% filter(waterLevel<0) %>% count() %>% pull()
  
  #Frequency of disnectivity
  dis_n_events<-nrow(dis)
  
  #Duration of typical event
  dis_mean_dur<- dis %>% mutate(dif = as.numeric(paste(stop - start))) %>% select(dif) %>% summarise(mean(dif)) %>% pull()
  
  #F. Plot~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #start plotting device
  png(paste0(output_dir,'time_series/',WetID,".png"), width=6.5, height=4, units = 'in', res=300)
  
  #Start GGPlot Device
  p<-ggplot() +
    #Add line thresholds
    geom_hline(yintercept = wL_spill, col="steelblue4", lty=2)+
    geom_hline(yintercept = 0, col="tan4", lty=2)+
    #Add Period od Connectivity
    geom_rect(data = con,
              aes(xmin=start, xmax=stop, ymax=top, ymin=bottom),
              fill="steelblue4", alpha=0.6)+
    #Add Period od Disconnectivity
    geom_rect(data = dis,
              aes(xmin=start, xmax=stop, ymax=top, ymin=bottom),
              fill="tan4", alpha=0.6)+
    #Add line
    geom_path(data = ts, aes(time, waterLevel)) + 
    #Add BW Theme
    theme_bw() +
    #Add Labels
    labs(title = paste(WetID, "Wetland"), x = 'Date', y= "Water Level [m]")
  
  print(p)
  
  #Turn plotting device off
  dev.off()
  
  #G. Export output tibble~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output<-tibble(
    WetID,
    wL_spill,
    recession_rate,
    wL_max,
    wL_mean,
    wL_mean_norm,
    wL_var,
    wL_var_norm,
    inun_dur,
    con_dur, 
    con_n_events, 
    con_mean_dur, 
    dis_dur, 
    dis_n_events, 
    dis_mean_dur
  )
  
  #Export output
  output
}

#2.2 Apply function to time series data-----------------------------------------
hydro<-lapply(seq(2,ncol(df)), fun) %>% bind_rows()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Examine spatial and temporal distributions---------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.1 Hydrologic Regime----------------------------------------------------------
#A. Create Plot~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
h<-hydro %>%
  #Change Row Headings for chart titles
  rename(
    '1. Water Level: Spill [m]' = wL_spill, 
    '1. Water Level: Recession [m/day]' = recession_rate,
    '1. Water Level: Max [m]' = wL_max,
    '1. Water Level: Mean [m]' = wL_mean,
    '1. Water Level: Mean [m/m]' = wL_mean_norm,
    '1. Water Level: Variance' = wL_var,
    '1. Water Level: Variance [norm]' = wL_var_norm,
    '2. Duration: Inundation [days]'= inun_dur,
    '2. Duration: Connectivity [days]' = con_dur,
    '3. Frequency: Spill Events' = con_n_events,
    '4. Period: Spill Event [days]' = con_mean_dur,
    '2. Duration: Dry [days]' = dis_dur,
    '3. Frequency: Dry Events' = dis_n_events,
    '4. Period: Dry Event [days]' =dis_mean_dur
  ) %>% 
  #Pivot to long format
  pivot_longer(-WetID, 
               names_to='metric', 
               values_to='val') %>% 
  #Start ggplot object
  ggplot(aes(val)) +
  #Facet object
  facet_wrap(.~metric, scales='free') +
  #add density plot
  geom_density(fill='steelblue4', alpha=0.6) + 
  #add black/white them
  theme_bw()

#print
h

#Print Plot~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#start plotting device
png(paste0(output_dir,'distributions/hydro_regime.png'), width=13, height=7, units = 'in', res=300)
print(h)
dev.off()

#3.2 Hydrogeomorphic Data-------------------------------------------------------
#A. Create Plot~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
g<-geo %>%
  #Pivot to long format
  pivot_longer(-Wetland, 
               names_to='metric', 
               values_to='val') %>% 
  #Start ggplot object
  ggplot(aes(val)) +
  #Facet object
  facet_wrap(.~metric, scales='free') +
  #add density plot
  geom_density(fill='dark orange', alpha=0.6) + 
  #add black/white them
  theme_bw()

#print
g

#Print Plot~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#start plotting device
png(paste0(output_dir,'distributions/hydrogeomorphic_characteristics.png'), width=13, height=7, units = 'in', res=300)
print(g)
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.0 NMDS Analysis--------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.1 NMDS Analysis--------------------------------------------------------------
#Create matrix for NMDS
x<-hydro %>% 
  #Filter FN out (for now)
  filter(WetID!="FN") %>% 
  #Convert WetID to RowName
  column_to_rownames('WetID') %>% 
  #Take care of NA
  mutate(dis_mean_dur = if_else(is.na(dis_mean_dur),0,dis_mean_dur))

#Scale variable
x_scaled<-scale(x)

#Create NMDS Model
set.seed(7)
n<-1000
mds.1 <-metaMDS(x_scaled,distance = "euclidean", k=1, trymax = n)
mds.2 <-metaMDS(x_scaled,distance = "euclidean", k=2, trymax = n)
mds.3 <-metaMDS(x_scaled,distance = "euclidean", k=3, trymax = n)
mds.4 <-metaMDS(x_scaled,distance = "euclidean", k=4, trymax = n)
mds.5 <-metaMDS(x_scaled,distance = "euclidean", k=5, trymax = n)
mds.6 <-metaMDS(x_scaled,distance = "euclidean", k=6, trymax = n)

#4.2 Analysis of fit------------------------------------------------------------
#Scree Plot
stress<-c(mds.1$stress, mds.2$stress, mds.3$stress, mds.4$stress, mds.5$stress, mds.6$stress)
plot(stress,
     ylim = c(0,0.27),
     xlab="Number of Axes",
     type = "o",
     ylab="Stress",
     cex.lab=1.6,
     lty = 2,
     lwd = 3,
     pch=21,
     col= "black",
     bg= "red",
     cex = 3,
     bty="l")
box(lwd=2, 
    bty = "l")

#Shephards Plot
par(mfrow=c(1,2))
stressplot(mds.3,
           main = "Shepard Plot")
gof = goodness(mds.3)
plot(mds.3, 
     type = "p",
     main = "Goodness of Fit",
     cex = gof*200)
par(mfrow=c(1,1))

#After visual investigation, a 3-axes model is most appropriate~~~~~~~~~~~~~~~~~

#4.3 Environmental Vector Analysis----------------------------------------------
#4.3.A Hydro Variables----------------------------------------------------------
#Fit environmnetal vectors
ef3<-envfit(mds.3, x, permu=n, choices = c(1,2))

#Create function for plotting
site.plot<-function(mds.x,mds.y,X.Label, Y.Label){
  plot(mds.x,mds.y, 
       #Point Options
       pch=21,bg= "gray",lwd = 2, cex=2.5,
       #Axes Labels
       xlab = X.Label, ylab = Y.Label,
       #Axes Options
       bty = "L", ps=12, cex.axis=10/12, cex.lab=14/12
  )
  #text(mds.x,mds.y, labels = rownames(x))
  plot(ef3, add = TRUE, cex = 1.3, col = "black")     
}

#Plot
#start plotting device
png(paste0(output_dir,'ordination/nmds_hydro.png'), width=6.5, height=6, units = 'in', res=300)
site.plot(mds.3$points[,1],mds.3$points[,2], "NMDS1 (Hydro Variability)", "NMDS2 (Inundation Duration)")
dev.off()

#4.3.B Geomorph Variables-------------------------------------------------------
#Create fit matrix
y<-geo %>% column_to_rownames('Wetland') %>% filter(geo$Wetland %in% hydro$WetID )

#Select the most relevant variables
y<-y %>% select(wetland_invert, watershed_hsc_cm, mean_elevation_m, hans_m)

#Fit environmnetal vectors
ef3<-envfit(mds.3, y, permu=10000, choices = c(1,2,3))

#Plot
png(paste0(output_dir,'ordination/nmds_geo.png'), width=6.5, height=6, units = 'in', res=300)
site.plot(mds.3$points[,1],mds.3$points[,2], "NMDS1 (Hydro Variability)", "NMDS2 (Inundation Duration)")
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Export Hydro Metrics~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_csv(hydro, paste0(data_dir,"output/hydro_metrics.csv"))

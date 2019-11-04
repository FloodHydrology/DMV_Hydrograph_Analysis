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

#download data
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
  dis_dur<-ts %>% filter(waterLevel>wL_spill) %>% count() %>% pull()
  
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
regime<-lapply(seq(2,ncol(df)), fun) %>% bind_rows()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Examine spatial and temporal distributions---------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.1 Hydrologic Regime----------------------------------------------------------
#A. Create Plot~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
r<-regime %>%
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

#Print Plot~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#start plotting device
png(paste0(output_dir,'distributions/hydro_regime.png'), width=13, height=7, units = 'in', res=300)
print(r)
dev.off()







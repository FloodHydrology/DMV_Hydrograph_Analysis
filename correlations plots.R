#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Ceorrelation plots
#Coder: C. Nathan Jones (cnjones7@ua.edu)
#Date: 11/5/2019  
#Purpose: 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup workspace------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear memory
rm(list=ls(all=TRUE))

#Download Libraries
library(corrplot)
library(tidyverse)

#Defind relevant working directories
data_dir<-"C://choptank/"
output_dir<-"C://choptank/output/"

#download data
geo_sites<-read_csv(paste0(data_dir, "spatial_data.csv"))

#download data
geo<-read_csv(paste0(data_dir, "spatial_data_all.csv")) %>% 
  rename(wetland_invert = z) %>% 
  filter(perimeter_m<2000) %>% 
  filter(watershed_area_m2<1e6) %>% 
  filter(volume_m3<5000) %>% 
  filter(wetland_hsc_cm<30) %>% 
  filter(watershed_hsc_cm>2) 

hydro<-read_csv(paste0(data_dir,"output/hydro_metrics.csv"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Function-------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Correlatin test
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Geo Plots-------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Prep data
df<-geo %>% select(c(
  "subshed_area_m2","watershed_area_m2","volume_m3", 'wetland_invert',
  "wetland_hsc_cm","watershed_hsc_cm","a_axis_length_m","perimeter_m",
  "area_m2", "p_a_ratio","hand_m","mean_elevation_m","hans_m","wet_order")) %>% 
  filter(!is.na(wetland_hsc_cm)) %>% 
  filter(watershed_hsc_cm>2)
m<-cor(df)
p.mat <- cor.mtest(df)

#Plot
png(paste0(output_dir,'correlations/geo.png'), 
    width=6.5, height=6, units = 'in', res=300)

corrplot(m, type="upper", order="hclust", 
         p.mat = p.mat, sig.level = 0.01)

dev.off()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.0 Hydro Plots-------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#prep data
df<-hydro %>% select(-c(WetID)) 
m<-cor(df)
p.mat <- cor.mtest(df)


#Plot
png(paste0(output_dir,'correlations/hydro.png'), 
    width=6.5, height=6, units = 'in', res=300)

corrplot(m, type="upper", order='hclust', 
         p.mat = p.mat, sig.level = 0.01)

dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5.0 Hydro Plots-------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hydro<-hydro %>% arrange(WetID)
geo_sites<-geo_sites %>% filter(Wetland!='FN') %>% arrange(Wetland)
df<-full_join(hydro, geo_sites %>% rename(WetID=Wetland))



#Plot
png(paste0(output_dir,'correlations/hydro_geo.png'), 
    width=30, height=30, units = 'in', res=300)
par(mfrow=c(15,14))
for(i in 2:length(hydro)){
  for(j in (length(hydro)+1):length(df))
    plot(df[c(j,i)], pch=19, 
         col="grey30", cex=2, 
         cex.lab=10/12, cex.axis=14/12)
    
    m<-data.frame(
      x<-df[,i]
      y<-df[,j]
    ) %>% na.omit(.)
    
    model<-lm(x~y)
  
}
dev.off()









df<-left_join(hydro %>% mutate(WetID = paste(WetID)), 
              geo_sites %>% rename(WetID = Wetland)) %>% 
  filter(WetID!='FN') %>% 
  select(-WetID)

m<-cor(df)
p.mat <- cor.mtest(df)

corrplot(m, type="upper", order='hclust', 
         p.mat = p.mat, sig.level = 0.01)



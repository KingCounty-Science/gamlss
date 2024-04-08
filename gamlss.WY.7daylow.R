library(tidyverse)
library(lubridate)

library(prism)
library(sf)
library(raster)
# library(terra)

library(EGRET)
library(dataRetrieval)

library(gamlss)
# library(gamlss.util)

library(patchwork)

library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("lag", "dplyr")

options(scipen=8,digits=7)

###############################################################################################################
staid <- c('12134500','12148500','12142000','12141300','12143400',
           '12145500','12144500','12149000','12115000','12115500',
           '12117000','12147600')
staname <- c('Skykomish','Tolt','NF Snoqualmie','MF Snoqualmie','SF Snoqualmie',
             'Raging','Snoqualmie nr Snoqualmie','Snoqualmie nr Carnation','Cedar River nr Cedar Falls',
             'Rex River nr Cedar Falls','Taylor River nr Selleck','SF Tolt nr Index')

# Remove Quinalt due to upstream storage (Lake Quinault)
# Remove Wilson and Nehalem because too far south (in Oregon)
# staname <- c('Dungeness','Quinault','Satsop','Naselle','Nehalem','Wilson')
staname <- c(staname,'Dungeness','Satsop','Naselle')
# staid <- c('12048000','12039500','12035000','12010000','14301000','14301500')
staid <- c(staid,'12048000','12035000','12010000')

# Queets 12040500 - 1931-2021 + huge gap in daily mean flow, odd, but smaller gap in inst. pk Q
staname <- c(staname,'Sauk abv White Chuck','NF Stillaguamish',
             'Puyallup at Puyallup','Sauk nr Sauk','NF Skokomish',
             'Queets')
staid <- c(staid,'12186000','12167000',
           '12101500','12189500','12056500',
           '12040500')
usgs <- data.frame(site_no = staid, basin = staname)

staid <- c('11377100')
staname <- c('Sacramento')
usgs <- data.frame(site_no = staid, basin = staname)
##############################################################################################################
# shape files created by streamstats_app.R

## 
file_list <- list.files("./rivers2_shp", pattern = "*mento.shp", full.names = TRUE)
########################################################################
########################################################################
### winter precipitation Oct-Mar 
########################################################################
prism_set_dl_dir('C:/prismtmp')

# function to extract mean annual precipitation
mean_grid_function<-function(shp_in,rasin,prj){ # the input should be the shapefile
  # shp<-shapefile(name) #convert to shapefile object
  shp_in<-st_transform(shp_in,st_crs(rasin)) #reproject shape
  pavg <- raster::extract(rasin,shp_in,fun=mean, na.rm = TRUE) #get spatially averaged rainfall
  return(pavg)
}

output.ppt <- NULL
output.tmean <- NULL
for(shape in file_list){
  shp_in <- st_read(shape)
  
  for(yr in 1929:2021){
    for (mo in 1:12){
      
      ppt <- raster(pd_to_file(prism_archive_subset("ppt", "monthly", years = yr, mon = mo)))
      tmean <- raster(pd_to_file(prism_archive_subset("tmean", "monthly", years = yr, mon = mo)))
      prj<-toString(crs(ppt)) #get raster projection
      dum <- mean_grid_function(shp_in,ppt,prj)
      
      dum <- dum/25.4
      names(dum)[1]<- 'Prec_in'
      dum <- data.frame(Prec_in = dum)
      # samm_basins <- filter(Sammamish@data,WTRSHD_NAM == 'Sammamish River')
      dum <- bind_cols(shp_in,dum)
      dum$Year <- yr
      dum$Month <- mo
      dum$shape <- shape
      dum$basin <- substr(shape,15,nchar(shape)-4)
      
      output.ppt <- bind_rows(output.ppt,dum)

      dum <- mean_grid_function(shp_in,tmean,prj)
      
      names(dum)[1]<- 'Tmean'
      dum <- data.frame(Tmean = dum)
      # samm_basins <- filter(Sammamish@data,WTRSHD_NAM == 'Sammamish River')
      dum <- bind_cols(shp_in,dum)
      dum$Year <- yr
      dum$Month <- mo
      dum$shape <- shape
      dum$basin <- substr(shape,15,nchar(shape)-4)
      
      output.tmean <- bind_rows(output.tmean,dum)
      
      print(paste0(shape,' ',yr,' ',mo))
      
    }
  }
}

output.ppt <- left_join(output.ppt,usgs,by="basin")
output.tmean <- left_join(output.tmean,usgs,by="basin")
  
saveRDS(output.ppt, 'output.ppt.rds')
saveRDS(output.tmean, 'output.tmean.rds')
output.ppt <- readRDS('output.ppt.rds')
output.tmean <- readRDS('output.tmean.rds')

saveRDS(output.ppt, 'output.ppt.sacramento.rds')
saveRDS(output.tmean, 'output.tmean.sacramento.rds')
output.ppt <- readRDS('output.ppt.sacramento.rds')
output.tmean <- readRDS('output.tmean.sacramento.rds')

ggplot(output.ppt %>% mutate(Date = as.Date(paste(Year,Month,15,sep="-"))),aes(Date,Prec_in)) +
  geom_line() +
  facet_wrap(~basin)

ggplot(output.tmean %>% mutate(Date = as.Date(paste(Year,Month,15,sep="-"))),aes(Date,Tmean)) +
  geom_line() +
  facet_wrap(~basin)

output.ppt <- output.ppt %>% data.frame() %>% mutate(Season1 = ifelse(Month %in% c(12,1,2),'DJF_ppt',
                                                             ifelse(Month %in% c(3,4,5),'MAM_ppt',
                                                                    ifelse(Month %in% c(6,7,8),"JJA_ppt",
                                                                           ifelse(Month %in% c(9,10,11),'SON_ppt',NA)))),
                                             waterYear1 = ifelse(Month>11,Year + 1, Year),
                                             Season2 = ifelse(Month %in% c(1,2,3),'JFM_ppt',
                                                              ifelse(Month %in% c(4,5,6),'AMJ_ppt',
                                                                     ifelse(Month %in% c(7,8,9),"JAS_ppt",
                                                                            ifelse(Month %in% c(10,11,12),'OND_ppt',NA)))),
                                             waterYear2 = ifelse(Month>9,Year + 1, Year))
output.tmean <- output.tmean %>% data.frame() %>% mutate(Season1 = ifelse(Month %in% c(12,1,2),'DJF_tmean',
                                                                      ifelse(Month %in% c(3,4,5),'MAM_tmean',
                                                                             ifelse(Month %in% c(6,7,8),"JJA_tmean",
                                                                                    ifelse(Month %in% c(9,10,11),'SON_tmean',NA)))),
                                                     waterYear1 = ifelse(Month>11,Year + 1, Year),
                                                     Season2 = ifelse(Month %in% c(1,2,3),'JFM_tmean',
                                                                      ifelse(Month %in% c(4,5,6),'AMJ_tmean',
                                                                             ifelse(Month %in% c(7,8,9),"JAS_tmean",
                                                                                    ifelse(Month %in% c(10,11,12),'OND_tmean',NA)))),
                                                     waterYear2 = ifelse(Month>9,Year + 1, Year))

out.annual <- output %>% group_by(basin,shape,Shape_Area,waterYear2) %>% 
  summarize(Prec_in = sum(Prec_in)) %>% 
  rename(waterYear = waterYear2) %>% 
  ungroup()

ggplot(out.annual %>% filter(between(waterYear,1930,2020)),aes(waterYear,Prec_in)) +
  geom_line() +
  geom_smooth() +
  facet_wrap(~basin)

out.ppt.waterYear1 <- output.ppt %>% group_by(basin,site_no,shape,Shape_Area,waterYear1,Season1) %>% 
  summarize(Prec_in = mean(Prec_in)) %>% 
  ungroup() %>% 
  spread(key = Season1,value = Prec_in)
out.tmean.waterYear1 <- output.tmean %>% group_by(basin,site_no,shape,Shape_Area,waterYear1,Season1) %>% 
  summarize(Tmean = mean(Tmean)) %>% 
  ungroup() %>% 
  spread(key = Season1,value = Tmean)

out.ppt.waterYear2 <- output.ppt %>% group_by(basin,site_no,shape,Shape_Area,waterYear2,Season2) %>% 
  summarize(Prec_in = mean(Prec_in)) %>% 
  ungroup() %>% 
  spread(key = Season2,value = Prec_in)
out.tmean.waterYear2 <- output.tmean %>% group_by(basin,site_no,shape,Shape_Area,waterYear2,Season2) %>% 
  summarize(Tmean = mean(Tmean)) %>% 
  ungroup() %>% 
  spread(key = Season2,value = Tmean)

####################################################################################################################
####################################################################################################################
###
####################################################################################################################
######################################
### get daily flow data
######################################
# 11 long-term gages
staname <- c('Snoqualmie nr Carnation','NF Snoqualmie','Sauk abv White Chuck','NF Stillaguamish',
             'Puyallup at Puyallup','Sauk nr Sauk','Naselle','NF Skokomish',
             # 'Satsop','Wilson','Skykomish','Taylor River nr Selleck','SF Tolt nr Index')
             'Satsop','Skykomish','Queets')
staid <- c('12149000','12142000','12186000','12167000',
           '12101500','12189500','12010000','12056500',
           # '12035000','14301500','12134500')
           '12035000','12134500','12040500')

df <- NULL
PS_drainSqKm <- 0
basin_info <- NULL

for (i in 1:length(staid)){
  
  cat("\nProcessing ", staname[i], "\n")
  Daily <- readNWISDaily(staid[i], "00060")
  INFO<- readNWISInfo(staid[i],'00060',interactive = FALSE)
  eList <- as.egret(INFO,Daily,NA,NA)
  Daily$site_no <- eList$INFO$site_no[1]
  Daily$station_nm <- eList$INFO$station_nm[1]
  Daily$basin <- staname[i]
  
  df <- rbind(df,Daily)
  
  # write out individual gage station objects for use separately in EGRET
  # objname <- paste0(staname[i],'_Daily')
  # assign(objname,Daily)
  # objname <- paste0(staname[i],'_INFO')
  # assign(objname,INFO)
  # objname <- paste0(staname[i],'_eList')
  # assign(objname,eList)
  
  PS_drainSqKm <- PS_drainSqKm + INFO$drainSqKm
  
  basin_info <- bind_rows(INFO,basin_info)
}

###############################################################
### patch in Naselle data
###############################################################
tmp <- readNWISdv('12010000', "00060")
tmp <- renameNWISColumns(tmp)
tmp <- tmp %>% filter(Date>=as.Date("2003-09-30")) %>% transmute(site_no=site_no,Date=Date,Q=Flow/35.3147,Month=month(Date),waterYear=ifelse(month(Date)>9,year(Date)+1,year(Date)),
                                                                 Qualifier=Flow_cd,station_nm='NASELLE RIVER NEAR NASELLE, WA',basin = 'Naselle')
df2 <- filter(df,!(site_no == '12010000'&Date>=as.Date("2003-09-30")))
tmp2 <- filter(df2,site_no == '12010000')
df <- bind_rows(df2,tmp)
###############################################################
# Note there are some substantial data gaps
# NF and SF Snoqualmie
df <- filter(df,!(site_no %in% c('12142000','12143400')&waterYear==1990))

df <- left_join(df,basin_info[,c("site_no","drainSqKm")],by="site_no")

df <- mutate(df, Q7s = (Q7/35.3147*86400)/(drainSqKm*1e6)*1000, Q30s = (Q30/35.3147*86400)/(drainSqKm*1e6)*1000)

######################
### leaving Naselle out for now. Need rolling 7-day mean calc


outputQ <- df %>%  mutate(Season1 = ifelse(Month %in% c(12,1,2),'DJF_Q',
                                                              ifelse(Month %in% c(3,4,5),'MAM_Q',
                                                                     ifelse(Month %in% c(6,7,8),"JJA_Q",
                                                                            ifelse(Month %in% c(9,10,11),'SON_Q',NA)))),
                                             waterYear1 = ifelse(Month>11,trunc(DecYear) + 1, trunc(DecYear)),
                                             Season2 = ifelse(Month %in% c(1,2,3),'JFM_Q',
                                                              ifelse(Month %in% c(4,5,6),'AMJ_Q',
                                                                     ifelse(Month %in% c(7,8,9),"JAS_Q",
                                                                            ifelse(Month %in% c(10,11,12),'OND_Q',NA)))),
                                             waterYear2 = ifelse(Month>9,trunc(DecYear) + 1, trunc(DecYear)))
# convert Q to cfs
outputQ.waterYear1 <- outputQ %>% filter(waterYear1<2024) %>% group_by(site_no,basin,waterYear1,Season1) %>% summarise(pkQ = min(Q7*35.3147,na.rm=F)) %>% 
  ungroup() %>% 
  spread(key = Season1,value=pkQ)

output.waterYear1 <- left_join(outputQ.waterYear1,out.ppt.waterYear1,by = c("site_no","basin","waterYear1")) 
output.waterYear1 <- left_join(output.waterYear1,out.tmean.waterYear1,by = c("site_no","basin","waterYear1","shape","Shape_Area")) 
output.waterYear1 <- filter(output.waterYear1,between(waterYear1,1929,2022))

outputQ.waterYear2 <- outputQ %>% filter(waterYear2<2024) %>% group_by(site_no,basin,waterYear2,Season2) %>% summarise(pkQ = min(Q7*35.3147,na.rm=F)) %>% 
  ungroup() %>% 
  spread(key = Season2,value=pkQ)

output.waterYear2 <- left_join(outputQ.waterYear2,out.ppt.waterYear2,by = c("site_no","basin","waterYear2")) 
output.waterYear2 <- left_join(output.waterYear2,out.tmean.waterYear2,by = c("site_no","basin","waterYear2","shape","Shape_Area")) 
output.waterYear2 <- filter(output.waterYear2,between(waterYear2,1929,2022))

########################################################################################################################
###
# sacramento
# scale.max = 250000
# scale.int = 50000
# min.yr = 1950
# snoqualmie
scale.max = 100000
scale.int = 25000
min.yr = 1930
# skykomish
scale.max = 150000
scale.int = 50000
min.yr = 1930
# NF Stillaguamish
scale.max = 3000
scale.int = 500
min.yr = 1930

########################################################################################################################
tmp <- filter(output.waterYear2, site_no == '12149000') # snoqualmie near Carnation
tmp <- filter(output.waterYear2, site_no == '12134500') # Skykomish
tmp <- filter(output.waterYear2, site_no == '12167000') # NF Stillaguamish
tmp <- filter(output.waterYear2, site_no == '11377100') # Sacramento
tmp <- mutate(tmp, JAS_ppt_lag = dplyr::lag(JAS_ppt, default = NA),  JAS_tmean_lag = dplyr::lag(JAS_tmean, default = NA)) %>% 
  #  filter(between(waterYear2,1949,2019))
   filter(between(waterYear2,1930,2021))

mod <- gamlss(OND_Q ~ waterYear2 + OND_ppt + OND_tmean + JAS_ppt_lag + JAS_tmean_lag, sigma.formula = OND_Q ~ waterYear2 + OND_ppt + OND_tmean + JAS_ppt_lag + JAS_tmean_lag, data = tmp, family = GA)
# mod <- gamlss(pkQ ~ waterYear2, sigma.formula = pkQ ~ waterYear2, data = tmp, family = GA)

### try stepGAIC ##########
mod <- stepGAIC(mod)
mod <- stepGAIC(mod, what = "sigma")
###############################

summary(mod)
plot(mod)
wp(mod)
p <- predict(mod)
p <- data.frame(waterYear2 = tmp$waterYear2, p = exp(p))
ggplot(tmp,aes(waterYear2,OND_Q)) + theme_bw() + geom_point() + geom_line() +
  geom_line(data = p, aes(waterYear2,p), color = 'red') +  geom_point(data = p, aes(waterYear2,p), color = 'red')
params <- predictAll(mod,newdata = tmp)
c0.95 <- qGA(c(0.95),params$mu,params$sigma)
c0.75 <- qGA(c(0.75),params$mu,params$sigma)
c0.5 <- qGA(c(0.5),params$mu,params$sigma)
c0.25 <- qGA(c(0.25),params$mu,params$sigma)
c0.05 <- qGA(c(0.05),params$mu,params$sigma)
OND.params <- params

centiles <- data.frame(waterYear2 = tmp$waterYear2, c0.05 = c0.05, c0.25 = c0.25, c0.5 = c0.5, c0.75 = c0.75, c0.95 = c0.95)
tmp2 <- left_join(tmp,centiles,by="waterYear2")
p2 <- ggplot(tmp2,aes(waterYear2,OND_Q)) + theme_bw() + geom_point(shape = 19, color = "green", size = 2) + # geom_line() +
  geom_line(aes(waterYear2,c0.5), color = 'red', linewidth = 1) +
  # geom_point(aes(waterYear2,c0.5), color = 'red') +  
  # geom_line(aes(waterYear2,c0.95), color = 'blue') +
  # geom_line(aes(waterYear2,c0.05), color = 'blue') +
  geom_ribbon(aes(ymin = c0.25, ymax = c0.75), fill = "blue", alpha = 0.4) +
  geom_ribbon(aes(ymin = c0.05, ymax = c0.95), fill = "lightblue", alpha = 0.4) +
  # labs(x = "Year", y = "Mean daily discharge maxima (cfs)") +
  labs(x = "", y = "7-day average discharge minimum (cfs)") +
  ggtitle("OND") +
  scale_x_continuous(breaks = seq(min.yr,2020,10)) +
  scale_y_continuous(breaks = seq(0,scale.max,scale.int), limits = c(0,scale.max)) 
#########################################################################################################################

mod <- gamlss(JFM_Q ~ waterYear2 + JFM_ppt + JFM_tmean + OND_ppt + OND_tmean, sigma.formula = JFM_Q ~ waterYear2 + JFM_ppt + JFM_tmean + OND_ppt + OND_tmean, data = tmp, family = GA)
# mod <- gamlss(pkQ ~ waterYear2, sigma.formula = pkQ ~ waterYear2, data = tmp, family = GA)

### try stepGAIC ##########
mod <- stepGAIC(mod)
mod <- stepGAIC(mod, what = "sigma")
###############################

summary(mod)
plot(mod)
wp(mod)
p <- predict(mod)
p <- data.frame(waterYear2 = tmp$waterYear2, p = exp(p))
ggplot(tmp,aes(waterYear2,JFM_Q)) + theme_bw() + geom_point() + geom_line() +
  geom_line(data = p, aes(waterYear2,p), color = 'red')

params <- predictAll(mod,newdata = tmp)
c0.95 <- qGA(c(0.95),params$mu,params$sigma)
c0.75 <- qGA(c(0.75),params$mu,params$sigma)
c0.5 <- qGA(c(0.5),params$mu,params$sigma)
c0.25 <- qGA(c(0.25),params$mu,params$sigma)
c0.05 <- qGA(c(0.05),params$mu,params$sigma)
JFM.params <- params

centiles <- data.frame(waterYear2 = tmp$waterYear2, c0.05 = c0.05, c0.25 = c0.25, c0.5 = c0.5, c0.75 = c0.75, c0.95 = c0.95)
tmp2 <- left_join(tmp,centiles,by="waterYear2")
p3 <- ggplot(tmp2,aes(waterYear2,JFM_Q)) + theme_bw() + geom_point(shape = 19, color = "green", size = 2) + # geom_line() +
  geom_line(aes(waterYear2,c0.5), color = 'red', linewidth = 1) +
  # geom_point(aes(waterYear2,c0.5), color = 'red') +  
  # geom_line(aes(waterYear2,c0.95), color = 'blue') +
  # geom_line(aes(waterYear2,c0.05), color = 'blue') +
  geom_ribbon(aes(ymin = c0.25, ymax = c0.75), fill = "blue", alpha = 0.4) +
  geom_ribbon(aes(ymin = c0.05, ymax = c0.95), fill = "lightblue", alpha = 0.4) +
  # labs(x = "Year", y = "7-day average discharge minimum (cfs)") +
  labs(x = "", y = "") +
  ggtitle("JFM") +
  scale_x_continuous(breaks = seq(min.yr,2020,10)) +
  scale_y_continuous(breaks = seq(0,scale.max,scale.int), limits = c(0,scale.max)) 
#################################################################################################################

mod <- gamlss(AMJ_Q ~ waterYear2 + AMJ_ppt + AMJ_tmean + JFM_ppt + JFM_tmean, sigma.formula = AMJ_Q ~ waterYear2 + AMJ_ppt + AMJ_tmean + JFM_ppt + JFM_tmean, data = tmp, family = GA)
# mod <- gamlss(pkQ ~ waterYear2, sigma.formula = pkQ ~ waterYear2, data = tmp, family = GA)

### try stepGAIC ##########
mod <- stepGAIC(mod)
mod <- stepGAIC(mod, what = "sigma")
###############################

summary(mod)
plot(mod)
wp(mod)
p <- predict(mod)
p <- data.frame(waterYear2 = tmp$waterYear2, p = exp(p))
ggplot(tmp,aes(waterYear2,AMJ_Q)) + theme_bw() + geom_point() + geom_line() +
  geom_line(data = p, aes(waterYear2,p), color = 'red')

params <- predictAll(mod,newdata = tmp)
c0.95 <- qGA(c(0.95),params$mu,params$sigma)
c0.75 <- qGA(c(0.75),params$mu,params$sigma)
c0.5 <- qGA(c(0.5),params$mu,params$sigma)
c0.25 <- qGA(c(0.25),params$mu,params$sigma)
c0.05 <- qGA(c(0.05),params$mu,params$sigma)
AMJ.params <- params

centiles <- data.frame(waterYear2 = tmp$waterYear2, c0.05 = c0.05, c0.25 = c0.25, c0.5 = c0.5, c0.75 = c0.75, c0.95 = c0.95)
tmp2 <- left_join(tmp,centiles,by="waterYear2")
p4 <- ggplot(tmp2,aes(waterYear2,AMJ_Q)) + theme_bw() + geom_point(shape = 19, color = "green", size = 2) + # geom_line() +
  geom_line(aes(waterYear2,c0.5), color = 'red', linewidth = 1) +
  # geom_point(aes(waterYear2,c0.5), color = 'red') +  
  # geom_line(aes(waterYear2,c0.95), color = 'blue') +
  # geom_line(aes(waterYear2,c0.05), color = 'blue') +
  geom_ribbon(aes(ymin = c0.25, ymax = c0.75), fill = "blue", alpha = 0.4) +
  geom_ribbon(aes(ymin = c0.05, ymax = c0.95), fill = "lightblue", alpha = 0.4) +
  labs(x = "Year", y = "7-day average discharge minimum (cfs)") +
  # labs(x = "Year", y = "") +
  ggtitle("AMJ") +
  scale_x_continuous(breaks = seq(min.yr,2020,10)) +
  scale_y_continuous(breaks = seq(0,scale.max,scale.int), limits = c(0,scale.max)) 
###########################################################################################################################

mod <- gamlss(JAS_Q ~ waterYear2 + JAS_ppt + JAS_tmean + AMJ_ppt + AMJ_tmean, sigma.formula = JAS_Q ~ waterYear2 + JAS_ppt + JAS_tmean + AMJ_ppt + AMJ_tmean, data = tmp, family = GA)
# mod <- gamlss(pkQ ~ waterYear2, sigma.formula = pkQ ~ waterYear2, data = tmp, family = GA)

### try stepGAIC ##########
mod <- stepGAIC(mod)
mod <- stepGAIC(mod, what = "sigma")
###############################

summary(mod)
plot(mod)
wp(mod)
p <- predict(mod)
p <- data.frame(waterYear2 = tmp$waterYear2, p = exp(p))
ggplot(tmp,aes(waterYear2,JAS_Q)) + theme_bw() + geom_point() + geom_line() +
  geom_line(data = p, aes(waterYear2,p), color = 'red')

params <- predictAll(mod,newdata = tmp)
c0.95 <- qGA(c(0.95),params$mu,params$sigma)
c0.75 <- qGA(c(0.75),params$mu,params$sigma)
c0.5 <- qGA(c(0.5),params$mu,params$sigma)
c0.25 <- qGA(c(0.25),params$mu,params$sigma)
c0.05 <- qGA(c(0.05),params$mu,params$sigma)
JAS.params <- params

centiles <- data.frame(waterYear2 = tmp$waterYear2, c0.05 = c0.05, c0.25 = c0.25, c0.5 = c0.5, c0.75 = c0.75, c0.95 = c0.95)
tmp2 <- left_join(tmp,centiles,by="waterYear2")
p1 <- ggplot(tmp2,aes(waterYear2,JAS_Q)) + theme_bw() + geom_point(shape = 19, color = "green", size = 2) + # geom_line() +
  geom_line(aes(waterYear2,c0.5), color = 'red', linewidth = 1) +
  # geom_point(aes(waterYear2,c0.5), color = 'red') +  
  # geom_line(aes(waterYear2,c0.95), color = 'blue') +
  # geom_line(aes(waterYear2,c0.05), color = 'blue') +
  geom_ribbon(aes(ymin = c0.25, ymax = c0.75), fill = "blue", alpha = 0.4) +
  geom_ribbon(aes(ymin = c0.05, ymax = c0.95), fill = "lightblue", alpha = 0.4) +
  # labs(x = "Year", y = "Mean daily discharge maxima (cfs)") +
  # labs(x = "", y = "Mean daily discharge maxima (cfs)") +
  labs(x = "Year", y = "") +
  ggtitle("JAS") +
  scale_x_continuous(breaks = seq(min.yr,2020,10)) +
  scale_y_continuous(breaks = seq(0,scale.max,scale.int), limits = c(0,scale.max)) 

# plot seasonal observations and predictions
# (p1 + p2)/(p3 + p4)
(p2 + p3)/(p4 + p1)


#################################################################################################################################
### find annual maximum across seasons and plot
pks <- tmp %>% group_by(waterYear2) %>% summarize(Q = min(OND_Q,JFM_Q,AMJ_Q,JAS_Q)) %>% ungroup()
ggplot(pks,aes(waterYear2,Q)) + theme_bw() + geom_point(shape = 19, color = "green", size = 2) 

#########################################################################################################
# 
# iter = 10000
iter = 1000
pred.pks <- NULL
pks.annual <- NULL
for (i in 1:iter){
  print(paste0('iteration = ', i))
  # for (j in 1:length(tmp$waterYear2)){
    # OND
    dum <- rGA(length(tmp$waterYear2),mu=OND.params$mu,sigma=OND.params$sigma) 
    dum <- data.frame(iter = i, Season = 'OND', waterYear2 = tmp$waterYear2, Q = dum)
    pred.pks <- bind_rows(pred.pks,dum)
    # JFM
    dum <- rGA(length(tmp$waterYear2),mu=JFM.params$mu,sigma=JFM.params$sigma) 
    dum <- data.frame(iter = i, Season = 'JFM', waterYear2 = tmp$waterYear2, Q = dum)
    pred.pks <- bind_rows(pred.pks,dum)
    # AMJ
    dum <- rGA(length(tmp$waterYear2),mu=AMJ.params$mu,sigma=AMJ.params$sigma) 
    dum <- data.frame(iter = i, Season = 'AMJ', waterYear2 = tmp$waterYear2, Q = dum)
    pred.pks <- bind_rows(pred.pks,dum)
    # JAS
    dum <- rGA(length(tmp$waterYear2),mu=JAS.params$mu,sigma=JAS.params$sigma) 
    dum <- data.frame(iter = i, Season = 'JAS', waterYear2 = tmp$waterYear2, Q = dum)
    pred.pks <- bind_rows(pred.pks,dum)
    
  # }
  
  suppressMessages(dum <- pred.pks %>% group_by(iter,waterYear2) %>% summarize(Q = min(Q)) %>% ungroup())
  pks.annual <- bind_rows(pks.annual,dum)
  pred.pks <- NULL
  
}
rm(dum,pred.pks)
#########################################################################################################
pks.annual.centiles <- pks.annual %>% group_by(waterYear2) %>% summarize(c0.05 = quantile(Q, probs = c(0.05)),
                                                                         c0.25 = quantile(Q, probs = c(0.25)),
                                                                         c0.5 = quantile(Q, probs = c(0.50)),
                                                                         c0.75 = quantile(Q, probs = c(0.75)),
                                                                         c0.95 = quantile(Q, probs = c(0.95)),
                                                                         pQ = mean(Q))
pks <- left_join(pks,pks.annual.centiles,by="waterYear2")
#########################################################################################################

ggplot(pks,aes(waterYear2,Q)) + theme_bw() + geom_point(shape = 19, color = "green", size = 2) +
  geom_point(data = pks.annual, aes(waterYear2,Q)) +
  geom_line(data = pks.annual, aes(waterYear2,Q)) +
  geom_line(aes(waterYear2,Q),color = "green") 

p5 <- ggplot(pks,aes(waterYear2,Q)) + theme_bw() + geom_point(shape = 19, color = "green", size = 2) + 
  geom_line(aes(waterYear2,c0.5), color = 'red', linewidth = 1) +
  # geom_point(aes(waterYear2,c0.5), color = 'red') +  
  # geom_line(aes(waterYear2,c0.95), color = 'blue') +
  # geom_line(aes(waterYear2,c0.05), color = 'blue') +
  geom_ribbon(aes(ymin = c0.25, ymax = c0.75), fill = "blue", alpha = 0.4) +
  geom_ribbon(aes(ymin = c0.05, ymax = c0.95), fill = "lightblue", alpha = 0.4) +
  # labs(x = "Year", y = "Mean daily discharge maxima (cfs)") +
  labs(x = "", y = "7-day average discharge minimum (cfs)") +
  ggtitle("Annual") +
  scale_x_continuous(breaks = seq(min.yr,2020,10)) +
  scale_y_continuous(breaks = seq(0,scale.max+25000,scale.int), limits = c(0,scale.max)) 

# plot seasonal observations and predictions
# (p1 + p2)/(p3 + p4)/p5
(p2 + p3)/(p4 + p1)/p5

p7 <- ggplot(pks,aes(waterYear2,Q)) + theme_bw() + geom_point(shape = 19, color = "green", size = 2) + 
  geom_line(aes(waterYear2,pQ), color = 'red', linewidth = 1) +
  # geom_point(aes(waterYear2,pQ), color = 'red', size = 2) +  
  # geom_point(aes(waterYear2,c0.5), color = 'red') +  
  # geom_line(aes(waterYear2,c0.95), color = 'blue') +
  # geom_line(aes(waterYear2,c0.05), color = 'blue') +
  geom_ribbon(aes(ymin = c0.25, ymax = c0.75), fill = "blue", alpha = 0.4) +
  geom_ribbon(aes(ymin = c0.05, ymax = c0.95), fill = "lightblue", alpha = 0.4) +

    # labs(x = "Year", y = "Mean daily discharge maxima (cfs)") +
  labs(x = "", y = "7-day average discharge minimum (cfs)") +
  ggtitle("Annual") +
  scale_x_continuous(breaks = seq(min.yr,2020,10)) +
  scale_y_continuous(breaks = seq(0,600,100), limits = c(0,600))   
################################################################################################################################
pdf.plot(mod)
histDist(mod$y, "GA")
# https://stackoverflow.com/questions/75200988/r-gamlss-prediction-of-z-scores-using-regression-with-multiple-explanatory-vari
params <- predictAll(mod,newdata = tmp)
c0.95 <- qGA(c(0.95),params$mu,params$sigma)
c0.75 <- qGA(c(0.75),params$mu,params$sigma)
c0.5 <- qGA(c(0.5),params$mu,params$sigma)
c0.25 <- qGA(c(0.25),params$mu,params$sigma)
c0.05 <- qGA(c(0.05),params$mu,params$sigma)
#################################################################################################################################
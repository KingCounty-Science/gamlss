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

##############################################################################################################
# shape files created by streamstats_app.R

## 
file_list <- list.files("./rivers2_shp", pattern = "*mento.shp", full.names = TRUE)
########################################################################
########################################################################
### winter precipitation Oct-Mar 
### use this in conjuction with prism_get.R 
### and streamstats_app.R
### to delineate basins and export delineation to /rivers2_shp and
### to get and process your own precipitation data
########################################################################
# prism_set_dl_dir('C:/prismtmp')
# 
# # function to extract mean annual precipitation
# mean_grid_function<-function(shp_in,rasin,prj){ # the input should be the shapefile
#   # shp<-shapefile(name) #convert to shapefile object
#   shp_in<-st_transform(shp_in,st_crs(rasin)) #reproject shape
#   pavg <- raster::extract(rasin,shp_in,fun=mean, na.rm = TRUE) #get spatially averaged rainfall
#   return(pavg)
# }
# 
# output.ppt <- NULL
# output.tmean <- NULL
# for(shape in file_list){
#   shp_in <- st_read(shape)
#   
#   for(yr in 1929:2021){
#     for (mo in 1:12){
#       
#       ppt <- raster(pd_to_file(prism_archive_subset("ppt", "monthly", years = yr, mon = mo)))
#       tmean <- raster(pd_to_file(prism_archive_subset("tmean", "monthly", years = yr, mon = mo)))
#       prj<-toString(crs(ppt)) #get raster projection
#       dum <- mean_grid_function(shp_in,ppt,prj)
#       
#       dum <- dum/25.4
#       names(dum)[1]<- 'Prec_in'
#       dum <- data.frame(Prec_in = dum)
#       # samm_basins <- filter(Sammamish@data,WTRSHD_NAM == 'Sammamish River')
#       dum <- bind_cols(shp_in,dum)
#       dum$Year <- yr
#       dum$Month <- mo
#       dum$shape <- shape
#       dum$basin <- substr(shape,15,nchar(shape)-4)
#       
#       output.ppt <- bind_rows(output.ppt,dum)
# 
#       dum <- mean_grid_function(shp_in,tmean,prj)
#       
#       names(dum)[1]<- 'Tmean'
#       dum <- data.frame(Tmean = dum)
#       # samm_basins <- filter(Sammamish@data,WTRSHD_NAM == 'Sammamish River')
#       dum <- bind_cols(shp_in,dum)
#       dum$Year <- yr
#       dum$Month <- mo
#       dum$shape <- shape
#       dum$basin <- substr(shape,15,nchar(shape)-4)
#       
#       output.tmean <- bind_rows(output.tmean,dum)
#       
#       print(paste0(shape,' ',yr,' ',mo))
#       
#     }
#   }
# }

# output.ppt <- left_join(output.ppt,usgs,by="basin")
# output.tmean <- left_join(output.tmean,usgs,by="basin")

# saveRDS(output.ppt, './data/processed/output.ppt.sacramento.rds')
# saveRDS(output.tmean, './data/processed/output.tmean.sacramento.rds')
output.ppt <- readRDS('./data/processed/output.ppt.sacramento.rds')
output.tmean <- readRDS('./data/processed/output.tmean.sacramento.rds')

ggplot(output.ppt %>% mutate(Date = as.Date(paste(Year,Month,15,sep="-"))),aes(Date,Prec_in)) +
  geom_line() +
  facet_wrap(~basin)

ggplot(output.tmean %>% mutate(Date = as.Date(paste(Year,Month,15,sep="-"))),aes(Date,Tmean)) +
  geom_line() +
  facet_wrap(~basin)

# as in Kim and Villarini (2023)
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

out.ppt.waterYear1 <- output.ppt %>% group_by(basin,site_no,shape,Shape_Area,waterYear1,Season1) %>% 
  summarize(Prec_in = mean(Prec_in)) %>% 
  ungroup() %>% 
  spread(key = Season1,value = Prec_in)
out.tmean.waterYear1 <- output.tmean %>% group_by(basin,site_no,shape,Shape_Area,waterYear1,Season1) %>% 
  summarize(Tmean = mean(Tmean)) %>% 
  ungroup() %>% 
  spread(key = Season1,value = Tmean)

####################################################################################################################
####################################################################################################################
### Sacramento River Above Bend Bridge Near Red Bluff, California
####################################################################################################################
######################################
### get daily flow data
######################################
staid <- c('11377100')
staname <- c('Sacramento')
usgs <- data.frame(site_no = staid, basin = staname)

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

df <- left_join(df,basin_info[,c("site_no","drainSqKm")],by="site_no")

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
outputQ.waterYear1 <- outputQ %>% filter(waterYear1<2024) %>% group_by(site_no,basin,waterYear1,Season1) %>% summarise(pkQ = max(Q*35.3147,na.rm=T)) %>% 
  ungroup() %>% 
  spread(key = Season1,value=pkQ)

output.waterYear1 <- left_join(outputQ.waterYear1,out.ppt.waterYear1,by = c("site_no","basin","waterYear1")) 
output.waterYear1 <- left_join(output.waterYear1,out.tmean.waterYear1,by = c("site_no","basin","waterYear1","shape","Shape_Area")) 
output.waterYear1 <- filter(output.waterYear1,between(waterYear1,1929,2022))
########################################################################################################################
###
# sacramento
 scale.max = 250000
 scale.int = 50000
 min.yr = 1950

########################################################################################################################
tmp <- filter(output.waterYear1, site_no == '11377100') # Sacramento
tmp <- mutate(tmp, SON_ppt_lag = dplyr::lag(SON_ppt, default = NA),  SON_tmean_lag = dplyr::lag(SON_tmean, default = NA)) %>% 
    filter(between(waterYear1,1949,2019))

mod <- gamlss(DJF_Q ~ waterYear1 + DJF_ppt + DJF_tmean + SON_ppt_lag + SON_tmean_lag, sigma.formula = DJF_Q ~ waterYear1 + DJF_ppt + DJF_tmean + SON_ppt_lag + SON_tmean_lag, data = tmp, family = GA)
# mod <- gamlss(pkQ ~ waterYear1, sigma.formula = pkQ ~ waterYear1, data = tmp, family = GA)

### try stepGAIC ##########
mod <- stepGAIC(mod)
mod <- stepGAIC(mod, what = "sigma")
###############################

summary(mod)
plot(mod)
wp(mod)
p <- predict(mod)
p <- data.frame(waterYear1 = tmp$waterYear1, p = exp(p))
ggplot(tmp,aes(waterYear1,DJF_Q)) + theme_bw() + geom_point() + geom_line() +
  geom_line(data = p, aes(waterYear1,p), color = 'red') +  geom_point(data = p, aes(waterYear1,p), color = 'red')
params <- predictAll(mod,newdata = tmp)
c0.95 <- qGA(c(0.95),params$mu,params$sigma)
c0.75 <- qGA(c(0.75),params$mu,params$sigma)
c0.5 <- qGA(c(0.5),params$mu,params$sigma)
c0.25 <- qGA(c(0.25),params$mu,params$sigma)
c0.05 <- qGA(c(0.05),params$mu,params$sigma)
DJF.params <- params

centiles <- data.frame(waterYear1 = tmp$waterYear1, c0.05 = c0.05, c0.25 = c0.25, c0.5 = c0.5, c0.75 = c0.75, c0.95 = c0.95)
tmp2 <- left_join(tmp,centiles,by="waterYear1")
p2 <- ggplot(tmp2,aes(waterYear1,DJF_Q)) + theme_bw() + geom_point(shape = 19, color = "green", size = 2) + # geom_line() +
  geom_line(aes(waterYear1,c0.5), color = 'red', linewidth = 1) +
  # geom_point(aes(waterYear1,c0.5), color = 'red') +  
  # geom_line(aes(waterYear1,c0.95), color = 'blue') +
  # geom_line(aes(waterYear1,c0.05), color = 'blue') +
  geom_ribbon(aes(ymin = c0.25, ymax = c0.75), fill = "blue", alpha = 0.4) +
  geom_ribbon(aes(ymin = c0.05, ymax = c0.95), fill = "lightblue", alpha = 0.4) +
  # labs(x = "Year", y = "Mean daily discharge maxima (cfs)") +
  labs(x = "", y = "") +
  ggtitle("DJF") +
  scale_x_continuous(breaks = seq(min.yr,2020,10)) +
  scale_y_continuous(breaks = seq(0,scale.max,scale.int), limits = c(0,scale.max)) 
#########################################################################################################################

mod <- gamlss(MAM_Q ~ waterYear1 + MAM_ppt + MAM_tmean + DJF_ppt + DJF_tmean, sigma.formula = MAM_Q ~ waterYear1 + MAM_ppt + MAM_tmean + DJF_ppt + DJF_tmean, data = tmp, family = GA)
# mod <- gamlss(pkQ ~ waterYear1, sigma.formula = pkQ ~ waterYear1, data = tmp, family = GA)

### try stepGAIC ##########
mod <- stepGAIC(mod)
mod <- stepGAIC(mod, what = "sigma")
###############################

summary(mod)
plot(mod)
wp(mod)
p <- predict(mod)
p <- data.frame(waterYear1 = tmp$waterYear1, p = exp(p))
ggplot(tmp,aes(waterYear1,MAM_Q)) + theme_bw() + geom_point() + geom_line() +
  geom_line(data = p, aes(waterYear1,p), color = 'red')

params <- predictAll(mod,newdata = tmp)
c0.95 <- qGA(c(0.95),params$mu,params$sigma)
c0.75 <- qGA(c(0.75),params$mu,params$sigma)
c0.5 <- qGA(c(0.5),params$mu,params$sigma)
c0.25 <- qGA(c(0.25),params$mu,params$sigma)
c0.05 <- qGA(c(0.05),params$mu,params$sigma)
MAM.params <- params

centiles <- data.frame(waterYear1 = tmp$waterYear1, c0.05 = c0.05, c0.25 = c0.25, c0.5 = c0.5, c0.75 = c0.75, c0.95 = c0.95)
tmp2 <- left_join(tmp,centiles,by="waterYear1")
p3 <- ggplot(tmp2,aes(waterYear1,MAM_Q)) + theme_bw() + geom_point(shape = 19, color = "green", size = 2) + # geom_line() +
  geom_line(aes(waterYear1,c0.5), color = 'red', linewidth = 1) +
  # geom_point(aes(waterYear1,c0.5), color = 'red') +  
  # geom_line(aes(waterYear1,c0.95), color = 'blue') +
  # geom_line(aes(waterYear1,c0.05), color = 'blue') +
  geom_ribbon(aes(ymin = c0.25, ymax = c0.75), fill = "blue", alpha = 0.4) +
  geom_ribbon(aes(ymin = c0.05, ymax = c0.95), fill = "lightblue", alpha = 0.4) +
  labs(x = "Year", y = "Mean daily discharge maxima (cfs)") +
  ggtitle("MAM") +
  scale_x_continuous(breaks = seq(min.yr,2020,10)) +
  scale_y_continuous(breaks = seq(0,scale.max,scale.int), limits = c(0,scale.max)) 
#################################################################################################################

mod <- gamlss(JJA_Q ~ waterYear1 + JJA_ppt + JJA_tmean + MAM_ppt + MAM_tmean, sigma.formula = JJA_Q ~ waterYear1 + JJA_ppt + JJA_tmean + MAM_ppt + MAM_tmean, data = tmp, family = GA)
# mod <- gamlss(pkQ ~ waterYear1, sigma.formula = pkQ ~ waterYear1, data = tmp, family = GA)

### try stepGAIC ##########
mod <- stepGAIC(mod)
mod <- stepGAIC(mod, what = "sigma")
###############################

summary(mod)
plot(mod)
wp(mod)
p <- predict(mod)
p <- data.frame(waterYear1 = tmp$waterYear1, p = exp(p))
ggplot(tmp,aes(waterYear1,JJA_Q)) + theme_bw() + geom_point() + geom_line() +
  geom_line(data = p, aes(waterYear1,p), color = 'red')

params <- predictAll(mod,newdata = tmp)
c0.95 <- qGA(c(0.95),params$mu,params$sigma)
c0.75 <- qGA(c(0.75),params$mu,params$sigma)
c0.5 <- qGA(c(0.5),params$mu,params$sigma)
c0.25 <- qGA(c(0.25),params$mu,params$sigma)
c0.05 <- qGA(c(0.05),params$mu,params$sigma)
JJA.params <- params

centiles <- data.frame(waterYear1 = tmp$waterYear1, c0.05 = c0.05, c0.25 = c0.25, c0.5 = c0.5, c0.75 = c0.75, c0.95 = c0.95)
tmp2 <- left_join(tmp,centiles,by="waterYear1")
p4 <- ggplot(tmp2,aes(waterYear1,JJA_Q)) + theme_bw() + geom_point(shape = 19, color = "green", size = 2) + # geom_line() +
  geom_line(aes(waterYear1,c0.5), color = 'red', linewidth = 1) +
  # geom_point(aes(waterYear1,c0.5), color = 'red') +  
  # geom_line(aes(waterYear1,c0.95), color = 'blue') +
  # geom_line(aes(waterYear1,c0.05), color = 'blue') +
  geom_ribbon(aes(ymin = c0.25, ymax = c0.75), fill = "blue", alpha = 0.4) +
  geom_ribbon(aes(ymin = c0.05, ymax = c0.95), fill = "lightblue", alpha = 0.4) +
  # labs(x = "Year", y = "Mean daily discharge maxima (cfs)") +
  labs(x = "Year", y = "") +
  ggtitle("JJA") +
  scale_x_continuous(breaks = seq(min.yr,2020,10)) +
  scale_y_continuous(breaks = seq(0,scale.max,scale.int), limits = c(0,scale.max)) 
###########################################################################################################################

mod <- gamlss(SON_Q ~ waterYear1 + SON_ppt + SON_tmean + JJA_ppt + JJA_tmean, sigma.formula = SON_Q ~ waterYear1 + SON_ppt + SON_tmean + JJA_ppt + JJA_tmean, data = tmp, family = GA)
# mod <- gamlss(pkQ ~ waterYear1, sigma.formula = pkQ ~ waterYear1, data = tmp, family = GA)

### try stepGAIC ##########
mod <- stepGAIC(mod)
mod <- stepGAIC(mod, what = "sigma")
###############################

summary(mod)
plot(mod)
wp(mod)
p <- predict(mod)
p <- data.frame(waterYear1 = tmp$waterYear1, p = exp(p))
ggplot(tmp,aes(waterYear1,SON_Q)) + theme_bw() + geom_point() + geom_line() +
  geom_line(data = p, aes(waterYear1,p), color = 'red')

params <- predictAll(mod,newdata = tmp)
c0.95 <- qGA(c(0.95),params$mu,params$sigma)
c0.75 <- qGA(c(0.75),params$mu,params$sigma)
c0.5 <- qGA(c(0.5),params$mu,params$sigma)
c0.25 <- qGA(c(0.25),params$mu,params$sigma)
c0.05 <- qGA(c(0.05),params$mu,params$sigma)
SON.params <- params

centiles <- data.frame(waterYear1 = tmp$waterYear1, c0.05 = c0.05, c0.25 = c0.25, c0.5 = c0.5, c0.75 = c0.75, c0.95 = c0.95)
tmp2 <- left_join(tmp,centiles,by="waterYear1")
p1 <- ggplot(tmp2,aes(waterYear1,SON_Q)) + theme_bw() + geom_point(shape = 19, color = "green", size = 2) + # geom_line() +
  geom_line(aes(waterYear1,c0.5), color = 'red', linewidth = 1) +
  # geom_point(aes(waterYear1,c0.5), color = 'red') +  
  # geom_line(aes(waterYear1,c0.95), color = 'blue') +
  # geom_line(aes(waterYear1,c0.05), color = 'blue') +
  geom_ribbon(aes(ymin = c0.25, ymax = c0.75), fill = "blue", alpha = 0.4) +
  geom_ribbon(aes(ymin = c0.05, ymax = c0.95), fill = "lightblue", alpha = 0.4) +
  # labs(x = "Year", y = "Mean daily discharge maxima (cfs)") +
  labs(x = "", y = "Mean daily discharge maxima (cfs)") +
  ggtitle("SON") +
  scale_x_continuous(breaks = seq(min.yr,2020,10)) +
  scale_y_continuous(breaks = seq(0,scale.max,scale.int), limits = c(0,scale.max)) 

# plot seasonal observations and predictions
(p1 + p2)/(p3 + p4)


0#################################################################################################################################
### find annual maximum across seasons and plot
pks <- tmp %>% group_by(waterYear1) %>% summarize(Q = max(DJF_Q,MAM_Q,JJA_Q,SON_Q)) %>% ungroup()
ggplot(pks,aes(waterYear1,Q)) + theme_bw() + geom_point(shape = 19, color = "green", size = 2) 

#########################################################################################################
# 
# iter = 10000
iter = 1000
pred.pks <- NULL
pks.annual <- NULL
for (i in 1:iter){
  print(paste0('iteration = ', i))
  # for (j in 1:length(tmp$waterYear1)){
    # DJF
    dum <- rGA(length(tmp$waterYear1),mu=DJF.params$mu,sigma=DJF.params$sigma) 
    dum <- data.frame(iter = i, Season = 'DJF', waterYear1 = tmp$waterYear1, Q = dum)
    pred.pks <- bind_rows(pred.pks,dum)
    # MAM
    dum <- rGA(length(tmp$waterYear1),mu=MAM.params$mu,sigma=MAM.params$sigma) 
    dum <- data.frame(iter = i, Season = 'MAM', waterYear1 = tmp$waterYear1, Q = dum)
    pred.pks <- bind_rows(pred.pks,dum)
    # JJA
    dum <- rGA(length(tmp$waterYear1),mu=JJA.params$mu,sigma=JJA.params$sigma) 
    dum <- data.frame(iter = i, Season = 'JJA', waterYear1 = tmp$waterYear1, Q = dum)
    pred.pks <- bind_rows(pred.pks,dum)
    # SON
    dum <- rGA(length(tmp$waterYear1),mu=SON.params$mu,sigma=SON.params$sigma) 
    dum <- data.frame(iter = i, Season = 'SON', waterYear1 = tmp$waterYear1, Q = dum)
    pred.pks <- bind_rows(pred.pks,dum)
    
  # }
  
  suppressMessages(dum <- pred.pks %>% group_by(iter,waterYear1) %>% summarize(Q = max(Q)) %>% ungroup())
  pks.annual <- bind_rows(pks.annual,dum)
  pred.pks <- NULL
  
} 
rm(dum,pred.pks)
#########################################################################################################
pks.annual.centiles <- pks.annual %>% group_by(waterYear1) %>% summarize(c0.05 = quantile(Q, probs = c(0.05)),
                                                                         c0.25 = quantile(Q, probs = c(0.25)),
                                                                         c0.5 = quantile(Q, probs = c(0.50)),
                                                                         c0.75 = quantile(Q, probs = c(0.75)),
                                                                         c0.95 = quantile(Q, probs = c(0.95)))
pks <- left_join(pks,pks.annual.centiles,by="waterYear1")
#########################################################################################################

ggplot(pks,aes(waterYear1,Q)) + theme_bw() + geom_point(shape = 19, color = "green", size = 2) +
  geom_point(data = pks.annual, aes(waterYear1,Q)) +
  geom_line(data = pks.annual, aes(waterYear1,Q)) +
  geom_line(aes(waterYear1,Q),color = "green") 

p5 <- ggplot(pks,aes(waterYear1,Q)) + theme_bw() + geom_point(shape = 19, color = "green", size = 2) + 
  geom_line(aes(waterYear1,c0.5), color = 'red', linewidth = 1) +
  # geom_point(aes(waterYear1,c0.5), color = 'red') +  
  # geom_line(aes(waterYear1,c0.95), color = 'blue') +
  # geom_line(aes(waterYear1,c0.05), color = 'blue') +
  geom_ribbon(aes(ymin = c0.25, ymax = c0.75), fill = "blue", alpha = 0.4) +
  geom_ribbon(aes(ymin = c0.05, ymax = c0.95), fill = "lightblue", alpha = 0.4) +
  # labs(x = "Year", y = "Mean daily discharge maxima (cfs)") +
  labs(x = "", y = "Mean daily discharge maxima (cfs)") +
  ggtitle("Annual") +
  scale_x_continuous(breaks = seq(min.yr,2020,10)) +
  scale_y_continuous(breaks = seq(0,scale.max+25000,scale.int), limits = c(0,scale.max)) 

# plot seasonal observations and predictions
(p1 + p2)/(p3 + p4)/p5


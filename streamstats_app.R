library(EGRET)
library(dataRetrieval)
library(tidyverse)
library(lubridate)

library(sf)
library(mapview)

# devtools::install_github("markwh/streamstats")
library(streamstats)

setTimeout(600)

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

staid <- c('11377100')
staname <- c('Sacramento')

df <- NULL
basin_info <- NULL

for (i in 1:length(staid)){
  
  INFO<- readNWISInfo(staid[i],'00060',interactive = FALSE)
  INFO$staname <- staname[i]
  basin_info <- bind_rows(INFO,basin_info)
  
}
basin_info$state <- 'WA'

# fix Queets
basin_info$dec_long_va <- with(basin_info,ifelse(staname=="Queets",-124.3147,dec_long_va))

# basin_info <- as.data.frame(basin_info)

parms <- NULL
stats <- NULL

for (bsn in unique(basin_info$staname)) {
  
  tmp <- filter(basin_info,basin_info$staname==bsn)
  ws1 <- delineateWatershed(xlocation = tmp$dec_long_va[1], ylocation = tmp$dec_lat_va[1], rcode = tmp$state[1], crs = 4326, 
                          includeparameters = "true", includeflowtypes = "true")
  leafletWatershed(ws1)
  
  chars1 <- computeChars(workspaceID = ws1$workspaceID, rcode = "WA")
  chars1$parameters$staname <- bsn
  chars1$parameters$site_no <- tmp$site_no
  parms <- bind_rows(parms,chars1$parameters)
  
  stats1 <- computeFlowStats(workspaceID = ws1$workspaceID, rcode = "WA", simplify = TRUE)
  stats1$site_no <- tmp$site_no
  stats1$staname <- bsn
  
  stats <- bind_rows(stats,stats1)
  
  writeShapefile(watershed = ws1, layer = bsn, dir = "rivers2_shp", what = "boundary")
  
}

saveRDS(parms,'streamstats_parms_20220719.rds')
saveRDS(stats,'streamstats_Q_stats_20220719.rds')

file_list <- list.files("./rivers2_shp", pattern = "*.shp", full.names = TRUE)
shapefile_list <- lapply(file_list, read_sf)

all_basins <- rbind(shapefile_list[[1]], shapefile_list[[2]], shapefile_list[[3]], shapefile_list[[4]])
all_basins <- bind_rows(shapefile_list)
plot(all_basins['Shape_Area'])

mapview(all_basins)


################################### testing, testing, testing ################################################################################

ws1 <- delineateWatershed(xlocation = basin_info$dec_long_va[4], ylocation = basin_info$dec_lat_va[4], rcode = basin_info$state[4], crs = 4326, 
                          includeparameters = "true", includeflowtypes = "true")

ws1 <- delineateWatershed(xlocation = -124.3147, ylocation = 47.53786, crs = 4326, rcode = 'WA', 
                          includeparameters = "true", includeflowtypes = "true")
leafletWatershed(ws1)
chars1 <- computeChars(workspaceID = ws1$workspaceID, rcode = "WA")
chars1$parameters
parms <- chars1$parameters

### example from github

ws1 <- delineateWatershed(xlocation = -72.9249, ylocation = 42.3170, crs = 4326, 
                          includeparameters = "true", includeflowtypes = "true")
leafletWatershed(ws1)
writeShapefile(watershed = ws1, layer = "layer_name", dir = "ws1_shp", what = "boundary")

chars1 <- computeChars(workspaceID = ws1$workspaceID, rcode = "MA")
chars1$parameters

stats1 <- computeFlowStats(workspaceID = ws1$workspaceID, rcode = "MA", simplify = TRUE)

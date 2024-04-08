####
# https://rpubs.com/collnell/get_prism
#
# https://cran.r-project.org/web/packages/prism/vignettes/prism.html
#
# get prism precipitation and temperature data and process into watershed mean values
#########################################################################################

library(devtools) #needed to download prism from github
library(reshape2) ##melting dataframes
library(dplyr) #data wrangling
library(raster) ##working with raster data
library(sp) ##manipulationg spatial data
library(rgdal)

library(prism)

library(leaflet)

library(tidyverse)
# -- Conflicts ------------------------------------------ tidyverse_conflicts() --
#   x tidyr::extract() masks raster::extract()
# x dplyr::filter()  masks stats::filter()
# x dplyr::lag()     masks stats::lag()
# x raster::select() masks dplyr::select()


prism_set_dl_dir("C:/Users/degaspec/OneDrive - King County/R/prism/prismtmp")

prism_set_dl_dir('C:/prismtmp')
###############################################################################
### get prism monthly precip 1980:2020 (zipped)
###############################################################################
# get_prism_normals(type = 'ppt', resolution = '4km', mon = 1:12, keepZip = FALSE)
get_prism_normals(type = 'ppt', resolution = '4km', mon = 12, keepZip = FALSE)
# get_prism_annual(type = 'ppt', years = 1980:2020, keepZip = FALSE, keep_pre81_months = FALSE)
# get_prism_normals(type = 'ppt', resolution = '4km', mon = NULL, annual = TRUE, keepZip = FALSE)
get_prism_monthlys(type = "ppt", years = 1980:2020, mon = 1:12, keepZip = FALSE)

get_prism_monthlys(type = "ppt", years = 1929:1980, mon = 1:12, keepZip = FALSE)
get_prism_monthlys(type = "ppt", years = 2020:2023, mon = 1:12, keepZip = FALSE)
get_prism_monthlys(type = "tmean", years = 1929:2023, mon = 1:12, keepZip = FALSE)
################################################################################
### explore, find data in archive
################################################################################

# current archive file list
prism_archive_ls()

# returns absolute file paths in archive for raster package
pd_to_file(prism_archive_ls())

# prism file product names
pd_get_name(prism_archive_ls())

# search for specific parameters, time steps, days, months, years, or ranges of days, months, years
prism_archive_subset("ppt","monthly",mon=6:9)

################################################################################
### quick raster plot
################################################################################
ppt_202002 <- prism_archive_subset(
  "ppt", "monthly",mon=2,years=2020
)
pd_image(ppt_202002)

ppt_201702 <- prism_archive_subset(
  "ppt", "monthly",mon=2,years=2017
)
pd_image(ppt_201702)

################################################################################
### get ppt normals and create anomaly plot for ppt Febuary 2017
################################################################################
# https://cran.r-project.org/web/packages/prism/vignettes/prism.html

norm_ppt_02 <- prism_archive_subset(
  "ppt", "monthly normals", mon = 2, resolution = "4km"
)
ppt_201702 <- prism_archive_subset("ppt", "monthly", years = 2017, mon = 2)

# raster needs a full path, not the "short" prism data name
norm_ppt_02 <- pd_to_file(norm_ppt_02)
ppt_201702 <- pd_to_file(ppt_201702)

## Now we'll load the rasters.
norm_ppt_02_rast <- raster(norm_ppt_02)
ppt_201702_rast <- raster(ppt_201702)

# Now we can do simple subtraction to get the anomaly by subtracting 2014 
# from the 30 year normal map
anomCalc <- function(x, y) {
  return(x - y)
}

anom_rast <- raster::overlay(ppt_201702_rast,norm_ppt_02_rast,fun = anomCalc)

plot(anom_rast)

################################################################################
### Single grid cell plot - SeaTac Airport
################################################################################
# https://cran.r-project.org/web/packages/prism/vignettes/prism.html

# data already exist in the prism dl dir
seatac <- c(-122.3, 47.44)

# prism_archive_subset() will return prism data that matches the specified 
# variable, time step, years, months, days, etc.
to_slice <- prism_archive_subset("ppt", "monthly", mon = 1)
p <- pd_plot_slice(to_slice, seatac)

# add a linear average and title
p + 
  stat_smooth(method="lm", se = FALSE) + 
  theme_bw() + 
  ggtitle("Average January precipitation at Seatac International Airport 1880-2020")
#> `geom_smooth()` using formula 'y ~ x'

####
## summer mean? total? ...doesn't appear that this is what pd_plot_slice does
####

to_slice <- prism_archive_subset("ppt", "monthly", mon = 7)
p <- pd_plot_slice(to_slice, seatac)

# add a linear average and title
p + 
  stat_smooth(method="lm", se = FALSE) + 
  theme_bw() + 
  ggtitle("Average July precipitation at Seatac International Airport 1880-2020")
#> `geom_smooth()` using formula 'y ~ x'

################################################################################
### Map data with leaflet
################################################################################
# https://cran.r-project.org/web/packages/prism/vignettes/prism.html

# 30-year normal average precipitation have already been downloaded for 
norm <- prism_archive_subset(
  "ppt", "annual normals", resolution = "4km"
)
rast <- raster(pd_to_file(norm))
proj4string(rast)<-sp::CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") ##assign projection info

# Create color palette and plot
pal <- colorNumeric(
  c("#0000FF", "#FFFF00", "#FF0000"), 
  values(rast),
  na.color = "transparent"
)

leaflet() %>% 
  addTiles(
    urlTemplate = 'http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}'
  ) %>% 
  addRasterImage(rast, colors = pal, opacity=.65) %>% 
  addLegend(pal = pal, values = values(rast), title = "Precip mm")

################################################################################
########################
p1990 <- raster('C:/Users/degaspec/Documents/prismtmp/PRISM_ppt_stable_4kmM2_1900_bil/PRISM_ppt_stable_4kmM2_1900_bil.bil')

plot(p1990, main = "Continental U.S. Precipitation - 1990",
     xlab = "Longitude", ylab= "Latitude")

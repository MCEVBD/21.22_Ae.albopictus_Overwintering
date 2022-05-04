#' ---
#' title: "07 OW albopictus- SNODAS snowdepth sites"
#' author: "Katie Susong"
#' date: "12th October 2020"
#' ---



#libraries
shhh <- suppressPackageStartupMessages # It's a library, so shhh!

shhh(library(raster))
shhh(library(tidyverse))
shhh(library(lubridate))
shhh(library(ncdf4))

# list SNODAS files
file.list <- list.files(path = "/Volumes/Seagate/SNODAS/snowdepth",pattern=".nc", full.names=TRUE)
# vector of dates
startdate <- ymd("2021-10-01")
enddate <- ymd("2022-04-16")
dates2018 <- seq(startdate, enddate, by = "days")
# site locations 
sites <- shapefile("00_Data/Shapefiles/Study_locations/study_location.shp")


# extraxt the site snow depth 

data <- data.frame(site = NA, date = ymd( NA), snow_depth = NA)  # empty df
for (i in 1:length(file.list)){
  snowdas_data <- raster::raster(file.list[i])                   # select the i file in the list 
  temp <- data.frame(site = sites@data$Site_Name,                # fill site with site_names ( Site_Names is defined in shape files)
                     date = ymd(rep(dates2018[i], 6)),           # date repeated (rep = # of sites)
                     snow_depth = rep(NA, 6))                    # blank NAs to fill with snowdepth
  temp[,3] <- raster::extract(snowdas_data, sites)               # extract snow depth 
  data <- rbind(data,temp)                                       # bind rows togauther 
}

#write file 
write_csv(data,'~/Documents/CBS_PhD/Ae.albo_OW_2021/21.22_Ae.albopictus_Overwintering/00_Data/21.22_SNODAS.csv')

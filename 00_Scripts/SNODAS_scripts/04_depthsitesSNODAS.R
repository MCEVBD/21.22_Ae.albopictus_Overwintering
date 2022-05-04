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

# list of 2018 SNODAS files
file.list <- list.files(path = "/Volumes/Seagate/SNODAS/studydates",pattern=".nc", full.names=TRUE)
# vector of dates
startdate <- ymd("2018-11-01")
enddate <- ymd("2018-12-31")
dates2018 <- seq(startdate, enddate, by = "days")
# site locations 
sites <- shapefile("~/Documents/CBS_PhD/albopictus_OW/Data/site_points/site_points1.shp")


# extraxt the site snow depth 

data <- data.frame(site = NA, date = ymd( NA), snow_depth = NA)
for (i in 1:length(file.list)){
  snowdas_data <- raster::raster(file.list[i])
  temp <- data.frame(site = sites@data$Number, date = ymd(rep(dates2018[i], 18)), snow_depth = rep(NA, 18))
  temp[,3] <- raster::extract(snowdas_data, sites)
  data <- rbind(data,temp)
}

#write file 
write_csv(data,'~/Documents/CBS_PhD/albopictus_OW/Data/OWsitesnow.csv')

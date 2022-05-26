#' ---
#' title: "02B add daily temperature to all snow file"
#' author: "Katie M Susong"
#' date: "05-May-2022"
#' ---
#' 

#' Overview
#' ========
#' 
#' Before completing regressions of tire temperature realted to snow. 
#' 
#' The format of the snow dataframe does not work well for R based analysis. 
#' To correct it will require that I split the df and correct each group of 
#' variables (cover) and (depth)
#'   The steps:
#'     1. Split snow dataframe into West, East, and South tire
#'     2. add number (tire id) to dfs
#'        tire ids: 1-13, tire number N. to S.(locations), E. to W (within locations).
#'     3. add E/W/S variable (ABC)
#'     4. change depth_Xtire, top_Xtire, side_Xtire variable to same name, rename snow_depth to snodas
#'     5. merge east, west and south 
#'     6. create single cover variable
#'        new varaibel cover will be made with the levels: "none" "top", "side", "both" 
#' 
#' Variabled added:
#' 
#'   mean daily T ( air and tire)
#'   max daily  T ( air and tire)
#'   min daily  T ( air and tire)
#'   daily Diff ( mean, max, min) 
#'   
#'   

## libraries ##
library(lubridate)
library(dplyr)
library(tidyr)
## Import data ##

# temperature data
dia  <- read.csv("00_Data/21.22_temperature.csv")
# format corrections
dia$DateTime <- mdy_hm(dia$DateTime)
dia$number   <- as.factor(dia$number)
dia$Date     <- mdy(dia$Date)

# snow data
snow <-  read.csv("00_Data/21.22_all_snow.csv")
# format corrections
snow$Date <- mdy(snow$Date)

##### Narrow snow dataframe ####

# 1. Split snow dataframe into West, East, and South tire
# each split df will contain: location, Date, depth_G, depth_Xtire, top_Xtire, side_Xtire, x, y

west <- subset(snow,select = c(location, Date, 
                               depth_G, snow_depth,
                               depth_Wtire, top_Wtire, side_Wtire, 
                               x, y))
east <- subset(snow,select = c(location, Date, 
                               depth_G, snow_depth,
                               depth_Etire, top_Etire, side_Etire, 
                               x, y))
south <- subset(snow,select = c(location, Date, 
                               depth_G, snow_depth,
                               depth_Stire, top_Stire, side_Stire, 
                               x, y))
# for south df remove all rows with NA values for the South variables

south <- drop_na(south,depth_Stire)


# 2. add number (tire id) to dfs

# east tires ( 1,3,5,8,10,12 (listed N to S))
east$number <- '12' # Car_E id number
east$number[east$location == 'Spo'] <- '1'
east$number[east$location == 'Han'] <- '3'
east$number[east$location == 'Arl'] <- '5'
east$number[east$location == 'Dek'] <- '8'
east$number[east$location == 'Bon'] <- '10'

# west tires (2,4,6,9,11,13)
west$number <- '13' # Car_E id number
west$number[west$location == 'Spo'] <- '2'
west$number[west$location == 'Han'] <- '4'
west$number[west$location == 'Arl'] <- '6'
west$number[west$location == 'Dek'] <- '9'
west$number[west$location == 'Bon'] <- '11'

# south tire (7)
south$number <- '7'

# 3. add E/W/S variable (ABC)

east$ABC  <- 'E'
west$ABC  <- 'W'
south$ABC <- 'S'

# 4. change depth_Xtire, top_Xtire, side_Xtire variable to same name, rename snow_depth to snodas
east  <- rename(east,depth_T = depth_Etire,
                cover_S = side_Etire,
                cover_T = top_Etire,
                snodas = snow_depth)
west  <- rename(west,depth_T = depth_Wtire, 
                cover_S = side_Wtire,
                cover_T = top_Wtire,
                snodas = snow_depth)
south <- rename(south,depth_T = depth_Stire,
                cover_S = side_Stire,
                cover_T = top_Stire,
                snodas = snow_depth)

# 5. rbind east, west and south 

snowF <- rbind(east, west, south)
# format corrections
snowF$number <- as.factor(snowF$number)
snowF$ABC    <- as.factor(snowF$ABC)

# 6. create single cover variable

# correct cover_T and cover_S: "" to "n"
snowF$cover_S[snowF$cover_S == ""] <- "n"
snowF$cover_T[snowF$cover_T == ""] <- "n"

snowF$cover <- NA # add blank variable
# select n and n for "none"
snowF$cover[snowF$cover_S == 'n' & snowF$cover_T == 'n'] <- "none"
snowF$cover[snowF$cover_S == 'y' & snowF$cover_T == 'n'] <- "side"
snowF$cover[snowF$cover_S == 'n' & snowF$cover_T == 'y'] <- "top"
snowF$cover[snowF$cover_S == 'y' & snowF$cover_T == 'y'] <- "both"


#### Daily temperature values ####


# create new dataframe to save new daily values
temp <- dia %>%
  group_by(number,Date) %>%
  dplyr::summarise(MeanT_Air = mean(Air_Temp), 
                   MeanT_Tire = mean(Tire),
                   MinT_Air = min(Air_Temp),
                   MinT_Tire = min(Tire),
                   MaxT_Air = max(Air_Temp),
                   MaxT_Tire = max(Tire))
  
# calculate difference values 
temp$MeanT_Diff <- temp$MeanT_Tire - temp$MeanT_Air
temp$MinT_Diff  <- temp$MinT_Tire  - temp$MinT_Air 
temp$MaxT_Diff  <- temp$MaxT_Tire  - temp$MaxT_Air 


# Merge temp daily files with snow file 

snow <- merge(temp, snow, by= c(number, Date))
  
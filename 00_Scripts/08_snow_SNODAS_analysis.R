#' ---
#' title: "08 SNODAS and snow analysis"
#' author: "Katie M. Susong"
#' date: "20-MAy-2022"
#' ---

#' Overview
#' ========
#' 
#' SNODAS is a useful tool for approximating snowcover but it will be useful 
#' to understand how that relates to measured on the ground snowcover and the coverage of the 
#' top or side or both. 
#' 
#' Model distribution:
#' 
#' The SNODAS and snow depth data follow a left-modal possion distribution 
#' (see 04_inital_snow_plots.R)
#' 
#' Models (Ground depth and SNODAS): 
#' ------
#' 
#' 1. Null model (null): depth_G ~ 1
#'     - AIC: 106856
#'     - Diagnostic plots: poor fit
#' 2. simple model (simple): depth_G ~ snow_depth
#'     - AIC: 51456
#'     -Diagnostic plots: poor fit
#' 3. include lat. (lat): depth_G ~ snow_depth + y
#'     - AIC: 49051
#'     - diagnostic plots: poor fit
#' 4. lat. plus interaction (latplus):  depth_G ~ snow_depth + y + y:snow_depth
#'     - AIC: 43483
#'     - diagnostic plots: improved fit
#' 5. Include long. (long) : depth_G ~ snow_depth + y + x + y:snow_depth
#'     - AIC: 42366
#'     - diagnostic plots: similar fit 
#' 6. long plus interaction between lat and long (longplus) : depth_G ~ snow_depth + y + x + y:snow_depth + x:y
#'     - AIC: 38373
#'     -diagnostic plots: improved
#' 7. longplus with long and snow_depth interaction (longplus2) : depth_G ~ snow_depth + y + x + y:snow_depth + x:y + x:snow_depth
#'     - AIC: 35627
#'     - diagnostic plots:  simiilar
#' 8. date added (date) : depth_G ~ snow_depth + y + x + date + y:snow_depth + x:y + x:snow_depth
#'     - AIC: 35269 **(reduced fit)** DO NOT SELECT
#' 


# libraries

library(lubridate)
library(sjPlot)

#### IMPORT DATA ####

snow <- read.csv("00_Data/21.22_all_snow.csv")
# format corrections
snow$date <- ymd(snow$date)

##### Null Moodel ####

null <- glm(depth_G ~ 1, family = poisson, snow)
summary(null)
plot(null)

#### Model 2 ####

simple <- glm(depth_G ~ snow_depth, family = poisson, snow)
summary(simple)
plot(simple)

#### Model 3 ####

lat <-  glm(depth_G ~ snow_depth + y, family = poisson, snow)
summary(lat)
plot(lat)

#### Model 4 ####

latplus <- glm(depth_G ~ snow_depth + y + y:snow_depth, family = poisson, snow)
summary(latplus)
plot(latplus)

#### Mdoel 5 ####

long <- glm(depth_G ~ snow_depth + y + x + y:snow_depth, family = poisson, snow)
summary(long)
plot(long)

#### Model 6 ####

longplus <- glm(depth_G ~ snow_depth + y + x + y:snow_depth + x:y, family = poisson, snow)
summary(longplus)
plot(longplus)

#### Model 7 ####

longplus2 <- glm(depth_G ~ snow_depth + y + x + y:snow_depth + x:y + x:snow_depth, family = poisson, snow)
summary(longplus2)
plot(longplus2)
#### Model 8 ####

date <- glm(depth_G ~ snow_depth + y + x + date + y:snow_depth + x:y + x:snow_depth, family = poisson, snow)
summary(date)
plot(date)


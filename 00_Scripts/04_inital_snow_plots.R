#' ---
#' title: "04 initial snow plots"
#' author: "Katie M Susong
#' date: "06-May-2022"
#' --- 

#' Snow deoth plots
#' ================
#' 
#' Inital plot looking at snow depth through time and SNODAS compared to measured snowdepth
#' 

# libraries
library(ggplot2)
library(dplyr)
library(lubridate)

######## Import and format Data ############

snow <- read.csv("00_Data/21.22_all_snow.csv") 

# correct str 
snow$date <- ymd(snow$date)


#on site measurements are in cm and need to be changed to mm
#snow$depth_Etire <- snow$depth_Etire * 10
#snow$depth_G     <- snow$depth_G * 10
#snow$depth_Wtire <- snow$depth_Wtire  * 10
#snow$depth_Stire <- snow$depth_Stire  * 10

# write .csv with corrected units
# write.csv(snow, "00_Data/21.22_all_snow.csv")

# inital investigation plots

## GROUND DEPTH v. SNODAS
ggplot(snow, aes(depth_G, snow_depth))+
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  ylim(0,400)
# HISTOGRAM: GOUND DEPTH
hist(snow$depth_G)
hist(snow$snow_depth)
#' both SNODAS and the measured snow cover exhibit a left-modal poisson PDF
#' 
#' It seems likely that these two distributions not be significantly different. 

## GROUND DEPTH v. TIRE DEPTH

ggplot(snow, aes(depth_G, depth_Etire))+
  geom_point() + 
  ylim(0,300)

ggplot(snow, aes(depth_G, depth_Wtire))+
  geom_point() + 
  ylim(0,300)

ggplot(snow, aes(depth_Etire, depth_Wtire))+
  geom_point() +
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,125)
#' E an W tire visually appear to trend the same 
#' 
#' There may be trend for ground v. tire cover but it is slopy and may only apply at higher snow depths. 



################################################################################

#pivot snow depth 
snow_p <- pivot_longer(snow,cols = c(starts_with("depth"), "snow_depth"), 
                     names_to = "measure",
                     values_to = "depth")


# add ground/tire varaible 
snow$loc.GT <- substr(snow$measure, 7,7)
snow$loc.GT [snow$loc.GT == "e"] <- "G"
snow$loc.GT [snow$loc.GT == "W"] <- "Ti"
snow$loc.GT [snow$loc.GT == "E"] <- "Ti"
snow$loc.GT [snow$loc.GT == "S"] <- "Ti"
snow$loc.GT <- as.factor(snow$loc.GT)

# add measure.type variable
snow$measure.type <- "onsite"
snow$measure.type[snow$measure == "snow_depth"] <- "SNODAS"
snow$measure.type <- as.factor(snow$measure.type)



# Plot 

snow %>%
  filter(loc.GT == "G") %>%
  ggplot( aes(date,depth, col= measure.type)) + 
  geom_line()+
  facet_wrap(~location)

snow %>%
  filter(loc.GT == "G") %>%
  filter(location == "Spo") %>%
  ggplot( aes(date,depth, col= measure.type)) + 
  geom_line()


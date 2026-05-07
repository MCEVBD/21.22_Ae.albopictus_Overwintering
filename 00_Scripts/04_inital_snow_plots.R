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
library(tidyr)
library(lubridate)

######## Import and format Data ############

snow <- read.csv("00_Data/21.22_all_snow.csv") 

# correct str 
snow$date <- ymd(snow$date)

# inital investigation plots

## GROUND DEPTH v. SNODAS
ggplot(snow, aes(depth_G, snow_depth))+
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  facet_wrap(~location) +
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
snow_p$loc.GT <- substr(snow_p$measure, 7,7)
snow_p$loc.GT [snow_p$loc.GT == "e"] <- "G"
snow_p$loc.GT [snow_p$loc.GT == "W"] <- "Ti"
snow_p$loc.GT [snow_p$loc.GT == "E"] <- "Ti"
snow_p$loc.GT [snow_p$loc.GT == "S"] <- "Ti"
snow_p$loc.GT <- as.factor(snow_p$loc.GT)

# add measure.type variable
snow_p$measure.type <- "onsite"
snow_p$measure.type[snow_p$measure == "snow_depth"] <- "SNODAS"
snow_p$measure.type <- as.factor(snow_p$measure.type)


# summary values 

Jan_avg_tire <- snow_p %>%
  filter(between(date, as.Date('2021-12-31'), as.Date('2022-02-01'))) %>%
  group_by(location,  measure) %>%
  summarise(mean = mean(depth, na.rm = T),
            sd = sd(depth, na.rm = T))
Jan_avg_loc <- snow_p %>%
  filter(between(date, as.Date('2021-12-31'), as.Date('2022-02-01'))) %>%
  group_by(location) %>%
  summarise(mean = mean(depth, na.rm = T),
            sd = sd(depth, na.rm = T))

# Plot 

snow_p %>%
  filter(loc.GT == "G") %>%
  ggplot( aes(date,depth, col= measure.type)) + 
  geom_line()+
  facet_wrap(~location)

snow_p %>%
  filter(loc.GT == "G") %>%
  filter(location == "Spo") %>%
  ggplot( aes(date,depth, col= measure.type)) + 
  geom_line()

?filter()

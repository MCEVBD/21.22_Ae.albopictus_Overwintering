#' ---
#' title: FIGURE: SNODAS model/ predictions
#' author: Katie M Susong
#' date: 22-June-2022
#' ---
#' 

rm (list  = ls())
# Libraries
 #data formatting
library(dplyr)
library(tidyr)
library(lubridate)
 #general plotting
library(ggplot2)
library(cowplot)
 #model specific 
library(sjPlot)
library(effects)

#### import/source data and models for figure development ####

setwd("~/Documents/CBS_PhD/Ae.albo_OW_2021/21.22_Ae.albopictus_Overwintering")

# this file contains all models and df for plots
source("00_Scripts/14_compare_snow.temperature_glm.R")

#' **data notes**
#' 18/19 data == DIA
#' 21/22 data == data
#' all   data == both
#' 
#'  holdout includes the predicted values from all 3 model methods on a with heald subset of the 
#'  data ( withheld from the all data model)
#'

# pivot both predicted values longer
both <- pivot_longer(both, cols = c(m18.19,m21.22,m.all), names_to = "model",values_to = "predict" )
# repeat for hold out df
holdout <- pivot_longer(holdout, cols = c(m18.19,m21.22,m.all), names_to = "model",values_to = "predict" )

# add year/ study year variable
both$year  <- year(both$date) # extract year
both$study <- "y18.19" # add 18.19 study year to all variables
both$study[both$year == "2021"] <- "y21.22" # replace to  21.22 to 2021 dates
both$study[both$year == "2022"] <- "y21.22" # replace to  21.22 to 2022 dates



# set snow depth color palette
my_colors <-c( RColorBrewer::brewer.pal(n = 7, name ="Purples") [3:7])


#'### Plot 1: Model figure
#'
#'Showing the trend by level of snow

# collect estimate data for both
effects <- effects::effect(term= "snow_FAC10", mod= m.all)
summary(effects)
x.eff <- as.data.frame(effects)


p1 <- ggplot(both, aes( MeanT_Ambient, MeanT_Tire))+
  geom_point(aes(col = snow_FAC10, shape = study), alpha = 0.4) +
  geom_smooth(aes(col = snow_FAC10, fill = snow_FAC10), method = "glm", show.legend = F, fullrange = F) +
  theme_classic() +
  ylab( "Mean Daily Temperature, Internal (??C)") +
  xlab ( "Mean Daily Temperature, External (??C)") + 
  scale_fill_manual(values = c("#BCBDDC" ,"#9E9AC8" ,"#807DBA" ,"#6A51A3" ,"#4A1486")) +
  scale_color_manual(name = "Snow Depth (mm)",
                     values = c("#BCBDDC" ,"#9E9AC8" ,"#807DBA" ,"#6A51A3" ,"#4A1486"),
                     labels = c("No Snow", "less than 100 mm ", "100 to 199 mm", 
                                 "200 to 299 mm", "more than 300 mm")) + 
  theme(legend.key=element_blank()) +
  scale_shape_discrete(name = "Study year",
                       labels = c("2018/2019", "2021/2022"))

# save high res. version of plot
setwd("~/Documents/CBS_PhD/Ae.albo_OW_2021/03_figures_presentation/Figures")
ggsave("SNODASmodel.pdf", plot = p1, width = 7, height = 5, units = "in", dpi = 500)


plot_model(m.all, type = "pred", terms = c("MeanT_Ambient", "snow_FAC10")) 
tab_model(m21.22)


#'### Plot 2: ability to predict and similarity between 2 years
#'

plot_models(m.all, m18.19, m21.22)

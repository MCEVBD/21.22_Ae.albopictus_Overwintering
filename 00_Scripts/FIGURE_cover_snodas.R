#' ---
#' title: FIGURE: Cover extent related to temperature
#' author: Katie M Susong
#' date: 22-June-2022
#' ---
#' 

rm (list  = ls())
# Libraries
library(dplyr)
library(ggplot2)
library(cowplot)
library(sjPlot)


#### Import Data  and models ####
# set wd 
setwd("~/Documents/CBS_PhD/Ae.albo_OW_2021/21.22_Ae.albopictus_Overwintering")
# source analysis code
source("00_Scripts/19_cover_temperature_analysis.R")

# set cover color palette
my_colors <- RColorBrewer::brewer.pal(n = 7, name ="Blues") [2:7]

#'### PLOT 1: Difference, cover and snow depth 
#'
#'

p1 <- ggplot(data, aes(snow_FAC10,MeanT_Diff)) +
  geom_violin() +
  geom_point(aes(col = cover ), alpha = 0.6) +
  theme_half_open(12) +
  background_grid(major = 'y', minor = "none") +
  xlab("Snow Depth, categorical (mm), SNODAS")+
  ylab("Differnce in Mean daily Temperature (??C)") +
  scale_x_discrete(labels = c("No Snow", "less than 100 mm ", "100 to 199 mm", 
                              "200 to 299 mm", "more than 300 mm")) +
  scale_color_manual(values = c("#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#084594"), 
                    na.value="black", 
                    labels = c("Uncovered Tire", "Only Side Covered", "Only Top Covered","Side and Top Covered")) +
  theme(axis.text.x = element_text(angle = 40, hjust= 1)) +
  theme(legend.position = "none")


#'### PLOT 2: Cover and snow depth
#'

p2 <- ggplot(data, aes(snow_FAC10, cover, fill = cover))+
  geom_bar(stat = "identity", width = 0.5) + 
  scale_fill_manual(values = c("#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#084594"), # blue color scale
                    na.value="black", 
                    labels = c("Uncovered Tire", "Only Side Covered", "Only Top Covered","Side and Top Covered")) +
  theme_classic()+
  xlab("Snow Depth, categorial (mm), SNODAS") +
  scale_x_discrete(labels = c("No Snow", "less than 100 mm ", "100 to 199 mm", 
                              "200 to 299 mm", "more than 300 mm")) +
  ylab("n (Obs. of Snow Depth)") +
  theme(axis.text.y = element_blank()) +
  theme(axis.text.x = element_text(angle = 40, hjust= 1)) +
  theme(legend.title = element_blank(), 
        legend.position = "top")

#'### Plot 3: Cover forest plot 
#'
#'

# quick  table
tab_model(cover.glmb)

# extract data from model 

lables <- c("Uncovered Tire", "Only Side Covered", "Only Top Covered",
"Side and Top Covered", "Mean Daily Air temperature")
Estimates <-cover.glmb$coefficients
LowerCI <- confint(cover.glmb)[1:5]
UpperCI<- confint(cover.glmb)[6:10]
pvalue <- summary(cover.glmb)$coef[,"Pr(>|t|)"]
round(pvalue, 3)
Rpvalue <- c("<0.001", "<0.001", "0.762", "<0.001", "<0.001" )


est <- data.frame(lables, Estimates, LowerCI, UpperCI, pvalue, Rpvalue)

p3 <- est%>%
  filter(lables != "Mean Daily Air temperature")%>%
  ggplot( aes(x=reorder(lables,Estimates), y=Estimates, ymin=LowerCI, ymax=UpperCI)) +
  geom_pointrange() + 
  geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Extent of Snow Cover") +
  ylab("Estimate (95% CI)") +
  theme_classic()  +
  background_grid(major = 'x', minor = "none") + 
  geom_text(label = est$Rpvalue[1:4] ,vjust = -1, hjust = 1) 

p3

#'### PLOT 4: Cover and Snow depth 
#'
#'

# group by snow depth (factor) and cover to result in  a tally of of cover at each depth
cover.tally <-data %>% 
  group_by(snow_FAC10,cover) %>%
  tally()

# total number of measurements taken at each depth. 
snow.tally <- data %>% 
  group_by(snow_FAC10) %>%
  tally()

#check that the same total number is generated
sum(snow.tally$n)
sum(cover.tally$n)

#create empty column for the proportion of each cover type at each snow depth 
cover.tally$prop <- NA
# add the proportion value by dividing the tally at each cover by the total a the same snow cover
## none
cover.tally$prop[cover.tally$snow_FAC10 == "none"] <-
  cover.tally$n[cover.tally$snow_FAC10 == "none"] / snow.tally$n[snow.tally$snow_FAC10 == "none"]
## >100
cover.tally$prop[cover.tally$snow_FAC10 == ">100"] <-
  cover.tally$n[cover.tally$snow_FAC10 == ">100"] / snow.tally$n[snow.tally$snow_FAC10 == ">100"]
## >200
cover.tally$prop[cover.tally$snow_FAC10 == ">200"] <-
  cover.tally$n[cover.tally$snow_FAC10 == ">200"] / snow.tally$n[snow.tally$snow_FAC10 == ">200"]
## >300
cover.tally$prop[cover.tally$snow_FAC10 == ">300"] <-
  cover.tally$n[cover.tally$snow_FAC10 == ">300"] / snow.tally$n[snow.tally$snow_FAC10 == ">300"]
## >400
cover.tally$prop[cover.tally$snow_FAC10 == ">400"] <-
  cover.tally$n[cover.tally$snow_FAC10 == ">400"] / snow.tally$n[snow.tally$snow_FAC10 == ">400"]

#'#### Create Plot
#'
#'
p4 <- ggplot(cover.tally, aes(snow_FAC10, prop, fill = cover))+
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = c("#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#084594"), # blue color scale
                    na.value="black", 
                    labels = c("Uncovered Tire", "Only Side Covered", "Only Top Covered","Side and Top Covered")) +
  theme_classic()+
  xlab("Snow Depth, categorial (mm), SNODAS") +
  scale_x_discrete(labels = c("No Snow", "less than 100 mm ", "100 to 199 mm", 
                              "200 to 299 mm", "more than 300 mm")) +
  ylab("Proportion of Observations") +
  theme(axis.text.y = element_blank()) +
  theme(axis.text.x = element_text(angle = 40, hjust= 1)) +
  theme(legend.title = element_blank(), 
        legend.position = "top")

#'### Make merged plot option
#'

bottem <- cowplot::plot_grid(p1, p2)
full <-plot_grid(p3, bottem, ncol= 1)

# create shared legend 

legend <- get_legend(p1)

plot_grid(p1, p2, legend, ncol = 1)

#'### Save plots as high quality files
#'

setwd("~/Documents/CBS_PhD/Ae.albo_OW_2021/03_figures_presentation/Figures")

ggsave("cover_violin.pdf", plot = p1, width = 5, height = 5, units = "in", dpi = 500)
ggsave("cover_bar.pdf",    plot = p2, width = 7, height = 5, units = "in", dpi = 500)
ggsave("cover_model.pdf",  plot = p3, width = 5, height = 3, units = "in", dpi = 500)
ggsave("cover_bar_prop.pdf",  plot = p4, width = 5, height = 3, units = "in", dpi = 500)


  
  
  

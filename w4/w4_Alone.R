#TidyTuesday w 4 2023
#Alone moovie data by Daniel Oehm
#https://gradientdescending.com/alone-r-package-datasets-from-the-survival-tv-series/
#git hub https://github.com/doehm/alone  description of the data 

#package with the data 
#install.packages("alone")

#libraries
library(tidyverse)
library(ggplot2)
library(alone)
library(jpeg)
library(cowplot) #several plots in one space 

#set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#import data 
tuesdata <- tidytuesdayR::tt_load('2023-01-24')


#image generated with Midjourney
img <- readJPEG("back5.jpg")


#data wrangling
summary <-  survivalists %>% group_by(gender) %>% summarise(days_lasted = round(mean(days_lasted), digits=0))
sum_parti <- survivalists %>% group_by(gender) %>% summarise(n=n())


### Texts 
# title 
tit <- "Alone"
subt <- "Women survived on average longer in the competition  \n Less women than men participated" 
capt <- "@Ralitza Soultanova, data: Daniel Oehm"

###plot survivors per day gender - left plot

surv <- ggplot(summary, aes(x = gender, y = days_lasted)) +
  geom_segment(aes(x = gender, xend = gender, y = 0, yend = days_lasted),
               color="grey", size=13) +
  geom_point(size=12, color = "grey") +
  geom_text(aes(label=days_lasted), color="#3a6d5c", size=8)+
  coord_flip()+
  theme_void()

### plot number of participants per gender - right plot

parti  <- ggplot(sum_parti, aes(x = gender, y = n)) +
  geom_segment(aes(x = gender, xend = gender, y = 0, yend = n),
               color="grey", size=13) +
  geom_point(size=12, color = "grey") +
  geom_text(aes(label=n), color="#3a6d5c", size=8)+
  coord_flip()+
  theme_void()


### combination of plots with ggdraw 
# cowplot article  https://meghan.rbind.io/blog/cowplot/

ggdraw() +
  #backgraound image
  theme(plot.background = element_rect(fill="black", color = NA))+
  draw_image(img, x = .0, y = .0, width = 1)+
  #Title plot 
  draw_label(tit,
             fontfamily = "Euphemia UCAS", size = 90,
             x = 0.5, y=0.94, hjust = 0.5, color="white") +
  draw_label(subt, 
             fontfamily = "Euphemia UCAS", size = 18,
             x = 0.5, y=0.85, hjust = 0.5, color="white")+

  #left plot
  draw_plot(surv, x = .025, y = .03, width = .35, height = .22) +
  draw_label("Average days survived", fontfamily = "Avenir", size = 24,
             y = 0.25, x = 0.06, hjust = 0, colour = "#3a6d5c")+
  draw_label("M.", fontfamily = "Avenir", size = 24,
             y = 0.189, x = 0.06, hjust = 0, colour = "#3a6d5c") +
  draw_label("F.", fontfamily = "Avenir", size = 24,
             y = 0.094, x = 0.06, hjust = 0, colour = "#3a6d5c") +

  #right plot 
  draw_plot(parti, x = .49, y = .03, width = .35, height = .22) +
  draw_label("Number of participants", fontfamily = "Avenir", size = 24,
             y = 0.25, x = 0.52, hjust = 0, colour = "#3a6d5c")+
  draw_label("M.", fontfamily = "Avenir", size = 24,
             y = 0.189, x = 0.52, hjust = 0, colour = "#3a6d5c") +
  draw_label("F.", fontfamily = "Avenir", size = 24,
             y = 0.094, x = 0.52, hjust = 0, colour = "#3a6d5c") +
# caption 
  draw_label(capt,
             fontfamily = "Avenir", size = 20,
             y = 0.03, x = 0.5, hjust = 0.5, colour = "#3a6d5c")+
  #theme
  theme(plot.margin = margin(1, 1, 1, 1))


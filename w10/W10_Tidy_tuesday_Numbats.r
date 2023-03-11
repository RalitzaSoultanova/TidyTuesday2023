##DATA DESCRIPTION 
#TidyTuesday w 10 2023 Numbats
# Autor Ralitza Soultanova
# Please quote if you use the code
#TidyTuesday  #r4ds #tidyverse #RStats #DataViz #TidyTuesday  @R4DSCommunity
#description

#My github https://github.com/RalitzaSoultanova/TidyTuesday2023

#clean work environment 
rm(list=ls())
#packages 
  
  library(tidyverse)
  library(camcorder)
  library(ggtext)
  library(showtext) #add google fonts 

#set working directory to file location 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Get the data 

numbats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-07/numbats.csv')


# camcorder record plot - keeps the plot ration fixed 
gg_record(dir = "tidytuesday-temp", device = "png", width = 12, height = 12, units = "in", dpi = 300)


# A quick glimpse on the data 

str(numbats)

ggplot(since2000, aes(n, year))+
  geom_line()

# clean the data
# remouve data with missing year 

num <- numbats %>% filter(year !="NA")%>%
      select(- prcp, -tmax, -tmin, - dryandra)


#observations since 2014 without 2023 
month_year <- num %>% filter(year> 2013&year < 2023)  %>% group_by(year, month) %>% 
  mutate(
    wday = factor(wday, levels = rev(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))),
    month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))) %>%
  count(month) %>%
  ungroup()

month_year <- month_year %>% arrange(year) 

#number of observations per month 
n_month <- month_year  %>% group_by(month) %>% summarise(per_month = sum(n))
total_since2013 <- n_month %>% summarise(total=sum(per_month))


#colors 
c_back <- "white"
c_text   <- '#033854'


#fonts 
# load Google fonts (https://fonts.google.com/)
font_add_google("Poppins", "Poppins")
font_add_google("Jura", "Jura")

# Automatically use showtext to render text
showtext_auto()

#texts
tit  <- "Summer Months: Peak Season for Numbat Sightings in Australia"
subtit  <- "Analysis of 405 Numbats observed since 2014, highlights that 70% 
of sightings occur during the last two months of the year."
cap  <-   "Visualisation - Ralitza Soultanova, data: Atlas of Living Australia, for: TidyTuesday week 10 "

#plot  
ggplot(month_year, aes(x = month, y = year, fill = n)) +
geom_tile(color = "white",
            lwd = 1,
            linetype = 1)+

geom_text(aes(label = n), color = "white", size = 14) +
scale_fill_gradient2(low = "white",
                       mid = "orange",
                       high = "#FF0000") +
#guides(fill = guide_colourbar(barwidth = 3,
  #                              barheight = 20))+
labs(title=tit, 
    subtitle = subtit,
    caption = cap) +
theme_void() +
scale_x_discrete(position = "top") +
scale_y_continuous(breaks = c(2014, 2015, 2016, 2017, 2018,2019, 2020, 2021, 2022)) +
theme(text = element_text(family = "Jura", colour = c_text),
  plot.title = element_text(family = "Jura",
                          size = 80,
                          colour = c_text,
                          hjust = 0,
                          margin = unit(c(0, 0.3, 1, 5), "cm")),
  plot.subtitle = element_text(
                          family = "Jura",
                          size = 60,
                          lineheight = 0.4,
                          hjust = 0,
                          colour = c_text,
                          margin = unit(c(0, 0.5, 2, 5), "cm")),
  plot.caption = element_text( size=52, 
                          hjust = 0,
                          colour = c_text,
                          margin = unit(c(1, 0, 1, 0), "cm")),

  #plot.tag.position = c(0.01, 0.72),
  legend.position = "none",
  plot.margin = unit(c(0.6, 0.6, 0.6, 0.6), "cm"),
  axis.text.y=element_text(
    family = "Poppins",
    size = 45,
    colour = c_text,
    hjust = 0),
  axis.text.x=element_text(
    family = "Poppins",
    size = 45,
    colour = c_text,
    hjust = 0.5),
  plot.title.position = "plot",
panel.grid.minor = element_line(colour="grey", size=0.1),
plot.background = element_rect(fill = c_back, colour = NA),
panel.background = element_rect(fill = c_back, colour = NA))



####### Save image 
ggsave("numbats.png",  width = 12)

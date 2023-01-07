#libraries
library(tidyverse)
library(ggplot2)
#library(lubridate)
library(readxl)
library(dplyr)
library(showtext)


#library(ggtext)
#turn on show text
font_add_google("Dosis", "dos")
showtext_auto()
#set working directory to file location 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#Import data

df <- read_xlsx("fitbit2022_short.xlsx")
glimpse(df)

#transform data 
df <- df %>%
  rename_all(str_to_lower)%>%
  rename_all(str_replace,  "\\s+", "_") %>% 
  mutate(
    date2=as.Date(ymd(date)),
    month = month(ymd(date))
  )

#Create a new column with steps goal of 11000 steps reached or not
df <- df%>% 
  mutate(Goal = if_else(steps > 10999, "yes", "no"))


#3 days with most steps
top_3 <- df %>% 
  arrange(desc(steps)) %>% 
  top_n(3, steps)

View(top_3)
#All top days correspond to walks with girl friends - Barcelona, Bulgaria, Belgium 

#making a plot of the steps per day 

p <- ggplot(df, 
       aes(date2, 
           ymin=100,
           ymax=steps))+
  geom_linerange(size = 1.3, alpha = 0.65, color="#292929")+ 
  scale_x_date(date_labels = "%b", breaks = date_breaks("month")) +
  ylim(0, 20000) +
  labs(
    title = "My 2022 in Steps",
    subtitle = " 4.173.417 Steps done in 2022 \n11.434 daily average\n3.300 km walked",
    caption = "©  Ralitza Soultanova",
    x = NULL,
    y = NULL) +
  
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.background = element_rect(fill = "#292929"), 
        panel.background = element_rect(fill = "#292929"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_line(color = "grey",
                                          size = 0.1,
                                          linetype = 1),
        panel.grid.major.x = element_line(color = "grey",
                                          size = 0.1,
                                          linetype = 1))+
        #coord_polar() +
        theme(legend.position = "bottom")+
        geom_hline(yintercept = 11000, color="#1a7678", alpha = 0.7) #line for goal
        
p + geom_linerange(aes(color= Goal), alpha=0.6) +
  theme(
        plot.title = element_text(hjust = 0.5, color = "#1a7678", size = 22), 
        plot.subtitle =element_text(hjust = 0.5, color = "#1a7678", size=12), 
        legend.title = element_text(color="#1a7678", size=10), 
        # Change legend background color
        legend.background = element_rect(fill = "#292929"),
        legend.key = element_rect(fill = "#292929", color = NA), 
        plot.caption = element_text(color="#1a7678")
  )  #hjust 0.5 center hjust 1 right align
        




#Inspired by 
#Joshua Kunst Fuentes. 2016. “How to: Weather Radials.” March 24, 2016. 
#https://jkunst.com/blog/posts/2016-03-24-how-to-weather-radials.
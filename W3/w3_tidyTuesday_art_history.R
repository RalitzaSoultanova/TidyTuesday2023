#Art history 
#Data Citation:
#  Stam, H. (2022). Data from: Quantifying art historical narratives. Duke Research Data Repository. https://doi.org/10.7924/r4dn48h0w
#r4ds #tidyverse #RStats #DataViz #TidyTuesday @R4DSCommunity


#packages 
library(dplyr)
library(tidyverse)
library(waffle)
library(ggplot2)
library(showtext)



#set working directory to file location 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Get the Data
#artists - data on artists included in 
artists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-17/artists.csv')



#save the data frame locally as not to download every time 
save(artists, file="artists.Rda")

#quick inspection of the data skim() in place of summary()
skim(artists)

#load fonts 

font_add_google("Atkinson Hyperlegible", "legible")
showtext_auto()

# tudy the data 
# select only Gardner 
#remouve na gender 

gardner <- artists %>% 
    filter(book=="Gardner") %>%
    filter(artist_gender != "N/A")


is_na <- gardner %>% filter(is.na(artist_gender))


#artist per gender df 

gender <- gardner %>% count(year, artist_gender) %>% arrange(desc(artist_gender))

#colors 
c_back <- "#35243E"
c_points <- c("#507F90", "#CDBBD1")
c_text <- "#DFD3DD"

#waffle artist gender 
p <- ggplot(gender, aes(fill=artist_gender, values=n)) +
  geom_waffle(colours=c_back, size = .25, n_rows = 5, flip = TRUE)+ 
  #color = c_back,
  facet_wrap(~year, nrow = 1, strip.position = "bottom")+ 
  scale_x_discrete() +
  scale_y_continuous(labels = function(x)x*5, expand = c(0,0)) +  # make this multiplyer the same as n_rows
  
  coord_equal() +
  labs(
    title = "Where are the Women?",
    subtitle = "Artists published in Gardner's 'Art Through the Ages' by Gender \n ",
    x = "Year",
    y = "Count", 
    caption= "@Ralitza Soultanova, data: Holland Stam, art history, for: TidyTuesday",
    legend.title = "Gender")  +
  theme_void() + 
    theme( 
      legend.position = "bottom",
        text = element_text (color=c_text, hjust=0.5),
        plot.title= element_text(
            hjust=0.5, 
            size=55,
            face = "bold",
            family = "legible"),
          #margin = margin(t = 10, b = 10)
        plot.subtitle= element_text(hjust=0.5, size=20),
        plot.background = element_rect(fill = c_back),
        plot.caption = element_text(hjust=0.5, size=15),
        plot.margin = margin(50, 30, 40, 30),
      legend.key.size = unit(0.9, "lines"),
      legend.margin = margin(0.5, 0.2, 0.5, 0.2, "cm")
    #  , plot.margin = margin(15,15,15,15)
     ) 

p + labs(fill="Gender")


####### Save image 
ggsave("women.png",  
       width = 1500,
       height = 1200,
       units = "px",
       bg = c_back,
       dpi = 300)



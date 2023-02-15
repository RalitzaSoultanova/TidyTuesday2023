#TidyTuesday w 7 2023 Age gap in holiwood movies
# Autor Ralitza Soultanova
# Please quote if you use the code
#TidyTuesday  #r4ds #tidyverse #RStats #DataViz #TidyTuesday  @R4DSCommunity
#Same sex couples are absent from Hollywood movies. TidyTuesday week 7. 
#github https://github.com/RalitzaSoultanova/TidyTuesday2023

#libraries
library(tidyverse)
library(ggplot2)
library(camcorder)
library(showtext)
library(ggtext)
library(patchwork) #several plots in one canvas 



#set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#import data 
age_gaps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-14/age_gaps.csv')
#inspect data 
str(age_gaps)

# main question do homosexual  relations exist in movies and how often are they represented
# adding a column for uni or by-sexual actors

  age_f <- age_gaps %>% mutate(type_couple =
                       case_when(character_1_gender== "woman" & character_2_gender == "woman" ~ "homo", 
                                 character_1_gender== "man" & character_2_gender == "man" ~ "homo",
                                 character_1_gender== "woman" & character_2_gender == "man" ~ "hetero",
                                 character_1_gender== "man" & character_2_gender == "woman"~ "hetero"))
  
 #summary n of film per year per type couple 
   summary <- age_f  %>%    
    count(release_year, type_couple)
 
# camcorder record plot 
   gg_record(dir = "tidytuesday-temp", device = "png", width = 12, height = 12, units = "in", dpi = 300)
   
# load Google fonts (https://fonts.google.com/)
   font_add_google("Poppins", "Poppins")
   font_add_google("Zeyada", "Zeyada")
   font_add_google("Jura", "Jura")
   
   ## Automatically use showtext to render text
   showtext_auto()

### Texts 
# title 
   tit <- "Same-sex couples are absent from Hollywood movies"
   subt <- "First same-sex male couple was represented in 1997,
            \nFirst same-sex female couple in 2003
            \nviz: Ralitza Soultanova, data: Data is Plural"

   
#colors 
   c_back <- "#1b232f"
   c_bars <- c("#91a08e", "#e0874c")
   c_text <- "#b8ccb5"
     
#plot
p <- ggplot(age_f, aes(release_year, fill=type_couple))+
  geom_bar()+ 
  scale_fill_manual(values=c_bars)+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank())+ 
  ylab("Number of couples")+
  xlab("Release Year") +
  scale_x_continuous(breaks = c(1935, 1955, 1975, 1995, 2010, 2022))


  
# join with patchwork
p + 
  
  plot_annotation(title = tit,
                  subtitle = subt) &
  theme(text = element_text(family = "Jura", colour = c_text),
        plot.title = element_text(family = "Jura",
                                  size = 30,
                                  colour = c_text,
                                  hjust = 0.3,
                                  margin = unit(c(0.8, 0.3, 1, 0), "cm")),
         plot.subtitle = element_markdown(size = 23,
                                         lineheight = 0.2,
                                         hjust = 0.3,
                                         colour = c_text,
                                         margin = unit(c(0, 0, 0, 2), "cm")),
        plot.tag.position = c(0.01, 0.72),
        axis.text.y=element_text(size=15, color=c_text, vjust=0),
        axis.title.y = element_text(size=20, color=c_text, vjust=2),
        axis.text.x=element_text(size=15, color=c_text, vjust=10),
        axis.title.x =element_text(vjust= 5, size=20, color=c_text),
        plot.title.position = "plot",
        #strip.text = element_text(size = 24, lineheight = 0.4, colour = "#2F4F4F"),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = c_back, colour = NA),
        panel.background = element_rect(fill = c_back, colour = NA))
       
  
  


#generate the final GIF, use the gg_playback() function. The user can define: - where the final GIF gets saved by setting the name 
#argument, - duration of the first and last
#images with first_image_duration or last_image_duration - delay between frames in seconds with frame_duration

gg_playback(
  name = file.path(tempdir("tidytuesday-temp"), "vignette_gif.gif"),
  first_image_duration = 3,
  last_image_duration = 10,
  frame_duration = .4,
  image_resize = 800
)


#save the data for future use 
write.csv(age_f , "age_f", row.names=TRUE)


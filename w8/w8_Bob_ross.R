#Tudy Tuesday w8 Bob Ross 
# Autor Ralitza Soultanova
# Please quote if you use the code
#r4ds #tidyverse #RStats #DataViz #TidyTuesday @R4DSCommunity

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


library(ggplot2)
library(dplyr)
library(tidyverse)
library(showtext)
library(camcorder)
library(geomtextpath)

#library(stringr)

# Get the Data
bob <- read_csv("https://raw.githubusercontent.com/jwilber/Bob_Ross_Paintings/master/data/bob_ross_paintings.csv") 
bob %>% write.csv("bob.csv")

#select only painting n, season, episode, color 
bob2 <-  bob[, c("painting_index", "season", "episode", "color_hex"), with = FALSE]

#remove additional separating characters in the color_hex 
bob2$color_hex <- str_remove_all(bob2$color_hex, "\\[|\\]|\\''' '") 

#df_sep - one line per color per episode  
df_sep <- bob2 %>% separate_rows(color_hex, sep = ",") 
df_sep$color_hex <- str_remove_all(df_sep$color_hex, " ")
df_sep$color_hex <- str_remove_all(df_sep$color_hex, "'")
df_sep$color_hex <- str_remove_all(df_sep$color_hex, "]")

# camcorder record plot 
gg_record(dir = "tidytuesday-temp", device = "png", width = 12, height = 12, units = "in", dpi = 300)

#top 3 colors per season 

top3 <- df_sep %>% 
  group_by(season) %>%
  count(color_hex) %>%
  top_n(3, n)

# Set fonts
font_add_google('Spectral')
showtext_auto()

### Texts 
# title 
tit <- "Colors of Bob Ross"
subt <- "Most used colors per season. Suprisingly they are quite consistent durring the seasons"
cap <- "viz: Ralitza Soultanova, data: Bob Ross paintings"
text_lab = "Seasons 1-31"
col_t <- '#0C0040'
col_back <- "white"
  

#colors for fill 
col <- as.factor(top3$color_hex)


#plot 

top3 %>%ggplot(aes(season, n, fill=col)) +
  geom_bar(position="fill", stat="identity", colour="#D0CFCF", width = 0.99) +
  scale_color_manual(values = col) +
  scale_fill_identity()+
  theme_void()+  
  coord_polar()+
  geom_segment(aes(x = 0.5, y = 1.03, xend = 2, yend = 1.03),
               arrow = arrow(length = unit(0.15, "cm")), color = col_t, size = .15)+
  #annotate(geom = 'text',family ="Spectral", x = 0.5, y = 1.1, label = 'Seasons', size = 12, hjust=0, color = col_t)+
  labs(title = tit,
       subtitle = paste0(subt),
       caption = cap)+
  theme_void()+
  #curved text by suggestion Cara Thomson and https://cran.r-project.org/web/packages/geomtextpath/vignettes/curved_polar.html 
  geom_texthline(yintercept = 1, 
                   label = text_lab, size=10,
                   family ="Spectral",
                   hjust = -0.01, vjust = -0.5, 
                    color = col_t)+ 
  theme(text = element_text(family ="Spectral", colour = col_t, hjust = 0.5),
        plot.title = element_text(size = 230,
                                  margin = unit(c(0.8, 0, 0, 0), "cm")),
        plot.subtitle = element_text( size = 50),
        plot.caption = element_text(size = 70,
                                 margin = unit(c(0, 0.3, 1, 0), "cm"), 
                                 hjust = 0),

        axis.text.y=element_blank(),
        axis.text.x=element_blank(),
        plot.title.position = "plot",

        plot.background = element_rect(fill = "#D0CFCF", colour = NA),
        panel.background = element_rect(fill = "#D0CFCF", colour = NA))





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



                
                
                



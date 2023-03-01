#Tidy Tuesday week 9 
# Autor Ralitza Soultanova
# Please quote if you use the code - Ralitza Soultanova
# Feel free to connect on Twitter https://twitter.com/ralitza_s 
# LinkedIn https://www.linkedin.com/in/ralitzasoultanova/ 
#TidyTuesday  #r4ds #tidyverse #RStats #DataViz #TidyTuesday  @R4DSCommunity

rm(list = ls())
#set wd to file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#libraries
library(tidyverse)
library(fmsb)


#read data files

language_countries <- read_csv("language_countries.csv")
afrisenti <- read_csv("afrisenti.csv")
languages <- read_csv("languages.csv")



#how many tweets in Ethiopiain languages filters the "tweets" data frame for the three Ethiopian languages
afrisenti_et <- afrisenti %>% filter(language_iso_code %in% c("amh", "orm", "tir"))

# Data wrangling - count sentiment per language, create a % collumn 
tweets <- afrisenti %>% group_by(language_iso_code) %>%
  count(label) %>% pivot_wider(names_from = label, values_from = n) %>% 
  mutate(total_lan =  negative + neutral+ positive)%>% 
  mutate(Positive =round(positive*100/total_lan, digits=0), Neutral=round(neutral*100/total_lan, digits=0), Negative=round(negative*100/total_lan, digits=0), language=language_iso_code)%>%
  arrange(desc(Positive))%>% 
  group_by(language)%>%
  select(language, Positive, Neutral, Negative)

#create data frame for the 3 ethiopian languages 
ethiopia <- tweets %>% filter(language %in% c("amh", "orm", "tir")) %>% #filter for 3 languages in Ethiopia
            as.data.frame(ethiopia)

ethiopia2 <- ethiopia %>% select(Positive, Neutral, Negative) 
#add min and max row for the radar chart 
ethiopia2 <- rbind(c( 50, 50, 50), c( 20, 20, 20), ethiopia2) #languages "tir" "amh" "orm"

# Colors - plot and border  
coul = c("#32606f","#e41f28", "#f78d1f")
colors_border <- coul
colors_in <- alpha(coul,0.35)


#chart 
radarchart(ethiopia2, axistype=2,
           #custom polygon
           pcol=colors_border , pfcol=colors_in , plwd=1 , plty=1, #pcol - line color, pfcol - fill color, plwd - line width
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=c(10, 20, 30, 40, 50), cglwd=0.6,
           #custom labels
           vlcex=0.8 
)

#Do not like how the annotations turned on in R so finaly made them in Photoshop
mtext(side = 3, line = 2.5, at = 0, cex = 2, "Sentiment analysis - three Ethiopian languages", font = 2)
mtext(side = 1, line = 1, at = 0, cex = 1.25, "Repartition between positive, negative and neutral tweets in %. 
\nSentiment analysis done on 14 372 tweets by Shamsuddeen Hassan Muhammad.
\nviz: @Ralitza Soultanova, data: Shamsuddeen Hassan Muhammad", col = '#666664')
# Legend
legend(x=0.5, y=1, legend = c("Tigrinya", "Amharic", "Oromo"), bty = "n", pch=20 , col=colors_border , 
           text.col = colors_border, cex=1, pt.cex=1)


#save image 
ggsave("p.png",  
       width = 1900,
       height = 1900,
       units = "px",
       dpi = 300)
  
 

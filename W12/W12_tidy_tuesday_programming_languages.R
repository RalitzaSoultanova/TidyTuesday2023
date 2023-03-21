
##DATA DESCRIPTION 
#TidyTuesday w 12 2023 programming languages on git hub 
# Autor Ralitza Soultanova
# Please quote if you use the code
#TidyTuesday  #r4ds #tidyverse #RStats #DataViz #TidyTuesday  @R4DSCommunity
#description

#My github https://github.com/RalitzaSoultanova/TidyTuesday2023

#clean work environment 
rm(list=ls())
#packages 
  
  library(tidyverse)
  library(stringr)
  library(camcorder)
  library(ggtext)
  library(showtext) #add google fonts 
library(ggrepel)

#set working directory to file location 
setwd(dirname(rstudioapi::getActiveDocumentContext()))


# Get the data 
languages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-21/languages.csv')

# camcorder record plot - keeps the plot ration fixed 
gg_record(dir = "tidytuesday-temp", device = "png", width = 12, height = 12, units = "in", dpi = 300)


#tidy the df 

lang <- languages %>% 
  select("pldb_id" ,"title","appeared" ,"language_rank", "number_of_jobs", "github_language_repos", "number_of_users", "is_open_source","github_repo_stars")%>%
  mutate(users_1000= number_of_users/1000, jobs_1000 =number_of_jobs/1000)

#top 50 languages
lang_50 <- lang %>% arrange(number_of_users)%>% slice_max(number_of_users, n=50)
#top 5 languages for the annotation 
lang_5 <- lang_50 %>% arrange(jobs_1000) %>% slice_max(jobs_1000, n=5)


####################
#    plot          #
####################

#colors 
c_back <- "#1C1C1C"
c_text   <- '#00ff00' #ms_dos collor 
c_text_light <- "#878887"  


#fonts 
# load Google fonts (https://fonts.google.com/)
font_add_google("Poppins", "Poppins")
font_add_google("VT323", "VT323")
font_add_google("Jura", "Jura")

# Automatically use showtext to render text
showtext_auto()

#texts
tit  <- "Best market value - SQL, JavaScript, HTML"
subtit  <- "The top 10 most popular programming languages were all created before 2000! 
\nIf you want to learn a new language with a thriving community and job opportunities, 
\nconsider SQL, JavaScript, HTML, or Java."

cap  <-   "Visualisation - Ralitza Soultanova, data: Git Hub via Jesus Castagnetto, TidyTuesday"

#plot  
p <-ggplot(lang_50, aes(appeared, users_1000, size=jobs_1000))+
  geom_point(alpha=0.5, colour=c_text)+
  scale_size(range = c(.1, 23), name="Number of Jobs \n(in 1000)")+
  
  labs(title=tit, 
    subtitle = subtit,
    caption = cap,
    x = "Year of creation",
    y = "Number of users (in 1000)",) +
theme_minimal() +
theme(text = element_text(colour = c_text,
                          family = "Jura",
                          size=55),
                      
        plot.title = element_text(size = rel(2.5), 
                          family = "VT323",
                          hjust = 0,
                          margin = unit(c(0.9, 0, 1, 0), "cm")), 
      
         plot.subtitle = element_text(
                          family = "Jura",
                          lineheight = 0.3,
                          margin = margin(0, 0, 20, 0)),
                       
        plot.caption = element_text(hjust = 0,
                          size = rel(1),        
                          margin = unit(c(1,0,1, 0), "cm")),

        axis.text = element_text(color = c_text_light, 
                           size = rel(.8),
                           margin = unit(c(1,1,1, 0), "cm")),
        axis.title=element_text(color = c_text_light, 
                          size = rel(.8)),
        legend.key.size = unit(0.8, 'cm'),
        legend.text = element_text(size=rel(0.9)),
        legend.title = element_text(size=rel(0.9)),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = c_back, colour = NA),
        panel.background = element_rect(fill = c_back, colour = NA),
      plot.margin = unit(c(1,3,0.5,2), "cm")) #external margin around whole graph

p + geom_text(data=lang_5, 
              aes(label = title),
              color=c_text_light,
              size=17,
              nudge_x = 7)

####### Save image 
ggsave("languages.png",  width = 12)

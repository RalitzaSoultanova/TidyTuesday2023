##DATA DESCRIPTION 
#TidyTuesday w 11 2023 drugs
# Autor Ralitza Soultanova
# Please quote if you use the code
#TidyTuesday  #r4ds #tidyverse #RStats #DataViz #TidyTuesday  @R4DSCommunity

#My github https://github.com/RalitzaSoultanova/TidyTuesday2023
#For this week the majority of the time was spent in data cleaning. 
#Wanted to explore the distribution of approved drug between the companies but the data was quite messy
#Teva was per example present as Teva, TEVA Ba, Teva B.A., etc  

#clean work environment 
rm(list=ls())
#packages 
  
library(tidyverse)
library(readxl)
library(stringr)
library(camcorder)
library(ggtext)
library(showtext) #add google fonts 
library(lubridate)
library(tools)  
library(writexl)
library(png)
library(cowplot)

#set working directory to file location 
setwd(dirname(rstudioapi::getActiveDocumentContext()))


# Get the data  # downloaded from  www.ema.europa.eu

drugs <- read_xlsx('Medicines_output_european_public_assessment_reports.xlsx', .name_repair = function(col){ gsub(" ", "_", col) } )

drugs <- as.data.frame(drugs)

# camcorder record plot - keeps the plot ration fixed 
gg_record(dir = "tidytuesday-temp", device = "png", width = 12, height = 12, units = "in", dpi = 300)

#select only human , approved drugs  
drugs <- drugs %>% filter(Category =="Human" &  Authorisation_status == "Authorised")

#clean company names 
drugs$company <- str_to_title(drugs$company) #first capital

#remouve leading and trailing whitespaces 
drugs <- drugs %>% 
  mutate(company_s = str_trim(company, "right")) %>% 
  mutate(company_s = str_trim(company_s, "left")) %>%
  mutate(company_1=sub("(\\w+\\s+).*", "\\1", company_s)) %>% #new column first word company
  mutate(company_2=sub("(\\w+\\s+\\w+).*", "\\1", company_s))  #new column with 2 first words in title
  

  #export to excell and manualy chose to keep 1 or 2 words title In some cases write manualy write another title
  write_xlsx(drugs, "drugs2.xlsx")
  
  
#import back the excell file   com_f - final company name 
drugs_n <- read_xlsx("drugs3.xlsx")


# extract year from year date
drugs_n <-  drugs_n %>% mutate(year = year(Decision_date)) %>% drop_na(year) 
  
#only new drugs 
#approved drugs by company 
  

by_com <- drugs_n %>%
  group_by(com_f)%>%
  count() %>%
  arrange(desc(n))%>%
  ungroup()

#number of drugs by top 6 companies 365 - 28%
# total number drugs 1315 
top_6 <- by_com %>% arrange(desc(n)) %>% slice_max(n, n=6)# %>%summarise(sum(n))

#export the distribution per company file
write_xlsx(by_com, "by_com.xlsx")


# bubble plot made in tableau
img <- readPNG("companies.png")

#colors 
c_back <- "grey"
c_text   <- '#033854'


#fonts 
# load Google fonts (https://fonts.google.com/)
font_add_google("Geneva", "Geneva")

# Automatically use showtext to render text
showtext_auto()

#texts
tit  <- "28% of Europe's Drug Authorizations are Concentrated Among 6 Companies"
subtit  <- "The European Medicines Agency plays a critical role in the approval process for new drugs in  
            \nEurope, particularly for diseases such as HIV, AIDS, cancer, diabetes, neurodegenerative diseases, 
             \nHowever, it's important to note that some medications may be approved at the national level."
cap  <-   "Visualisation - Ralitza Soultanova, data: European Medicines Agency, March 2023"

#plot  
### combination of plots with ggdraw 
# cowplot article  https://meghan.rbind.io/blog/cowplot/

ggdraw() +
  #backgraound image
  theme(plot.background = element_rect(fill=c_back),
        plot.margin = margin(1, 1, 1, 1))+
  draw_label(tit,
             fontfamily = "Geneva", size = 20,
             x = 0.5, y=0.94, hjust = 0.5, color=c_text) +
  draw_label(subtit, 
             fontfamily = "Geneva", size = 15,
             x = 0.13, y=0.87, hjust = 0, 
             lineheight = .5,
             color=c_text) +
  draw_label(cap,
             fontfamily = "Geneva", 
             size = 15,
             y = 0.02, x = 0.5, hjust = 0.5, 
             colour = c_text)+
  # image
draw_image(img, x = .12, y = -.06,  width = 0.75)
  
  
#plot.background = element_rect(fill = c_back, colour = NA),
#panel.background = element_rect(fill = c_back, colour = NA))

####### Save image 
ggsave("companies.png",  width = 12, height = 12, units = "cm", dpi = 300)

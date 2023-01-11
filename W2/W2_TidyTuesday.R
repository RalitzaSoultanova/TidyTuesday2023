#Project FeederWatch.
#https://feederwatch.org/explore/raw-dataset-requests/
#FeederWatch is a November-April survey of birds that visit backyards, nature centers, community areas, and other locales in North America. 
#Citizen scientists could birds in areas with plantings, habitat, water, or food that attracts birds. The schedule is completely flexible. 
#People count birds as long as they like on days of their choosing, then enter their counts online. This allows anyone to track what is happening to birds 
#around your home and to contribute to a continental data-set of bird distribution and abundance.

#FeederWatch data show which bird species visit feeders at thousands of locations across the continent every winter. 
#The data also indicate how many individuals of each species are seen. 
#This information can be used to measure changes in the winter ranges and abundances of bird species over time.


#packages 
library(dplyr)
library(tidyverse)
library(maps)   # Longititude and longitude data for various maps
library(skimr)
library(readxl)
library(RColorBrewer)

#set working directory to file location 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Get the Data
# Read in with tidytuesdayR package 

data_full <- tidytuesdayR::tt_load('2023-01-10')
View(data_full)

#only public table
df <- data_full$PFW_2021_public


#save the data frame locally as not to download every time 
save(df, file="df_birds.Rda")

#quick inspection of the data skim() in place of summary()
skim(df)


# tudy the data 
# remouve non valid observations - valid=1 is a valid observation

df <- df %>% 
    filter(valid==1)


#separate the subnational code into state and country, filter only US data keep only relevant collumns, keep on ly usefull collums 

df_us <- df %>% separate(subnational1_code, into = c("country", "state"), sep = "-") %>% 
  filter(!country %in% c("XX", "CA"))  %>% 
  select("loc_id", "latitude", "longitude", "state")

#unique observation stations 
df_unloc <- df_us %>% distinct(loc_id, .keep_all = TRUE)

#remouve an observation station with no state data
per_loc <- df_unloc %>% 
  group_by(loc_id, state) %>%
  filter(loc_id!="L1336896")

#summary number of observation stations locations per state 

obs_st <- per_loc %>% 
  group_by(state) %>% 
  count(state, sort=TRUE) 


# load United States state map data
data_states <- map_data("state")


# state population data
state_pop <- read.csv("https://raw.githubusercontent.com/ds4stats/r-tutorials/master/intro-maps/data/StatePopulation.csv", as.is = TRUE)

#exported to csv and merged in Excell (I'm sure not the best way to do it . Have a look for alternative)
write.csv(state_pop, file='state_pop.csv')
write.csv(obs_st, file='obs_st.csv')

obs_pop <- read_excel("stations_pop.xlsx")

# data_states and StatePopulation files
MergedStates <- inner_join(data_states, obs_pop, by = "region")

#Most popular place for project 
n_max <- obs_pop %>% 
  group_by(region) %>%
  arrange(desc(stations_mil_pop))

#Least popular place for project
n_min <- obs_pop %>% 
  group_by(region) %>%
  arrange(stations_mil_pop)

#########
#Plot 

# colors 
pal <- c('#C5FDD3', '#94E1B4', '#69C5A0', '#45A994', '#288D8A', '#126171', '#033854')
#background color 
back <- "#292929"
# Create a Choropleth map of the United States

p <- ggplot()
p <- p + geom_polygon( data=MergedStates, 
                       aes(x=long, y=lat, group=group, fill =stations_mil_pop ), 
                       color="white", size = 0.2) 

p <- p + scale_fill_gradient(low = "#94E1B4", high = "#033854", 
                       # name="N stations (pop in millions)", 
                        limits = c(0,700))
#create anotations layer - min max populatiry 

#find the coordinates Wioming for the annotation 
MergedStates %>% 
  filter(state=="WI")


p <- p + labs(
  title="Popularity of Project FeederWatch in the United States",
  subtitle= "Number of observation stations per 1M of population", 
  caption= "@Ralitza Soultanova, data: Project FeederWatch, TidyTuesday")+
  theme_void() + 
  theme(legend.position ="none", 
        text = element_text (color='#45A994', hjust=0.5),
        plot.title= element_text(hjust=0.5, size=20),
        plot.subtitle= element_text(hjust=0.5, size=15),
        plot.background = element_rect(fill = back, colour = NA),
        plot.caption = element_text(hjust=0.5, size=15),
        plot.margin = margin(30, 30, 40, 30)
  ) 

p <- p + 
  annotate("text", x = -107.5, y = 43, label = "max popularity \nWyoming - 630\nstations per M", colour ='#45A994', size=4, hjust=0.5)+
  annotate("text", x = -88, y = 33, label = "min popularity \nMississippi - 5\nstations per M", colour ="#033854", size=4, hjust=0.5)
p

####### Save image 
ggsave("stations.png",  width = 12)

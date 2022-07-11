# author: chao xu
# date: 11/07/2022
# Project for junior data scientist position 

# to set working directory
setwd("/Users/chaoxu/Desktop/Paris/chao codes/worldbank")

# to load the library
library(tidyverse)
library(readr) # to import the csv file (read_csv)
library(dplyr) # to rename the column
library(foreign)# to read spss data
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(ggridges)
library(viridisLite)
library(gglabeller)
library(scales)
library(ggmap)
library(knitr)
library(sf)
library(tigris)
library(lubridate)


# to load the data

# immigration data since 1996
border <- read_csv("/Users/chaoxu/Desktop/Paris/chao codes/AFP/Border_Crossing_Entry_Data.csv")
# data source: https://catalog.data.gov/dataset/border-crossing-entry-data
head(border)
table(border$State) # US-Canada Border US-Mexico Border
table(border$Border)
border$Date <- my(border$Date) # "1996-01-01" "2022-03-01"
# to check the immigrants trend by year and border
# filter out the data yeared 2022
border_year_state <- border %>%
  mutate(Year = year(Date)) %>%
  group_by(Border, Year, State) %>%
  summarise(n_year = sum(Value)) %>%
  filter(Year != 2022)

# to see the trends
ggplot(border_year_state, aes(x=Year,y=n_year, color=State))+
  geom_line()+
  facet_wrap(.~Border, ncol = 2)+
  scale_y_continuous(labels = comma)+
  labs(x="Year", y="Number", 
       title="Annual Number of Immigrants to U.S. (1996 - 2021)",
       caption = "U.S. Customs and Border Protection (CBP) \n Visualized by Chao Xu")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, vjust = 1.0, size = 14),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, size = 10),
        legend.position="right",
        legend.title= element_text(size=10),
        legend.text = element_text(size=8),
        axis.text.x = element_text(color = "grey20", size = 8, angle = 0, hjust = 0, vjust = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 8, angle = 0, hjust = 0, vjust = 0, face = "plain"))

# to load the second data set
# MISSING MIGRANTS PROJECT since 2014
missing <- read_csv("/Users/chaoxu/Desktop/Paris/chao codes/AFP/Missing_Migrants_Global_Figures_allData.csv")

# to get the lat and lon data
head(missing)
colnames(missing) <- make.names(colnames(missing))
missing_mexico <- missing %>%
  mutate(Coordinates=str_replace(Coordinates, "POINT \\(", ""))%>%
  mutate(lon=str_replace(Coordinates, " .*", ""))%>%
  mutate(lat=str_replace(Coordinates, ".* ", ""))%>%
  mutate(lat=str_replace(lat, "\\)", "")) %>%
  mutate(lon = as.numeric(lon), lat=as.numeric(lat))%>%
  filter(Migration.route == "US-Mexico border crossing") %>%
  filter(Incident.year !=2022)

# to get the backgroud map
table(border$State)
state_var <- c("Arizona", "California", "New Mexico","Texas")

options(tigris_class = "sf")
states <- states(cb=T) 
states_interest <- states %>%
  filter(NAME %in% state_var)

# 
ggplot(states_interest) + 
  geom_sf()+
  geom_point(data=missing_mexico, aes(x=lon, y=lat,size=Number.of.Dead, 
                                      color=Region.of.Origin), fill="white", 
             shape=1) +
  theme_void() +
  theme(panel.grid.major = element_line()) +
  coord_sf(xlim = c(-125, -90), ylim = c(22, 42))+
  labs(title = "Incidents at the US-Mexico Border between 2014 to 2021",
       size = "Number of deaths",
       caption = "Source: US DOT and International Organization for Migration")+
  theme_void()+
  theme(panel.grid.major = element_line(colour = 'transparent'),
        plot.title = element_text(hjust = 0.5, vjust = 0.0, size = 14),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.0, size = 10),
        legend.position = "top")+
  facet_wrap(~Incident.year)

ggplot(states_interest) + 
  geom_sf()+
  geom_point(data=missing_mexico, aes(x=lon, y=lat,size=Number.of.Dead, 
                                      color=Region.of.Origin), fill="white", 
             shape=1) +
  theme_void() +
  theme(panel.grid.major = element_line()) +
  coord_sf(xlim = c(-125, -90), ylim = c(22, 42))+
  labs(title = "Incidents at the US-Mexico Border between 2014 to 2021",
       size = "Number of deaths",
       caption = "Source: US DOT and International Organization for Migration")+
  theme_void()+
  theme(panel.grid.major = element_line(colour = 'transparent'),
        plot.title = element_text(hjust = 0.5, vjust = 0.0, size = 14),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.0, size = 10),
        legend.position = "top")+
  facet_wrap(~Incident.year)


missing_mexico_death <- missing_mexico %>%
  group_by(Incident.year, Cause.of.Death) %>%
  summarise(n=n(), n_death=sum(Number.of.Dead,na.rm = T))
missing_mexico_death$Cause.of.Death[missing_mexico_death$Cause.of.Death==
                                      "Harsh environmental conditions / lack of adequate shelter, food, water"]="Harsh environment"
missing_mexico_death$Cause.of.Death[missing_mexico_death$Cause.of.Death==
                                      "Sickness / lack of access to adequate healthcare"]="Sickness"
missing_mexico_death$Cause.of.Death[missing_mexico_death$Cause.of.Death==
                                      "Vehicle accident / death linked to hazardous transport"]="Vehicle accident"

# to check the death trend by cause
ggplot(missing_mexico_death, aes(x=Incident.year, y=n_death))+
  geom_line()+
  facet_wrap(~Cause.of.Death)+
  labs(x="Year", y="Number", 
       title="Annual Number of Death by Cause at US-Mexico Border (2014 - 2021)",
       caption = "Source: International Organization for Migration ")+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1.0, size = 14),
                        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, size = 10),
                        legend.position="right",
                        legend.title= element_text(size=10),
                        legend.text = element_text(size=8),
                        axis.text.x = element_text(color = "grey20", size = 8, angle = 0, hjust = 0, vjust = 0, face = "plain"),
                        axis.text.y = element_text(color = "grey20", size = 8, angle = 0, hjust = 0, vjust = 0, face = "plain"))


# to check the locations of the three causes
# Drowing, Harsh environmental conditions, Mixed or unknown
missing_mexico$Cause.of.Death[missing_mexico$Cause.of.Death==
                                      "Harsh environmental conditions / lack of adequate shelter, food, water"]="Harsh environment"
missing_mexico$Cause.of.Death[missing_mexico$Cause.of.Death==
                                      "Sickness / lack of access to adequate healthcare"]="Sickness"
missing_mexico$Cause.of.Death[missing_mexico$Cause.of.Death==
                                      "Vehicle accident / death linked to hazardous transport"]="Vehicle accident"

missing_mexico_3types$Cause.of.Death
missing_mexico_3types <-missing_mexico %>%
  filter(Cause.of.Death==c("Drowning","Harsh environment","Mixed or unknown")) %>%
  filter(Incident.year %in% c(2018:2021))

ggplot(states_interest) + 
  geom_sf()+
  geom_point(data=missing_mexico_3types, aes(x=lon, y=lat,size=Number.of.Dead, 
                                      color=Cause.of.Death), fill="white", 
             shape=1) +
  theme_void() +
  theme(panel.grid.major = element_line()) +
  coord_sf(xlim = c(-125, -90), ylim = c(22, 42))+
  labs(title = "Fatal Incidents by cause at the US-Mexico Border (2018-2021)",
       size = "Number of deaths",
       caption = "Source: International Organization for Migration")+
  theme_void()+
  theme(panel.grid.major = element_line(colour = 'transparent'),
        plot.title = element_text(hjust = 0.5, vjust = 1.0, size = 14),
        legend.position = "right")+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
  facet_wrap(~Incident.year, ncol = 2)



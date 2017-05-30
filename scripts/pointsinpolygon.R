# Calculating shootings and police calls to specific census tracts

library(readr)
library(lubridate)
library(dplyr)
library(stringr)
library(tidycensus)
library(tidyverse)
source("keys.R")
source("theme_nothing.R")
library(tigris)
#library(sf)
library(viridis)
library(ggmap)
library(leaflet)
library(DT)

# Data is from the Jersey City open data portal
police_calls <- read_csv("http://data.jerseycitynj.gov/dataset/8754c180-b5e3-408a-b1f5-385d8edefb1d/resource/bf37145d-7282-4271-92b8-db4c92b04dd8/download/jcpd-all.csv")
shootings <- read_csv("http://data.jerseycitynj.gov/dataset/a321323d-930c-4888-b6ad-6e8f112403fe/resource/adbbf3ca-0408-42cf-802e-1be3f5f01092/download/jerseycity2015shootings-use.csv")

# Bringing in the shapefile for census tracts in Hudson county
nj_h <- tracts("NJ", county="Hudson", cb=F)

# Turning the shapefile into a dataframe
nj_hf <- fortify(nj_h, region="GEOID")

# Isolating the coordinates of the police calls
coords <- police_calls[c("LONGITUDE", "LATITUDE")]

# Telling R these are spatial coordinates
sp <- SpatialPoints(coords)

# Isolating the coordinates of the police shootings
coords2 <- shootings[c("longitude", "latitude")]

# Telling R these are spatial coordinates
sp2 <- SpatialPoints(coords2)

# Plotting out the shapefile of the census tracts and shootings and police calls
plot(nj_h)
plot(sp, col="red", add=TRUE)
plot(sp2, col="blue", add=TRUE)

# Cleaning up the dates in the shootings data
shootings$datetime <- mdy_hm(shootings$datetime)

# The long process of cleaning up the dates in the police calls data
police_calls$datetime <- mdy_hm(police_calls$`Time Received`)

police_calls$blah <- ifelse(is.na(police_calls$datetime),police_calls$`Time Received`, "" )

police_calls$datetime2 <- mdy_hms(police_calls$blah)

police_calls$datetime3 <- ifelse(is.na(police_calls$datetime),as.character(police_calls$datetime2), as.character(police_calls$datetime))
police_calls$datetime3 <- ymd_hms(police_calls$datetime3)

# Extracting the years from the police calls data
police_calls$year <- year(police_calls$datetime3)
police_calls_2015 <- subset(police_calls, year==2015)

# Getting rid of the duplicate police calls
police_calls_2015 <- police_calls_2015 [!duplicated(police_calls_2015$`Event Number`),]

# Summarizing the types of codes
code2015 <- police_calls_2015 %>% group_by(`Call Code Description`, Priority) %>% summarize(total=n()) %>% arrange(desc(total))
code2015 <- police_calls_2015 %>% group_by(`Call Code Description`) %>% summarize(total=n()) %>% arrange(desc(total))
callcode <- police_calls_2015 %>% group_by(CALLCODE) %>% summarize(total=n()) %>% arrange(desc(total))

# Writing out the call code data to determine which ones to focus on
write.csv(code2015, "data/codes2015.csv")
write.csv(callcode, "data/callcode.csv")

# Another way to look at the code calls
prio1_2015 <- subset(police_calls_2015, Priority==1)
prio1_2015 <- unique(prio1_2015$`Event Number`)

# These were the calls we decided to focus on
calls <- c("BURG ALARM COMMERCIAL PROP", "ASSAULT NO WEAPON", "USE/SALE OF DRUGS", "THEFT PROP FROM VEHICLE", "MOTOR VEHICLE THEFT", "THEFT FROM PERSON",
           "ROB COMERCIAL", "FOUND CDS/PARAPHERNALIA", "HOMICIDE")

police_calls_2015_sub <- police_calls_2015 %>%
  filter(str_detect(`Call Code Description`, "BURG ALARM COMMERCIAL PROP|ASSAULT NO WEAPON|USE/SALE OF DRUGS|THEFT PROP FROM VEHICLE|MOTOR VEHICLE THEFT|THEFT FROM PERSON|ROB COMERCIAL|FOUND CDS/PARAPHERNALIA|HOMICIDE"))

police_calls_2015_sub$type <- gsub(";.*", "", police_calls_2015_sub$`Call Code Description`)
police_calls_2015_sub <- select(police_calls_2015_sub, type, date=datetime3, lat=LATITUDE, lon=LONGITUDE)
shootings_2015_sub <- shootings
shootings_2015_sub$shooting_type <- paste("Shooting:", shootings_2015_sub$shooting_type)
shootings_2015_sub <- select(shootings, type=shooting_type, date=datetime, lat=latitude, lon=longitude)

# Combining calls with shootings
sub_2015 <- rbind(police_calls_2015_sub, shootings_2015_sub)

# Isolating the coordinates again
coords <- sub_2015[c("lon", "lat")]

sp <- SpatialPoints(coords)

# Plotting again
plot(nj_h)
plot(sp, col="red", add=TRUE)

# Specifying the projection for the coordinates
proj4string(sp) <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
proj4string(sp)

# THIS IS the points in polygon function
by_tract <- over(sp, nj_h)

# Joining the data frames together
sub_2015 <- cbind(sub_2015, by_tract)

# Summarizing the calls/shootings by census tract
tract_totals <- sub_2015 %>%
  group_by(GEOID, NAMELSAD, type) %>%
  summarize(total=n())

# Figuring out the percent per tract
tract_totals <- tract_totals %>%
  group_by(type) %>%
  mutate(percent=round(total/sum(total, na.rm=T)*100,2))

# cleaning up some names
tract_totals$type <- ifelse(tract_totals$type=="Fatal", "Shooting: Fatal", tract_totals$type)
tract_totals$type <- ifelse(tract_totals$type=="Non-fatal", "Shooting: Non-fatal", tract_totals$type)

tract_totals <- filter(tract_totals, !is.na(GEOID))

# These are the tracts specific to the development
proj1 <- c("34017004400", "34017004500", "34017004600", "34017005200", "34017005300", "34017005500", "34017005600", "34017005801", "34017006000", "34017006100", "34017006200", "34017006400", "34017006500", "34017006700", "34017006800", "34017007600")

# Filtering the data specific to the project tracts
tract_totals_a  <- filter(tract_totals, GEOID %in% proj1)

# Splitting up the data between shootings and calls
shootings_tracts <- filter(tract_totals_a, type=="Shooting: Fatal" | type=="Shooting: Non-fatal")
tract_totals_a <- filter(tract_totals_a, type!="Shooting: Fatal" & type!="Shooting: Non-fatal")

# bringing in the map for census tracts in Hudson county
nj_h <- tracts("NJ", county="Hudson", cb=F)
nj_hf <- fortify(nj_h, region="GEOID")


nj_tracts <- left_join(nj_hf, tract_totals_a, by=c("id"="GEOID"))

#nj_tracts <- filter(nj_tracts, !is.na(year))
nj_tracts <- nj_tracts[!is.na(nj_tracts$percent),]

#nj_tracts$year <- gsub("ur_", "", nj_hud$year)

# mapping!

nj_map <- ggplot()
#nj_map <- nj_map + geom_polygon(data=nj_hf, aes(x=long, y=lat, group=group), fill=NA, color="black", size=.1)
nj_map <- nj_map + geom_polygon(data=nj_tracts, aes(x=long, y=lat, group=group, fill=percent), color="black", size=.5)
nj_map <- nj_map + facet_wrap(~type, ncol=2)
nj_map <- nj_map + coord_map() 
nj_map <- nj_map + scale_fill_viridis(option = "heat", direction=-1, name = "Percent of crime calls")
nj_map <- nj_map + scale_color_viridis(option = "heat", direction=-1)
nj_map <- nj_map + theme_nothing(legend=TRUE) 
nj_map <- nj_map + labs(x=NULL, y=NULL, title="Percent of crime calls in 2015")
nj_map <- nj_map + theme(panel.grid.major = element_line(colour = NA))
nj_map <- nj_map + theme(text = element_text(size=15))
nj_map <- nj_map + theme(plot.title=element_text(face="bold", hjust=.4))
nj_map <- nj_map + theme(plot.subtitle=element_text(face="italic", size=9, margin=margin(l=20)))
nj_map <- nj_map + theme(plot.caption=element_text(size=12, margin=margin(t=12), color="#7a7d7e", hjust=0))
nj_map <- nj_map + theme(legend.key.size = unit(1, "cm"))
nj_map <- nj_map + annotate("segment", x = -74.03577, xend = -74.005, y = 40.72, yend = 40.72, colour = "tomato", size=.5) 
nj_map <- nj_map + annotate("point", x = -74.03577, y = 40.72, colour = "lightblue", size = 1) 
nj_map <- nj_map + annotate("text", x = -73.99217, y = 40.72, label = "65 Bay Street", size=3, colour="gray30") 

print(nj_map)
datatable(tract_totals_a[,2:5])


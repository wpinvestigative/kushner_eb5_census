library(rgdal)
library(rgeos)
library(tidyverse)
library(tigris)
library(ggplot2)
library(geosphere)
library(tidycensus)
source("../scripts/keys.R")
library(viridis)
library(ggmap)
library(DT)
library(leaflet)
library(readr)
library(lubridate)
library(dplyr)
library(stringr)
library(sp)
library(knitr)
source("keys.R")

# Brings in a census API key from the keys.R script (which is hidden)
# Sign up here: http://api.census.gov/data/key_signup.html

census_api_key(key)

# Alternatively: census_api_key("your_key_goes_here")

# Pulling the shapefile for New Jersey's Hudson county census tracts
nj_h <- tracts("NJ", county="Hudson", cb=F)

# Figuring out the latitude and longitudes for the center of each census tract
nj_h_centroids <-  SpatialPointsDataFrame(gCentroid(nj_h, byid=TRUE), 
                                          nj_h@data, match.ID=FALSE)

nj_h_centroids <- as.data.frame(nj_h_centroids)
nj_h_centroids <- select(nj_h_centroids, GEOID, x, y)


# Setting up some blank columns
nj_h_centroids$distance_proj1 <- 0
nj_h_centroids$distance_proj2 <- 0

# Determining the distance in miles between the project site and the center of each census tract in Hudson county
for (i in 1:nrow(nj_h_centroids)) {
  nj_h_centroids$distance_proj1[i] <- distm (c(-74.063644, 40.734330), c(nj_h_centroids$x[i], nj_h_centroids$y[i]), fun = distHaversine)[,1]/ 1609
  nj_h_centroids$distance_proj2[i] <- distm (c(-74.03577, 40.72), c(nj_h_centroids$x[i], nj_h_centroids$y[i]), fun = distHaversine)[,1]/ 1609
}

# 1 Journal Square (2017)

# Census tracts included by planners in this project
# GEOIDs: 34017001900, 34017004600, 34017005300, 34017006600, 34017006700, 34017007100

# Census tracts immediately adjacent to the project
# GEIODs: 19, 20 18, 17, 9.02, 12.02, 71

# Getting the unemployment figures and total labor workforce for all tracts in Hudson county
hudson_unemployment_2013 <- get_acs(geography="tract", endyear=2013, variables= c("B23025_005E", "B23025_002E"), county = "Hudson", state="NJ")

# Why 2009-2013? That's the ACS 5-year data that developers used to justify their project to officials, according to documents

# Discarding the margin of error
hudson_unemployment_2013$moe <- NULL

# Tidying up the structure of the new data frame
hudson_unemployment_2013 <- spread(hudson_unemployment_2013,variable, estimate)

# Calculating the unemployment rate
hudson_unemployment_2013$per_un <- round(hudson_unemployment_2013[,4]/hudson_unemployment_2013[,3]*100,2)

# Filtering out the blank census tracts
hudson_unemployment_2013 <- filter(hudson_unemployment_2013, GEOID!="34017006900" & GEOID!="34017980100")

# Array of group-designated census tracts
proj1 <- c("34017001900", "34017004600", "34017005300", "34017006600", "34017006700", "34017007100")

# Filtering out the tracts data specific to the project
hudson_unemployment_2013_sm <- filter(hudson_unemployment_2013, GEOID %in% proj1)

# Cleaning up the data frame
colnames(hudson_unemployment_2013_sm) <- c("GEOID", "name", "total", "unemployed", "unemp_rate")
hudson_unemployment_2013_sm$un_rate <- hudson_unemployment_2013_sm$unemp_rate$B23025_005
hudson_unemployment_2013_sm$unemp_rate <- NULL

# Adding a label column
hudson_unemployment_2013_sm$radius <- "gerrymandered"

# Array of census tracts immediately adjacent to the project
proj1_half <- c("34017001900", "34017002000", "34017001800", "34017001700", "34017000902", "34017001202", "34017007100")

# Filtering out the tracts data specific to the project
hudson_unemployment_2013_sm_half <- filter(hudson_unemployment_2013, GEOID %in% proj1_half)

# Cleaning up the data frame
colnames(hudson_unemployment_2013_sm_half) <- c("GEOID", "name", "total", "unemployed", "unemp_rate")
hudson_unemployment_2013_sm_half$un_rate <- hudson_unemployment_2013_sm_half$unemp_rate$B23025_005
hudson_unemployment_2013_sm_half$unemp_rate <- NULL

# Adding a label column
chudson_unemployment_2013_sm_half$radius <- "adjacent tracts"

# Combining the two census tracts data frames
hudson_unemployment_sm <- rbind(hudson_unemployment_2013_sm, hudson_unemployment_2013_sm_half)

# Cleaning up the census tract names
hudson_unemployment_sm$name <- gsub(",.*", "", hudson_unemployment_sm$name)

# More cleaning
hudson_unemployment_2013$un_rate <- hudson_unemployment_2013$per_un$B23025_005
hudson_unemployment_2013$per_un <- NULL

# Prepping the shape file to be joined with data
# (This process is necessary when visualizing with ggplot)
nj_hf <- fortify(nj_h, region="GEOID")

# Joining census data of project to shape file
nj_hud <- left_join(nj_hf, hudson_unemployment_sm, by=c("id"="GEOID"))

# Filter out the census tracts with no data
nj_hud <- filter(nj_hud, !is.na(radius))

# Joining census data of all tracts in county to shape file
nj_all <- left_join(nj_hf, hudson_unemployment_2013, by=c("id"="GEOID"))

# Slicing up the unemployment data (for all tracts) into buckets
nj_all$nj_mid <- cut(nj_all$un_rate, breaks = seq(0, 30, by=5))

# colors <- colorRampPalette(c("white", "red"))(length(levels(nj_all$nj_mid)))

# Slicing up the unemployment data (for project tracts) into buckets
nj_hud$nj_mid <- cut(nj_hud$un_rate, breaks = seq(0, 30, by=5))

#colors <- colorRampPalette(c("white", "red"))(length(levels(nj_hud$nj_mid)))

# Mapping project-only census tract data
nj_map <- ggplot()
nj_map <- nj_map + geom_polygon(data=nj_hud, aes(x=long, y=lat, group=group, fill=factor(nj_mid)), color="black", size=.5)
nj_map <- nj_map + facet_wrap(~radius)
nj_map <- nj_map + coord_map() 
nj_map <- nj_map + scale_fill_manual(drop=FALSE, values=c("blue", "yellow", "orange", "red", "green", "purple"), na.value="#EEEEEE", name="Unemployment rate")
nj_map <- nj_map + theme_nothing(legend=TRUE) 
nj_map <- nj_map + labs(x=NULL, y=NULL, title="1 Journal Square (2017)")
nj_map <- nj_map + theme(panel.grid.major = element_line(colour = NA))
nj_map <- nj_map + theme(text = element_text(size=15))
nj_map <- nj_map + theme(plot.title=element_text(face="bold", hjust=.4))
nj_map <- nj_map + theme(plot.subtitle=element_text(face="italic", size=9, margin=margin(l=20)))
nj_map <- nj_map + theme(plot.caption=element_text(size=12, margin=margin(t=12), color="#7a7d7e", hjust=0))
nj_map <- nj_map + theme(legend.key.size = unit(1, "cm"))
nj_map <- nj_map + annotate("segment", x = -74.063644, xend = -74.035, y = 40.734330, yend = 40.734330, colour = "tomato", size=.5) 
nj_map <- nj_map + annotate("point", x = -74.063644, y = 40.734330, colour = "lightblue", size = 1) 
nj_map <- nj_map + annotate("text", x = -74.01, y = 40.734330, label = "1 Journal Square", size=3, colour="gray30") 

print(nj_map)

# Saving as a pdf
ggsave("map1a.pdf", nj_map, device="pdf")

# Mapping all census tract borders in the county and layering on the project-only census tract data
nj_map <- ggplot()
nj_map <- nj_map + geom_polygon(data=nj_hf, aes(x=long, y=lat, group=group), fill=NA, color="black", size=.1)
nj_map <- nj_map + geom_polygon(data=nj_hud, aes(x=long, y=lat, group=group, fill=factor(nj_mid)), color="black", size=.5)
nj_map <- nj_map + facet_wrap(~radius)
nj_map <- nj_map + coord_map() 
nj_map <- nj_map + scale_fill_manual(drop=FALSE, values=c("blue", "yellow", "orange", "red", "green", "purple"), na.value="#EEEEEE", name="Unemployment rate")
nj_map <- nj_map + theme_nothing(legend=TRUE) 
nj_map <- nj_map + labs(x=NULL, y=NULL, title="1 Journal Square (2017)")
nj_map <- nj_map + theme(panel.grid.major = element_line(colour = NA))
nj_map <- nj_map + theme(text = element_text(size=15))
nj_map <- nj_map + theme(plot.title=element_text(face="bold", hjust=.4))
nj_map <- nj_map + theme(plot.subtitle=element_text(face="italic", size=9, margin=margin(l=20)))
nj_map <- nj_map + theme(plot.caption=element_text(size=12, margin=margin(t=12), color="#7a7d7e", hjust=0))
nj_map <- nj_map + theme(legend.key.size = unit(1, "cm"))
nj_map <- nj_map + annotate("segment", x = -74.063644, xend = -74.035, y = 40.734330, yend = 40.734330, colour = "tomato", size=.5) 
nj_map <- nj_map + annotate("point", x = -74.063644, y = 40.734330, colour = "lightblue", size = 1) 
nj_map <- nj_map + annotate("text", x = -74.01, y = 40.734330, label = "1 Journal Square", size=3, colour="gray30") 

print(nj_map)

# Saving output as a pdf
ggsave("map1b.pdf", nj_map, device="pdf")

# Mapping all census tract data in the county and layering on the project-only census tract data
nj_map <- ggplot()
nj_map <- nj_map + geom_polygon(data=nj_all, aes(x=long, y=lat, group=group, fill=factor(nj_mid)), color="gray", size=.3)
nj_map <- nj_map + geom_polygon(data=nj_hud, aes(x=long, y=lat, group=group, fill=factor(nj_mid)), color="black", size=.6)
nj_map <- nj_map + facet_wrap(~radius)
nj_map <- nj_map + coord_map() 
nj_map <- nj_map + scale_fill_manual(drop=FALSE, values=c("blue", "yellow", "orange", "red", "green", "purple"), na.value="#EEEEEE", name="Unemployment rate")
nj_map <- nj_map + theme_nothing(legend=TRUE) 
nj_map <- nj_map + labs(x=NULL, y=NULL, title="1 Journal Square (2017)")
nj_map <- nj_map + theme(panel.grid.major = element_line(colour = NA))
nj_map <- nj_map + theme(text = element_text(size=15))
nj_map <- nj_map + theme(plot.title=element_text(face="bold", hjust=.4))
nj_map <- nj_map + theme(plot.subtitle=element_text(face="italic", size=9, margin=margin(l=20)))
nj_map <- nj_map + theme(plot.caption=element_text(size=12, margin=margin(t=12), color="#7a7d7e", hjust=0))
nj_map <- nj_map + theme(legend.key.size = unit(1, "cm"))
nj_map <- nj_map + annotate("segment", x = -74.063644, xend = -74.035, y = 40.734330, yend = 40.734330, colour = "tomato", size=.5) 
nj_map <- nj_map + annotate("point", x = -74.063644, y = 40.734330, colour = "lightblue", size = 1) 
nj_map <- nj_map + annotate("text", x = -74.01, y = 40.734330, label = "1 Journal Square", size=3, colour="gray30") 

print(nj_map)
ggsave("map1c.pdf", nj_map, device="pdf")

# Summarizing the table
# Unemployment rate for the census tracts picked by the project group
# versus unemployment rate for census tracts only adjacent to the project

sm_table <- hudson_unemployment_sm %>%
group_by(radius) %>%
summarize(average_unemployment=round(mean(un_rate, na.rm=T),2), median_unemployment=round(median(un_rate, na.rm=T),2)) %>%
arrange(average_unemployment)

kable(sm_table)


## OK, SAME AS ABOVE BUT NOW FOR A NEW PROJECT SITE

# 65 Bay Street (2015)

# Census tracts included by planners in this project
# GEOIDs: 34017004400, 34017004500, 34017004600, 34017005200, 34017005300, 34017005500, 34017005600, 34017005801, 34017006000, 34017006100, 34017006200, 34017006400, 34017006500, 34017006700, 34017006800, 34017007600 

# Census tracts immediately adjacent to the project
# GEOIDS: 77, 78, 70, 64, 75, 74, 76

# Getting the unemployment figures and total labor workforce for all tracts in Hudson county

hudson_unemployment_2012 <- get_acs(geography="tract", endyear=2012, variables= c("B23025_005E", "B23025_002E"), county = "Hudson", state="NJ")

# Why 2008-2012? That's the ACS 5-year data that developers used to justify their project to officials, according to documents

# Discarding the margin of error

hudson_unemployment_2012$moe <- NULL

# Tidying up the structure of the new data frame

hudson_unemployment_2012 <- spread(hudson_unemployment_2012,variable, estimate )

# Calculating the unemployment rate
hudson_unemployment_2012$per_un <- round(hudson_unemployment_2012[,4]/hudson_unemployment_2012[,3]*100,2)

# Filtering out the blank census tracts
hudson_unemployment_2012 <- filter(hudson_unemployment_2012, GEOID!="34017006900" & GEOID!="34017980100")

# Array of group-designated census tracts
proj1 <- c("34017004400", "34017004500", "34017004600", "34017005200", "34017005300", "34017005500", "34017005600", "34017005801", "34017006000", "34017006100", "34017006200", "34017006400", "34017006500", "34017006700", "34017006800", "34017007600")

# Filtering out the tracts data specific to the project
hudson_unemployment_2012_sm <- filter(hudson_unemployment_2012, GEOID %in% proj1)

# Cleaning up the data frame
colnames(hudson_unemployment_2012_sm) <- c("GEOID", "name", "total", "unemployed", "unemp_rate")
hudson_unemployment_2012_sm$un_rate <- hudson_unemployment_2012_sm$unemp_rate$B23025_005
hudson_unemployment_2012_sm$unemp_rate <- NULL

# Adding a label column
hudson_unemployment_2012_sm$radius <- "gerrymandered"

# Array of census tracts immediately adjacent to the project
proj1_half <- c("34017007700", "34017007800", "34017007000", "34017006400", "34017007500", "34017007400", "34017007600")

# Filtering out the tracts data specific to the project
hudson_unemployment_2012_sm_half <- filter(hudson_unemployment_2012, GEOID %in% proj1_half)

# Cleaning up the data frame
colnames(hudson_unemployment_2012_sm_half) <- c("GEOID", "name", "total", "unemployed", "unemp_rate")
hudson_unemployment_2012_sm_half$un_rate <- hudson_unemployment_2012_sm_half$unemp_rate$B23025_005
hudson_unemployment_2012_sm_half$unemp_rate <- NULL

# Adding a label column
hudson_unemployment_2012_sm_half$radius <- "adjacent tracts"

# Combining the two census tracts data frames
hudson_unemployment_sm <- rbind(hudson_unemployment_2012_sm, hudson_unemployment_2012_sm_half)

# Cleaning up the census tract names
hudson_unemployment_sm$name <- gsub(",.*", "", hudson_unemployment_sm$name)

# Prepping the shape file to be joined with data
# (This process is necessary when visualizing with ggplot)
nj_hf <- fortify(nj_h, region="GEOID")

# Joining census data of project to shape file
nj_hud <- left_join(nj_hf, hudson_unemployment_sm, by=c("id"="GEOID"))

# Filter out the census tracts with no data
nj_hud <- filter(nj_hud, !is.na(radius))

# Slicing up the unemployment data (for project tracts) into buckets
nj_hud$nj_mid <- cut(nj_hud$un_rate, breaks = seq(0, 30, by=5))

#colors <- colorRampPalette(c("white", "red"))(length(levels(nj_hud$nj_mid)))

# Mapping project-only census tract data
nj_map <- ggplot()
nj_map <- nj_map + geom_polygon(data=nj_hud, aes(x=long, y=lat, group=group, fill=factor(nj_mid)), color="black", size=.5)
nj_map <- nj_map + facet_wrap(~radius)
nj_map <- nj_map + coord_map() 
nj_map <- nj_map + scale_fill_manual(drop=FALSE, values=c("blue", "yellow", "orange", "red", "green", "purple"), na.value="#EEEEEE", name="Unemployment rate")
nj_map <- nj_map + theme_nothing(legend=TRUE) 
nj_map <- nj_map + labs(x=NULL, y=NULL, title="65 Bay Street (2015)")
nj_map <- nj_map + theme(panel.grid.major = element_line(colour = NA))
nj_map <- nj_map + theme(text = element_text(size=15))
nj_map <- nj_map + theme(plot.title=element_text(face="bold", hjust=.4))
nj_map <- nj_map + theme(plot.subtitle=element_text(face="italic", size=9, margin=margin(l=20)))
nj_map <- nj_map + theme(plot.caption=element_text(size=12, margin=margin(t=12), color="#7a7d7e", hjust=0))
nj_map <- nj_map + theme(legend.key.size = unit(1, "cm"))
nj_map <- nj_map + annotate("segment", x = -74.03577, xend = -74.005, y = 40.72, yend = 40.72, colour = "tomato", size=.5) 
nj_map <- nj_map + annotate("point", x = -74.03577, y = 40.72, colour = "lightblue", size = 1) 
nj_map <- nj_map + annotate("text", x = -73.98217, y = 40.72, label = "65 Bay Street", size=3, colour="gray30") 

print(nj_map)

ggsave( "map2a.pdf", nj_map, device="pdf")

# Mapping all census tract borders in the county and layering on the project-only census tract data
nj_map <- ggplot()
nj_map <- nj_map + geom_polygon(data=nj_hf, aes(x=long, y=lat, group=group), fill=NA, color="black", size=.1)
nj_map <- nj_map + geom_polygon(data=nj_hud, aes(x=long, y=lat, group=group, fill=factor(nj_mid)), color="black", size=.5)
nj_map <- nj_map + facet_wrap(~radius)
nj_map <- nj_map + coord_map() 
nj_map <- nj_map + scale_fill_manual(drop=FALSE, values=c("blue", "yellow", "orange", "red", "green", "purple"), na.value="#EEEEEE", name="Unemployment rate")
nj_map <- nj_map + theme_nothing(legend=TRUE) 
nj_map <- nj_map + labs(x=NULL, y=NULL, title="65 Bay Street (2015)")
nj_map <- nj_map + theme(panel.grid.major = element_line(colour = NA))
nj_map <- nj_map + theme(text = element_text(size=15))
nj_map <- nj_map + theme(plot.title=element_text(face="bold", hjust=.4))
nj_map <- nj_map + theme(plot.subtitle=element_text(face="italic", size=9, margin=margin(l=20)))
nj_map <- nj_map + theme(plot.caption=element_text(size=12, margin=margin(t=12), color="#7a7d7e", hjust=0))
nj_map <- nj_map + theme(legend.key.size = unit(1, "cm"))
nj_map <- nj_map + annotate("segment", x = -74.03577, xend = -74.005, y = 40.72, yend = 40.72, colour = "tomato", size=.5) 
nj_map <- nj_map + annotate("point", x = -74.03577, y = 40.72, colour = "lightblue", size = 1) 
nj_map <- nj_map + annotate("text", x = -73.98217, y = 40.72, label = "65 Bay Street", size=3, colour="gray30") 

print(nj_map)
ggsave("map2b.pdf", nj_map, device="pdf")

# Mapping all census tract data in the county and layering on the project-only census tract data
nj_map <- ggplot()
nj_map <- nj_map + geom_polygon(data=nj_all, aes(x=long, y=lat, group=group, fill=factor(nj_mid)), color="gray", size=.3)
nj_map <- nj_map + geom_polygon(data=nj_hud, aes(x=long, y=lat, group=group, fill=factor(nj_mid)), color="black", size=.6)
nj_map <- nj_map + facet_wrap(~radius)
nj_map <- nj_map + coord_map() 
nj_map <- nj_map + scale_fill_manual(drop=FALSE, values=c("blue", "yellow", "orange", "red", "green", "purple"), na.value="#EEEEEE", name="Unemployment rate")
nj_map <- nj_map + theme_nothing(legend=TRUE) 
nj_map <- nj_map + labs(x=NULL, y=NULL, title="1 Journal Square (2017)")
nj_map <- nj_map + theme(panel.grid.major = element_line(colour = NA))
nj_map <- nj_map + theme(text = element_text(size=15))
nj_map <- nj_map + theme(plot.title=element_text(face="bold", hjust=.4))
nj_map <- nj_map + theme(plot.subtitle=element_text(face="italic", size=9, margin=margin(l=20)))
nj_map <- nj_map + theme(plot.caption=element_text(size=12, margin=margin(t=12), color="#7a7d7e", hjust=0))
nj_map <- nj_map + theme(legend.key.size = unit(1, "cm"))
nj_map <- nj_map + annotate("segment", x = -74.063644, xend = -74.035, y = 40.734330, yend = 40.734330, colour = "tomato", size=.5) 
nj_map <- nj_map + annotate("point", x = -74.063644, y = 40.734330, colour = "lightblue", size = 1) 
nj_map <- nj_map + annotate("text", x = -74.01, y = 40.734330, label = "1 Journal Square", size=3, colour="gray30") 

print(nj_map)
ggsave("map2c.pdf", nj_map, device="pdf")

# Summarizing the table
# Unemployment rate for the census tracts picked by the project group
# versus unemployment rate for census tracts only adjacent to the project
sm_table <- hudson_unemployment_sm %>%
  group_by(radius) %>%
  summarize(average_unemployment=round(mean(un_rate, na.rm=T),2), median_unemployment=round(median(un_rate, na.rm=T),2)) %>%
  arrange(average_unemployment)

kable(sm_table)

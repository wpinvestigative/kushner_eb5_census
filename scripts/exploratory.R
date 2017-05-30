# devtools::install_github("walkerke/tidycensus")

library(tidycensus)
library(tidyverse)
source("keys.R")
source("theme_nothing.R")
library(tigris)
#library(sf)
library(viridis)
library(ggmap)
library(leaflet)

options(tigris_use_cache = TRUE)

census_api_key(key)

# Lines below pull up the table variables available 
# v15 <- load_variables(2015, "acs5", cache = TRUE)
# View(v15)

# Get 5-year ACS data based on years of their methodology
hudson_unemployment_2011 <- get_acs(geography="tract", endyear=2011, variables= c("B23025_005E", "B23025_002E"), county = "Hudson", state="NJ")

# Get 5-year ACS data based on most recently available data
hudson_unemployment_2015 <- get_acs(geography="tract", endyear=2015, variables= c("B23025_005E", "B23025_002E"), county = "Hudson", state="NJ")

# Ignoring margin of error for this exercise
hudson_unemployment_2011$moe <- NULL
hudson_unemployment_2015$moe <- NULL

# Setting up the dataframes (tidying)
hudson_unemployment_2011 <- spread(hudson_unemployment_2011,variable, estimate )
hudson_unemployment_2015 <- spread(hudson_unemployment_2015,variable, estimate )

# Figuring out the unemployment rate from the estimates
hudson_unemployment_2011$per_un <- round(hudson_unemployment_2011[,4]/hudson_unemployment_2011[,3]*100,2)
hudson_unemployment_2015$per_un <- round(hudson_unemployment_2015[,4]/hudson_unemployment_2015[,3]*100,2)

# Getting rid of the blank census tracts
hudson_unemployment_2011 <- filter(hudson_unemployment_2011, GEOID!="34017006900" & GEOID!="34017980100")
hudson_unemployment_2015 <- filter(hudson_unemployment_2015, GEOID!="34017006900" & GEOID!="34017980100")

# Creating a column based on the rank of the unemployment in the county
hudson_unemployment_2011$rank <- rank(hudson_unemployment_2011$per_un)
hudson_unemployment_2015$rank <- rank(hudson_unemployment_2015$per_un)

# Creating an array of census tracts as specified by the developers
proj1 <- c("34017001900", "34017004600", "34017005300", "34017006600", "34017006700", "34017007100")

# Filtering the census tracts unemployment dataframe to those specific to the project
hudson_unemployment_2011_sm <- filter(hudson_unemployment_2011, GEOID %in% proj1)
hudson_unemployment_2015_sm <- filter(hudson_unemployment_2015, GEOID %in% proj1)

# Cleaning up the column names
colnames(hudson_unemployment_2011_sm) <- c("GEOID", "name", "total", "unemployed", "unemp_rate_2011", "2011_rank")
colnames(hudson_unemployment_2015_sm) <- c("GEOID", "name", "total", "unemployed", "unemp_rate_2015", "2015_rank")

# Narrowing down the columns and renaming some so it's easier to join
hudson_unemployment_2011_sm <- select(hudson_unemployment_2011_sm, id=GEOID, name, unemp_rate_2011, `2011_rank`)
hudson_unemployment_2015_sm <- select(hudson_unemployment_2015_sm, id=GEOID, unemp_rate_2015, `2015_rank`)

# Creating one data frame with 2011 data and recent data
hudson_unemp <- left_join(hudson_unemployment_2011_sm, hudson_unemployment_2015_sm)

# Determining the difference in unemployment rate for the two year spans
hudson_unemp$diff <- hudson_unemp$unemp_rate_2015$B23025_005 - hudson_unemp$unemp_rate_2011$B23025_005

# What's the average unemployment rate for these two years
avg_2011 <- mean(hudson_unemp$unemp_rate_2011$B23025_005)
avg_2015 <- mean(hudson_unemp$unemp_rate_2015$B23025_005)

# What's the difference between the averages
avg_diff <- avg_2015 - avg_2011

hudson_unemp$ur_2011 <- hudson_unemp$unemp_rate_2011$B23025_005 
hudson_unemp$ur_2015 <- hudson_unemp$unemp_rate_2015$B23025_005 

hudson_unemp$unemp_rate_2011 <- NULL
hudson_unemp$unemp_rate_2015 <- NULL

hudson_unemp2 <- select(hudson_unemp, name, id, ur_2011, ur_2015)
hudson_unemp2 <- gather(hudson_unemp2, "year", "unemployment_rate", 3:4)


# bringing in jersey city tract boundaries

nj_h <- tracts("NJ", county="Hudson", cb=F)

# Converting the shapefile into a dataframe so it can be used with ggplot2
nj_hf <- fortify(nj_h, region="GEOID")

# Joining the shapefile to the census unemployment data
nj_hud <- left_join(nj_hf, hudson_unemp2)
nj_hud <- filter(nj_hud, !is.na(year))

# Cleaning up the census tract names
nj_hud$year <- gsub("ur_", "", nj_hud$year)
#one_journal_square <- c("40.734330, -74.063644")

# Mapping out the unemployment rate for the two year spans
nj_map <- ggplot()
nj_map <- nj_map + geom_polygon(data=nj_hf, aes(x=long, y=lat, group=group), fill=NA, color="black", size=.1)
nj_map <- nj_map + geom_polygon(data=nj_hud, aes(x=long, y=lat, group=group, fill=unemployment_rate), color="black", size=.5)
nj_map <- nj_map + facet_wrap(~year)
nj_map <- nj_map + coord_map() 
nj_map <- nj_map + scale_fill_viridis(option = "heat", direction=-1, name = "Unemployment rate")
nj_map <- nj_map + scale_color_viridis(option = "heat", direction=-1)
nj_map <- nj_map + theme_nothing(legend=TRUE) 
nj_map <- nj_map + labs(x=NULL, y=NULL, title="1 Journal Square")
nj_map <- nj_map + theme(panel.grid.major = element_line(colour = NA))
nj_map <- nj_map + theme(text = element_text(size=15))
nj_map <- nj_map + theme(plot.title=element_text(face="bold", hjust=.4))
nj_map <- nj_map + theme(plot.subtitle=element_text(face="italic", size=9, margin=margin(l=20)))
nj_map <- nj_map + theme(plot.caption=element_text(size=12, margin=margin(t=12), color="#7a7d7e", hjust=0))
nj_map <- nj_map + theme(legend.key.size = unit(1, "cm"))
nj_map <- nj_map + annotate("segment", x = -74.063644, xend = -74.035, y = 40.734330, yend = 40.734330, colour = "tomato", size=.5) 
nj_map <- nj_map + annotate("point", x = -74.063644, y = 40.734330, colour = "lightblue", size = 2) 
nj_map <- nj_map + annotate("text", x = -74.01, y = 40.734330, label = "1 Journal Square", size=5, colour="gray30") 

nj_map

## slope graph

hudson_unemp2$year <- gsub("ur_", "", hudson_unemp2$year)
hudson_unemp2$name <- gsub(",.*", "", hudson_unemp2$name)
hudson_unemp2_proj <- filter(hudson_unemp2, id=="34017001900")

sp <- ggplot(hudson_unemp2) 
sp <- sp + geom_line(aes(x=as.factor(year), y=unemployment_rate, group=id), size=1)
sp <- sp + geom_point(aes(x=as.factor(year), y=unemployment_rate), size=3)
sp <- sp + geom_line(data=hudson_unemp2_proj, aes(x=as.factor(year), y=unemployment_rate, group=id), size=1, color="tomato")
sp <- sp + geom_point(data=hudson_unemp2_proj, aes(x=as.factor(year), y=unemployment_rate), size=3, color="tomato")
sp <- sp + theme_minimal()
sp <- sp + scale_y_log10()
sp <- sp +geom_text(data = subset(hudson_unemp2, year=="2011"), 
                    aes(x = as.factor(year), y = unemployment_rate, label = paste0(name, ": ", unemployment_rate, "%")), 
                    size = 3, hjust = 1.7)
sp <- sp +  geom_text(data = subset(hudson_unemp2, year=="2015"), 
                      aes(x = as.factor(year), y = unemployment_rate, label =  paste0(unemployment_rate, "%")), 
                      size = 3, hjust = -1.5) 
sp <- sp+ theme(legend.position = "none", 
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                panel.grid.major.x = element_blank(), 
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_blank(), 
                axis.title.y = element_blank(), 
                axis.text.y = element_blank(), 
                plot.title = element_text(size = 18))  
sp <- sp + ggtitle("Unemployment rate in tracts associated with 1 Journal Square")
sp <- sp + xlab("")
sp

head(jersey_tracts)

jersey_tracts  <- get_acs(state = "NJ", county = "Hudson", geography = "tract", 
                  variables = "B23025_001E", geometry = TRUE)

gg <- ggplot(jersey_tracts, aes(fill = estimate, color = estimate))
gg <- gg + geom_sf() 
gg <- gg + coord_sf(crs = 26911) 
gg <- gg + scale_fill_viridis(option = "magma")
gg <- gg + scale_color_viridis(option = "magma")
gg <- gg + theme_nothing(legend=TRUE) 
gg <- gg + labs(x=NULL, y=NULL, title="Jersey City employment")
gg <- gg + theme(panel.grid.major = element_line(colour = NA))
gg <- gg + theme(text = element_text(size=15))
gg <- gg + theme(plot.title=element_text(face="bold", hjust=.4))
gg <- gg + theme(plot.subtitle=element_text(face="italic", size=9, margin=margin(l=20)))
gg <- gg + theme(plot.caption=element_text(size=12, margin=margin(t=12), color="#7a7d7e", hjust=0))
gg <- gg + theme(legend.key.size = unit(1, "cm"))
gg


##

jc <- get_acs(geography = "tract", variables = "B23025_004", key = key,
                        state = "NJ", county = "Hudson County", geometry = TRUE,
                        summary_var = "B23025_001") 

library(forcats)

# Leaflet map
jc_map <- jc %>%
  mutate(pct = 100 * (estimate / summary_est),
         variable = fct_recode(variable,
                               Unemployed = "B23025_004")) %>%
  ggplot(aes(fill = pct, color = pct)) +
  facet_wrap(~variable) +
  geom_sf() +
  coord_sf(crs = 26915) + 
  scale_fill_viridis() +
  scale_color_viridis()
jc_map <- jc_map + theme_nothing(legend=TRUE) 
jc_map <- jc_map + labs(x=NULL, y=NULL, title="Jersey City employment")
jc_map <- jc_map + theme(panel.grid.major = element_line(colour = NA))
jc_map <- jc_map + theme(text = element_text(size=15))
jc_map <- jc_map + theme(plot.title=element_text(face="bold", hjust=.4))
jc_map <- jc_map + theme(plot.subtitle=element_text(face="italic", size=9, margin=margin(l=20)))
jc_map <- jc_map + theme(plot.caption=element_text(size=12, margin=margin(t=12), color="#7a7d7e", hjust=0))
jc_map <- jc_map + theme(legend.key.size = unit(1, "cm"))
jc_map


###

hudson_unemp3 <- subset(hudson_unemp2, year=="2011")
nj_merged <- geo_join(nj_h, hudson_unemp3, "GEOID", "id")

nj_merged <- filter(nj_merged, !is.na(unemployment_rate))
nj_merged <- nj_merged[!is.na(nj_merged$unemployment_rate),]
pal_sb <- colorNumeric("Greens", domain=hudson_unemp2$unemployment_rate)
popup_sb <- paste0(nj_merged$name, ": ", as.character(nj_merged$unemployment_rate))


leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-74.063644, 40.734330, zoom = 13) %>% 
  addPolygons(data = nj_merged , 
              fillColor = ~pal_sb(nj_merged$unemployment_rate), 
              color = "#444444",
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label=popup_sb,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addMarkers(lng=-74.063644, lat=40.734330, popup="1 Journal Square") %>%
  addLegend(pal = pal_sb, 
            values = nj_merged$unemployment_rate, 
            position = "bottomright", 
            title = "Unemployment rate<br />2011")
# Webmap tutorial
# Code based on tutorial by Helen Mckenzie
# https://www.helenmakesmaps.com/
# Here I will present municipal employment growth in German municipalities
# Data from the Federal Employment Agency and part of my Thesis.


# 1. Libraries -------------------------------------------------------------

library(tidyverse)
library(leaflet)
library(rgdal)
library(readxl)
library(RColorBrewer)


# 2. Data import and cleaning ---------------------------------------------

#download List of all German municipalities
url_muni <- "https://www.destatis.de/DE/Themen/Laender-Regionen/Regionales/Gemeindeverzeichnis/Administrativ/Archiv/GVAuszugJ/31122020_Auszug_GV.xlsx?__blob=publicationFile"
destfile <- "31122020_Auszug_GV.xlsx" 
download.file(url_muni, destfile, mode = "wb") # saving file in working directory

# create dataframe with Names and municipal keys
muni_list <- read_xlsx(path = "31122020_Auszug_GV.xlsx",
                       sheet = 2, 
                       range = "C7:H16074", # cell with value
                       col_names = F) %>%
  select(-4) %>% 
  unite(muni_key, 1:4, remove = FALSE, na.rm = TRUE, sep = "") %>%
  filter(nchar(muni_key) == 8) %>% # only municipalities
  select(-c(2:5)) %>%
  rename("name" = ...6)

### read in geoJson employment data
data.empgrw.raw <- readOGR("webmap_data/emp_grw_clean.geojson")

# join data
data.empgrw <- merge(data.empgrw.raw, muni_list, by.x = "ags", by.y = "muni_key")
data.empgrw$emp_grw_p <- round(data.empgrw$emp_grw_p, 2)

# filter for municipalities in Hesse and Thruinga
data.empgrw.HT <- subset(data.empgrw, str_detect(ags ,"^06|^16"))

### read in geoJson for State borders
# important that it is only polylines, otherwise the code below does not work
data.laender.raw <- readOGR("webmap_data/l_bundeslaender.geojson")

# filter for Hesse and Thuringa
data.laender.HT <- subset(data.laender.raw, str_detect(AGS ,"^06|^16"))


# 3. Setting colors and map features --------------------------------------

# creating custom color palette
self.palette <- c("#cc99ff","#C2E699", "#78C679", "#31A354","#006837")

# create bins for chrolopeth map
data.bins <- c(-62, 0, 20, 50, 100, 880) # based on natural breaks (jenks) from QGIS
data.pal <- colorBin(palette = self.palette,
                     na.color = "#F8F8F8", # specify NA color
                     domain = data.empgrw.HT$emp_grw_p, 
                     bins = data.bins)


#Specify what should be shown when clicking on municipality up content
data.empgrw.HT$popup <- paste("<strong>", data.empgrw.HT$name,"</strong>", "</br>", 
                               data.empgrw.HT$ags, "</br>", # municipal key
                           "Employment growth in %", 
                           prettyNum(data.empgrw.HT$emp_grw_p, big.mark = ","))


# 4. Create webmap --------------------------------------------------------

# create webmap
web.map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  # polygons of Municipalities with Employment Growth data
  addPolygons(data = data.empgrw.HT, 
              stroke = TRUE,
              weight = 0.1,
              color = "#ABABAB",
              smoothFactor = 0.3,
              opacity = 0.9, # of stroke
              fillColor = ~data.pal(data.empgrw.HT$emp_grw_p),
              fillOpacity = 0.8,
              popup = ~popup,
              highlightOptions = highlightOptions(color = "#E2068A", # highlights borders when hovering
                                                  weight = 1.5,
                                                  bringToFront = TRUE,
                                                  fillOpacity = 0.5)) %>% 
  # polyglines of State Borders
  addPolylines(data = data.laender.HT, 
               stroke = TRUE,
               weight = 0.7,
               color = "#585858",
               smoothFactor = 0.3,
               opacity = 1) %>% 
  
  addLegend(position = "bottomright", #adding legend
            opacity = 0.9,  
            title = "Employment Growth </br>(2009 to 2019)",
            labels= c("<0%","0-20%","20-50%","50-100%", ">100%", "No data"), 
            colors = c("#cc99ff","#C2E699", "#78C679",
                       "#31A354","#006837", "#F8F8F8")) %>%
  addProviderTiles(providers$CartoDB.Positron, 
                   group = "Basemap - greyscale") %>% #adding basemaps
  addProviderTiles(providers$CartoDB.DarkMatter, 
                   group = "Basemap - dark") %>%
  addLayersControl(
    baseGroups = c("Basemap - greyscale", "Basemap - dark"), # adding control for base maps
    options = layersControlOptions(collapsed = TRUE))

# call web.map
web.map





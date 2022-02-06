# Webmap tutorial
# Code based on tutorial by Helen Mckenzie
# https://www.helenmakesmaps.com/
# Here trying to 

library(tidyverse)
library(leaflet)
library(rgdal)
library(readxl)
library(RColorBrewer)

setwd("~/Data_projects/Intern/upload_code/Webmap_LocalLaborMarket")

#download List of all German municipalities
url_muni <- "https://www.destatis.de/DE/Themen/Laender-Regionen/Regionales/Gemeindeverzeichnis/Administrativ/Archiv/GVAuszugJ/31122020_Auszug_GV.xlsx?__blob=publicationFile"
destfile <- "31122020_Auszug_GV.xlsx"
download.file(url_muni, destfile, mode = "wb")

# create datafrmae with Names and municipal keys
muni_list <- read_xlsx(path = "31122020_Auszug_GV.xlsx",
                       sheet = 2, 
                       range = "C7:H16074", # cell with value
                       col_names = F) %>%
  select(-4) %>% 
  unite(muni_key, 1:4, remove = FALSE, na.rm = TRUE, sep = "") %>%
  filter(nchar(muni_key) == 8) %>%
  select(-c(2:5)) %>%
  rename("name" = ...6)


# read in geo.Jon
data.empgrw.raw <- readOGR("webmap_data/emp_grw_clean.geojson")

# join data
data.empgrw <- merge(data.empgrw.raw, muni_list, by.x = "ags", by.y = "muni_key")
data.empgrw$emp_grw_p <- round(data.empgrw$emp_grw_p, 2)

# creating custom color palette
self.palette <- c("#cc99ff","#C2E699", "#78C679", "#31A354","#006837")


# create bins for chrolopeth map
data.bins <- c(-62, 0, 20, 50, 100, 880)
data.pal <- colorBin(palette = self.palette, reverse = F,
                     na.color = "#F8F8F8",
                     domain = data.empgrw$emp_grw_p, bins = data.bins)



#set pop-up content
data.empgrw$popup <- paste("<strong>", data.empgrw$name,"</strong>", "</br>", 
                           data.empgrw$ags, "</br>",
                           "Employment growth in %", 
                           prettyNum(data.empgrw$emp_grw_p, big.mark = ","))


web.map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(data = data.empgrw,
              stroke = TRUE,
              weight = 0.1,
              color = "#ABABAB",
              smoothFactor = 0.3,
              opacity = 0.9,
              fillColor = ~data.pal(data.empgrw$emp_grw_p),
              fillOpacity = 0.6,
              popup = ~popup,
              highlightOptions = highlightOptions(color = "#E2068A",
                                                  weight = 1.5,
                                                  bringToFront = TRUE,
                                                  fillOpacity = 0.5)) %>% 
  addLegend(position = "bottomright", opacity = 0.9, 
            title = "Employment Growth </br>(2009 to 2019)",
            labels= c("<0%","0-20%","20-50%","50-100%", ">100%", "No data"), 
            colors = c("#cc99ff","#C2E699", "#78C679", "#31A354","#006837", "#F8F8F8")) %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Basemap - aerial") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Basemap - greyscale") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "Basemap - dark") %>%
  addLayersControl(
    baseGroups = c("Basemap - dark","Basemap - greyscale","Basemap - aerial"),
    options = layersControlOptions(collapsed = TRUE))


web.map


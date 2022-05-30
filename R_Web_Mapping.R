library(sf)
library(leaflet)
library(leaflet.extras)
library(leaflet.esri)
library(dplyr)
library(RColorBrewer)
library(htmlwidgets)
library(htmltools)

#if (!require('devtools')) install.packages('devtools')
#devtools::install_github('rstudio/leaflet')

#You must set your own working directory.
setwd("E:/GRI_PRESENTATION/DATA/R")
buildings <- st_read("Buildings.shp")
channels <- st_read("Channels.shp")
Lake_Naivasha <- st_read("Lake_Naivasha_Basin.shp")
study_area <- st_read("Study_Area.shp")

buildings_wm <- st_transform(buildings, crs=4326)
channels_wm <- st_transform(channels, crs=4326)
Lake_Naivasha_wm <- st_transform(Lake_Naivasha, crs=4326)
study_area_wm <- st_transform(study_area, crs=4326)

leaflet() %>%
  addTiles()
leaflet() %>%
  addTiles() %>%
  setView(lat= -0.8, lng=36.4, zoom=4)
names(providers)

leaflet(buildings_wm) %>%
  setView(-0.8, 36.4, 4) %>%
  addTiles() %>% 
  addPolygons()
leaflet(Lake_Naivasha_wm) %>%
  setView(-0.8, 36.4, 4) %>%
  addTiles() %>% 
  addPolygons()
leaflet(study_area_wm) %>%
  setView(-0.8, 36.4, 4) %>%
  addTiles() %>% 
  addPolygons()
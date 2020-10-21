
## Name: 01_setupmap.R
## Date: 08/09/20

## Clear
rm(list=ls())

## Packages
library(broom)
library(readr)
library(dplyr)
library(rgdal)
library(rgeos)
library(VIM)
library(ggplot2)

## Local data directory
dir_data <- "C:/Users/Dan/Documents/BCUHB"

## Wales local admin units
laus_wales <- c("Monmouthshire",
                "Powys",
                "Ceredigion",
                "Carmarthenshire",
                "Gwynedd",
                "Conwy",
                "Denbighshire",
                "Flintshire",
                "Wrexham",
                "Isle of Anglesey",
                "Pembrokeshire",
                "Swansea",
                "Neath Port Talbot",
                "Bridgend",
                "Merthyr Tydfil",
                "Rhondda Cynon Taf",
                "Vale of Glamorgan",
                "Cardiff",
                "Caerphilly",
                "Blaenau Gwent",
                "Torfaen",
                "Newport")

##
## Step 1: A shapefile for BCUHB mapping
##

## Load the shapefile
shapefile <- readOGR(dsn="C:/Users/Dan/Documents/BCUHB/shapefiles/map1", 
                     layer="Local_Administrative_Units_Level_1_December_2015_Full_Extent_Boundaries_in_England_and_Wales")

## Reshape for ggplot2 using the Broom package
mapdata_wales <- broom::tidy(shapefile, region="lau115nm") %>%
  filter(id %in% laus_wales) %>%
  `colnames<-`(c("long","lat","order","hole","piece","county","id"))

## Save
write.csv(mapdata_wales,"2_interimdata/4_areadata/mapdata_wales.csv",row.names = F)

## Test map
mapdata_wales %>%
  ggplot() + 
  geom_polygon(aes(x = long, y = lat, group=county), fill = "#333333", color="#cccccc", size = 0.25) + # Plot the map
  coord_fixed(1) + 
  theme_minimal() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y = element_blank())

##
## END
##

#Packages

library(dplyr)
#library(httr)
#library(rvest)
library(purrr)
#library(plyr)
library(dplyr)
library(ggplot2)
library(jsonlite)
library(lubridate)
library(stringr)
library(viridis)
library(leaflet)
library(leaflet.extras)
#library(htmltools)
library(RColorBrewer)
library(rgdal)
#library(maptools)
library(rgeos)
library(raster)
library(stringr)
library(geojsonio)
library(data.table)
library(ggpubr)
library(readxl)

# library(rsconnect)
# rsconnect::setAccountInfo(name='simetrica', token='76DCAF1DFBB36E84E0F54FDE76BBFBFB', secret='mZFTrw6hrnJTL2qdhNbEG+MYBXSd40ZZshfQ9f7H')

#Projections
ukgrid = "+init=epsg:27700"
latlong = "+init=epsg:4326"

#Location icon
nuclear_icon <- makeIcon(
  iconUrl = "C:/Users/Sebastien/Documents/placeholder.png",
  iconWidth = 64, iconHeight = 64,
  iconAnchorX = 32, iconAnchorY = 32
)

#Lookup
setwd(str_replace_all(path.expand("~"), "Documents", ""))
LSOA_geo_input <- fread("Simetrica/Simetrica - Data/Auxiliary baselining files/LSOA DZ/LSOA_geoindicators.csv", header=TRUE, sep=",", check.names=TRUE)
lsoa_list <- filter(LSOA_geo_input,Copeland==1|Allerdale==1)$LSOA11CD

#LSOA Shapefile
setwd(str_replace_all(path.expand("~"), "Documents", ""))#
setwd("Simetrica/Simetrica - Data/Lookups/LSOA boundaries/")
spdf_lsoa_m <- readOGR("Lower_Layer_Super_Output_Areas_December_2011_Super_Generalised_Clipped__Boundaries_in_England_and_Wales.shp")
spdf_lsoa_m <- spTransform(spdf_lsoa_m, CRS(latlong))
spdf_lsoa_m@data <- dplyr::rename(spdf_lsoa_m@data,LSOA11CD=lsoa11cd)
spdf_lsoa_nw <- subset(spdf_lsoa_m,LSOA11CD %in% lsoa_list)

# setwd(str_replace_all(path.expand("~"), "Documents", ""))
# setwd("Documents/Sellafield Toy dashboard clean/")
# dir.create("tempdir")
# 
# writeOGR(obj=spdf_lsoa_nw, dsn="tempdir", layer="spdf_lsoa_nw", driver="ESRI Shapefile")
# 
setwd(str_replace_all(path.expand("~"), "Documents", ""))#
setwd("Documents/Sellafield Toy dashboard clean/tempdir/")
spdf_mini<- readOGR("spdf_lsoa_nw.shp")
plot(spdf_mini)
spdf_mini@data

#Locations
locations <- data.frame(names=c("Sellafield"),latitude=c(54.4251),longitude=c(-3.5045))

#Simple leaflet map
locations %>% 
  leaflet(.,options = leafletOptions(zoomControl = FALSE)) %>%
  setView(-3.5045,54.4251, 13) %>%
  addPolygons(data=spdf_lsoa_nw,fill=FALSE,stroke = T,col="tomato", dashArray="4", weight = 3, opacity = 0.8) %>%
  addTiles() %>%
  addMarkers(~as.numeric(locations$longitude),
             ~as.numeric(locations$latitude),icon=nuclear_icon)

############# POLLUTION ############

#Air pollution grids

setwd(str_replace_all(path.expand("~"), "Documents", ""))
setwd("Simetrica/Simetrica - Data/Modelled UK Air Data/R/")

UKAirGridLevelData <- as_tibble(read.dta13("air_pollution_clean.dta",
                                           convert.factors = TRUE, generate.factors = FALSE,
                                           encoding = "UTF-8", fromEncoding = NULL, convert.underscore = FALSE,
                                           missing.type = FALSE, convert.dates = TRUE, replace.strl = TRUE,
                                           add.rownames = FALSE, nonint.factors = FALSE, select.rows = NULL,
                                           select.cols = NULL, strlexport = FALSE, strlpath = "."))

UKAirGridLevelData_sp <- SpatialPointsDataFrame(cbind(UKAirGridLevelData$x,UKAirGridLevelData$y),
                                                data = data.frame(ukgridcode=UKAirGridLevelData$ukgridcode,
                                                                  nox2017=UKAirGridLevelData$nox2017,
                                                                  nox2016=UKAirGridLevelData$nox2016,
                                                                  nox2015=UKAirGridLevelData$nox2015,
                                                                  no22017=UKAirGridLevelData$no22017,
                                                                  no22016=UKAirGridLevelData$no22016,
                                                                  no22015=UKAirGridLevelData$no22015,
                                                                  pm252017=UKAirGridLevelData$pm252017,
                                                                  pm252016=UKAirGridLevelData$pm252016,
                                                                  pm252015=UKAirGridLevelData$pm252015,
                                                                  pm102017=UKAirGridLevelData$pm102017,
                                                                  pm102016=UKAirGridLevelData$pm102016,
                                                                  pm102015=UKAirGridLevelData$pm102015),
                                                proj4string = CRS("+init=epsg:27700"))

rm(UKAirGridLevelData)

UKAirGridLevelData_sp <- spTransform(UKAirGridLevelData_sp, CRS(latlong))

UKAirGridLevelData_sp@data <- cbind(UKAirGridLevelData_sp@data,UKAirGridLevelData_sp@coords)

UKAirGrid_sp_Sella <- subset(UKAirGridLevelData_sp,coords.x1>(-3.98)&coords.x1<(-1.88)&coords.x2>(54)&coords.x2<(55)&
                               (is.na(nox2017)==F|is.na(nox2017)==F))

rm(UKAirGridLevelData_sp)

#Pollution in MNWQ, Grids

p_popup <- paste("<strong>NOX</strong>",UKAirGrid_sp_Sella$ukgridcode,UKAirGrid_sp_Sella$nox2017,sep=" - ")

#polpal <- c("#e0ecf4","#8c6bb1","#88419d","#810f7c","#4d004b")
polpal <- c("#d7191c","#fdae61","#ffffbf","#a6d96a","#1a9641")[5:1]

palbin <- colorBin(polpal, domain = UKAirGrid_sp_Sella$nox2017,  bins=c(0,2.5,7.5,10,12.5,Inf),
                   na.color = "transparent", alpha = FALSE, reverse = FALSE,right = FALSE)

leaflet(UKAirGrid_sp_Sella) %>%
  setView(-3.5045,54.4251, 11) %>%
  addProviderTiles(providers$OpenTopoMap) %>% 
  addCircleMarkers(data=UKAirGrid_sp_Sella, stroke = T, weight=0.5, col="#525252",fillColor = ~palbin(nox2017),radius=10,
                   fillOpacity = 0.5,popup = p_popup) %>%
  addMarkers(~as.numeric(locations$longitude),
             ~as.numeric(locations$latitude),icon=nuclear_icon) %>%
  addLegend("bottomright", col=polpal, title = 'Nitrogen oxide µg/m³', labels=c("0 to 2.5","2.5 to 7.5","7.5 to 10","10 to 12.5",">12.5")) # legend title

# #Population chart
# Sella.data.pop <- read_excel("combined-descriptives.xlsx",sheet="Small data long")
# 
# #Access to services
# Sella.data.services <- read_excel("combined-descriptives.xlsx",sheet="Services data long")
# 
# #Population count
# Sella.data.wide <- read_excel("combined-descriptives.xlsx",sheet="Small data") %>%
#   filter(.,Outcome=="Population (2018)")

#Population

setwd(str_replace_all(path.expand("~"), "Documents", ""))
setwd("Documents/Sellafield Toy dashboard clean/")

Sella.data.wide <- read_excel("combined-descriptives.xlsx",sheet="Small data") %>%
  filter(.,Outcome=="Population (2018)")

Sella.data.wide <- fread("Small data.csv", header=TRUE, sep=",", check.names=T)  %>%
  filter(.,Outcome=="Population (2018)")

read_excel("combined-descriptives.xlsx",sheet="Small data") %>%
  

#Population growth

setwd(str_replace_all(path.expand("~"), "Documents", ""))
setwd("Documents/Sellafield Toy dashboard clean/")
Sella.data.pop <- read_excel("combined-descriptives.xlsx",sheet="Small data long")
Sella.data.pop$Rate2 <- paste0(Sella.data.pop$Rate,"%")

ggplot(data=Sella.data.pop, aes(x=Year, y=Rate, fill=Area)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=Rate2), vjust=1.6, color="black",
            position = position_dodge(0.9), size=4)+
  ylim(-1, 1) +
  scale_fill_brewer(palette="Set2")+
  theme_pubclean() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"),
                     axis.text=element_text(size=12),
                     axis.title=element_text(size=12,face="bold"),
                     legend.title = element_text(color = "black", size = 12),
                     legend.text = element_text(color = "black", size = 12))

#Access to services

setwd(str_replace_all(path.expand("~"), "Documents", ""))
setwd("Documents/Sellafield Toy dashboard clean/")
Sella.data.services <- read_excel("combined-descriptives.xlsx",sheet="Services data long")

ggplot(Sella.data.services, aes(Service, Minutes)) +
  geom_linerange(
    aes(x = Service, ymin = 0, ymax = Minutes, group = Area), 
    color = "lightgray", size = 1.5,
    position = position_dodge(0.3)
  )+
  geom_point(
    aes(color = Area),
    position = position_dodge(0.3), size = 5
  )+
  scale_fill_brewer(palette="Set2") + theme_pubclean() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"),
        legend.title = element_text(color = "black", size = 12),
        legend.text = element_text(color = "black", size = 12))

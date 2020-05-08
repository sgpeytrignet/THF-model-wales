##########################################################
################### DEVELOPMENT IDEAS ####################
##########################################################

#On mac, can we make look run on multi-core?
#Use a backup of to import in, for all postcodes in Wales

#Why this data and why this research question
#Does being closer to medical resources mean you have better health and thus less
#likely to become a patient, or is the distance an impediment?
#Unless in extreme cases, people are usually able to reach care in this country

#Set out the research question: Does... ?
#Explain data you will use
#Explain the difficulties and how you will solve them brieflt
#Take them through the code to show what it looks like

#Edit code to make it run faster (only import what you need to)

######################################################
################### LOAD PACKAGES ####################
######################################################

if (!require("pacman")) install.packages("pacman")

pacman::p_load(dplyr,stringr,sp,ggplot2,plyr,readxl,gmodels,Rmisc,DescTools,data.table,
               Hmisc,tibble,rgdal,leaflet,rgeos,raster,plotly,pbapply,pbmcapply,
               ContourFunctions,ROCR,pROC)

rm(list = ls()) ##### Clean up the global environment

########################################################################
################### PRE-LOAD USER-WRITTEN FUNCTIONS ####################
########################################################################

#This function will, for each reference point (e.g.post code), compute the number of items
#within a given buffer and find the nearest item

number.within.buffer <- function(k,distpar_km,adminpoints.spdf,interestpoints.spdf){
  
  #Buffer around the selected admin-area
  # k=825
  # distpar_km=1
  # adminpoints.spdf=Survey_postcodes_shp
  # interestpoints.spdf=healthcare_resources_shp
  
  aux.refpoint <- adminpoints.spdf[k,] #Isolate the admin-area-center
  
  #Store its coordinates
  long.point <- spTransform(aux.refpoint, CRS(latlong))@coords[1,1]
  lat.point <- spTransform(aux.refpoint, CRS(latlong))@coords[1,2]
  a <- c(long.point,lat.point)
  
  #Create a buffer around the amdin-area-center
  aux.refpoint.buffer <- rgeos::gBuffer(aux.refpoint,width=distpar_km*1000,byid=TRUE)
  
  # plot(aux.refpoint.buffer)
  # points(aux.refpoint,col="red")
  # points(healthcare_resources_shp,col="blue")
  
  #Extract the candidate items in that buffer
  aux.overlay <- over(interestpoints.spdf,aux.refpoint.buffer)
  candidates.spdf <- interestpoints.spdf[which(!is.na(aux.overlay[,1])),]
  N_aux <- nrow(candidates.spdf@data) #How many did we find?
  
  if (N_aux>=1) {
    
    #Extract the info about the candidate items
    candidates.data <- candidates.spdf@data
    
    #This matrix will store the information about distances
    cand.mat <- as.data.frame(matrix(NA, nrow = N_aux, ncol = 6))
    names(cand.mat)=c("pcode","country","buffer.km","N.in.buffer","dist.to.point","name")
    
    #Compute distances to admin-area-centre in a loop
    for (s in 1:N_aux){
      
      #Coordinates of candidate point
      long.item <- spTransform(candidates.spdf, CRS(latlong))@coords[s,1]
      lat.item <- spTransform(candidates.spdf, CRS(latlong))@coords[s,2]
      b <- c(long.item,lat.item)
      
      #Compute distance
      distKm <- geosphere::distCosine(a, b, r=6378137)/1000
      #distMin=(gmapsdistance(origin=m,destination=n,mode="walking")$Time)/60
      
      #Populate the results matrix
      cand.mat[s,1]=as.character(aux.refpoint@data$pcode)
      cand.mat[s,2]=as.character(aux.refpoint@data$ctry)
      cand.mat[s,3]=distpar_km
      cand.mat[s,4]=N_aux
      cand.mat[s,5]=distKm
      cand.mat[s,6]=as.character(candidates.spdf@data$Name[s])
      
    } 
    
    #Extract the nearest one, which will be the final output
    idx.nearest <- which(cand.mat[,5]==min(cand.mat[,5]))[1]
    end.mat <- cand.mat[idx.nearest,]
    
  }
  
  else {
    
    #This matrix will store the results - which are always 0 if there were no candidates
    end.mat <- as.data.frame(matrix(NA, nrow = N_aux, ncol = 6))
    
    names(end.mat)=c("pcode","country","buffer.km","N.in.buffer","dist.to.point","name")
    
    end.mat[1,1]=as.character(aux.refpoint@data$pcode)
    end.mat[1,2]=as.character(aux.refpoint@data$ctry)
    end.mat[1,3]=distpar_km
    end.mat[1,4]=N_aux
    end.mat[1,5]=NA
    end.mat[1,6]=NA
    
  }
  
  return(end.mat)
}

######################################################
################### SET DIRECTORY ####################
######################################################

setwd(str_replace_all(path.expand("~"), "Documents", ""))
setwd("Documents/GitHub/THF-model-wales/")

#####################################################################
################### PROJECTIONS FOR GIS ANALYSIS ####################
#####################################################################

ukgrid = "+init=epsg:27700"
latlong="+init=epsg:4326"

##########################################################
################### IMPORT SURVEY DATA ###################
##########################################################

#Wave 8 (2017) of Understanding Society, a nationally-representative
#longitudinal population survey (from ESRC)

#Includes health module, including outcomes on health resource utilisation
#We can manipulate this dataset and coax into a structure similar to
#A retrospective observational study
#Useful to describe associations and produce prediction models

USoc <- fread("Understanding-Society-Wave8.csv", header=TRUE, sep=",", check.names=T) %>% 
          filter(.,gor_dv=="[10] wales") %>% as_tibble()

summary(USoc)

#How many people inpatient or outpatient in the year after data collection?
#Summarize into graphs
#Look at some univariate statistics (overlaid histograms) for age
#Bubble chart or frequency table for LT_health and urban/rural (this sets the scene)

#We would like to describe the likelihood of being being an inpatient our outpatient
#Using a set of standard socio-economic predictors, but also location-based variables
#That we obtain using web-scraping and GIS analysis

#However,the location of respondent is unknown (available under special license)
#This is not an approved use
#So for the sake of this exercise, we will impute this
#How? Sampling postcodes at random (with replacement) based on rural/urban indicator

USoc_urban <- filter(USoc,urban==1) %>% select(.,pidp)
USoc_rural <- filter(USoc,urban==0) %>% select(.,pidp)

#####################################################################
################### LOOKUP TABLES FOR GEOGRAPHIES ###################
#####################################################################

#Import directory of postcodes in the UK, Wales

# Wales_postcodes <- fread("ONS Postcode Directory (Latest) Centroids.csv",
#                       header=TRUE, sep=",", check.names=T,
#                       select=c("lat","long","pcds","ctry","ru11ind")) %>%
#   filter(.,ctry=="W92000004")
# 
# Wales_postcodes$pcode <- str_replace_all(Wales_postcodes$pcds, fixed(" "), "") #Remove spaces in Postcodes variable to homogenise
# 
# #Turn the categorical urban/rural indicator into a binary classification - based on guidelines from the Office for National Statistics
# 
# Wales_postcodes$urban <- ifelse(Wales_postcodes$ru11ind=="A1"|Wales_postcodes$ru11ind=="B1"|Wales_postcodes$ru11ind=="C1"|
#                                 Wales_postcodes$ru11ind=="C2",1,0)

Wales_postcodes_full <- fread("Welsh postcodes.csv",
                               header=TRUE, sep=",", check.names=T)

Wales_postcodes_small <- fread("Welsh postcodes small.csv",
                         header=TRUE, sep=",", check.names=T)

# Wales_postcodes_small <- Wales_postcodes_full[sample(1:nrow(Wales_postcodes_full),10000,replace= FALSE),]
# fwrite(Wales_postcodes_small, file = "Welsh postcodes small.csv", sep = ",")

  #Sample urban postcodes for USoc dataset (with replacement)

urban_postcodes <- filter(Wales_postcodes_small,urban==1)
samples_postcodes_urban_idx <- sample(nrow(urban_postcodes),nrow(USoc_urban), replace = TRUE, prob = NULL)
USoc_imputed_pcode_urban <- urban_postcodes[samples_postcodes_urban_idx,] %>% select(.,pcode) %>% cbind.data.frame(.,USoc_urban)

  #Sample rural postcodes for USoc dataset (with replacement)

rural_postcodes <- filter(Wales_postcodes_small,urban==0)
samples_postcodes_rural_idx <- sample(nrow(rural_postcodes),nrow(USoc_rural), replace = TRUE, prob = NULL)
USoc_imputed_pcode_rural <- urban_postcodes[samples_postcodes_rural_idx,] %>% select(.,pcode) %>% cbind.data.frame(.,USoc_rural)

  #Dataset of imputed postcodes

imputed_postcodes <- rbind(USoc_imputed_pcode_urban,USoc_imputed_pcode_rural) %>% as.data.table()

  #Merge imputed postcodes back into dataset

USoc <- left_join(USoc,imputed_postcodes,by="pidp")

rm(urban_postcodes,samples_postcodes_urban_idx,USoc_imputed_pcode_urban,
   rural_postcodes,samples_postcodes_rural_idx,USoc_imputed_pcode_rural,
   USoc_rural,USoc_urban,imputed_postcodes) #Clean up environment

  #Create a shapfile of survey postcodes and visualize imputed results

Survey_postcodes_shp <- SpatialPointsDataFrame(cbind(Wales_postcodes_small$long,Wales_postcodes_small$lat),
                                              data = Wales_postcodes_small,
                                              proj4string = CRS(latlong)) %>%
                        subset(., pcode %in% USoc$pcode)

leaflet(Survey_postcodes_shp,options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles(providers$Stamen.Terrain) %>%
  addCircleMarkers(data=Survey_postcodes_shp,fillColor = "blue",radius=5,
                   fillOpacity = 0.5,stroke=T,col="#737373",weight = 1)

###############################################################
################### IMPORT WEB-SCRAPED DATA ###################
###############################################################

#Import data on GP surgeries (NHS Digital) as CSV
#Only keep GP surgeries (Setting=4 - excluding other medical services)

gp_surgeries <- fread("epraccur.csv", header = T, sep = ',', data.table = T,
                      select=c("GP_name","pcode")) %>%
                rename(.,Name=GP_name)

gp_surgeries$pcode <- str_replace_all(gp_surgeries$pcode, fixed(" "), "")

#Merge in coordinates bases on GP surgery postcode
#And only keep the ones that can be located using a postcode
#At the same time, restrict it to surgeries in Wales
#Turn this dataset into a shapefile

gp_surgeries <- left_join(gp_surgeries,Wales_postcodes_full,by="pcode") %>%
    filter(.,is.na(lat)==F&is.na(long)==F) %>%
    mutate(.,Type="GP") %>%
    select(.,Name,Type,lat,long)

gp_surgeries_shp <- SpatialPointsDataFrame(cbind(gp_surgeries$long,gp_surgeries$lat),
                                          data = gp_surgeries,
                                          proj4string = CRS(latlong))

#Import data on Hospitals and Pharmacies web-scraped from OpenStreetMaps
#Objects tagged with amenity=hospital, amenity=pharmacy or amenity=chemist
#Extracted using Overpass Turbo and saving shapefile locally
#To make processing easier, we will only keep points and ignore polygons

OSM_points_shp <- readOGR("hospitals.geojson", "hospitals", require_geomType="wkbPoint") #Import shapefile
OSM_points_shp <- spTransform(OSM_points_shp, CRS(latlong)) #Set to the same projection

OSM_points_shp@data <- select(OSM_points_shp@data,name,amenity) %>%
  rename(.,Name=name,Type=amenity)

#Merge geospatial data from NHS and OpenStreetMaps into one shapefile

healthcare_resources_shp <- bind(OSM_points_shp,gp_surgeries_shp)

rm(OSM_points_shp,gp_surgeries_shp,gp_surgeries,Wales_postcodes_full,Wales_postcodes_small) #Clean up environment

#################################################################
################### VISUALIZE GEOSPATIAL DATA ###################
#################################################################

palher <- colorFactor(palette=c("#e7298a","#e6ab02"),
                      levels = c("GP","hospital"))

leaflet(healthcare_resources_shp,options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles(providers$Stamen.Terrain) %>%
  addCircleMarkers(data=healthcare_resources_shp,fillColor = ~palher(Type),radius=5,
                   fillOpacity = 0.5,stroke=T,col="#737373",weight = 1) %>%
  addLegend("bottomright",  # location
            col=c("#e7298a","#e6ab02"),    # palette function
            title = 'Amenity',
            labels=c("GP","hospital"),opacity = 1) # legend title

##########################################################
################### APPLY GIS ANALYSIS ###################
##########################################################

#For each survey response, we want to compute the distance to the nearest amenity

Survey_postcodes_shp <- spTransform(Survey_postcodes_shp, CRS(ukgrid))

healthcare_resources_shp <- spTransform(healthcare_resources_shp, CRS(ukgrid))

##### Test the function for first 5 postcodes among survey responses

loop.support.one <- 1:5

survey.predictors.wales <- pbmclapply(loop.support.one,number.within.buffer,
                                      distpar_km=20,
                                      adminpoints.spdf=Survey_postcodes_shp,
                                      interestpoints.spdf=healthcare_resources_shp) %>% data.table::rbindlist(.)

##### Applying can take up to 30min, so let's import the ready-made results instead

survey.predictors.wales <- fread("Welsh postcodes small 20km.csv",
                         header=TRUE, sep=",", check.names=T)

##### How is this new predictor distributed?

summary(survey.predictors.wales$dist.to.point)

ggplot(survey.predictors.wales, aes(dist.to.point, fill = cut(dist.to.point, 100))) +
  geom_histogram(show.legend = FALSE) +
  theme_minimal() +
  labs(x = "Km to nearest GP/hospital", y = "n") +
  ggtitle("Histogram") + scale_fill_discrete(h = c(240, 10), c = 120, l = 70)

#############################################################
################### MERGE INTO THE SURVEY ###################
#############################################################

USoc <- left_join(USoc,survey.predictors.wales,by="pcode")

#############################################################
################### SIMPLE REGRESSION MODEL #################
#############################################################

Model_1 <-  glm(inpatient_nexttyear ~ age+male_fe+leq_hhincome+LT_health,data=USoc, family=binomial)
summary(Model_1)

#In this simple model, no differences according to gender
#Older people, holding health constant, do have a higher likelihood of
#becoming a patient (significant if you combine outpatient/inpatient) [EXPRESS MARGIN?]
#Income does not have an impact in this model
#Not surprisingly, having a LT health condition is a strong predictor [EXPRESS MARGIN?]

Model_2 <-  glm(inpatient_nexttyear ~ age+male_fe+leq_hhincome+LT_health+urban,data=USoc, family=binomial)
summary(Model_2)

#Living in a rural area does not make you less likely to become an inpatient

Model_3 <-  glm(inpatient_nexttyear ~ age+male_fe+leq_hhincome+LT_health+dist.to.point,data=USoc, family=binomial)
summary(Model_3)

#As an extension, neither does the distance to a GP/hospital
#(although suggests a longer distance means you are less likely health being held constant)

#############################################################
################### SIMPLE PREDICTIVE ANALYTICS #############
#############################################################

predict_model1 = predict(Model_1, type="response")

length(predict_model1)

predict_model2 = predict(Model_2, type="response")
predict_model3 = predict(Model_3, type="response")

roc_model1 <- roc(diagnostic_data$malignant,predict_model1)
roc_model2 <- roc(diagnostic_data$malignant,predict_model2)
roc_model3 <- roc(diagnostic_data$malignant,predict_model3)

#ROC curve for a model
#maybe plot/curve in terms of age and distance to GP/hospital like in code already done
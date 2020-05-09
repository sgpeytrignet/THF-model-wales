#####################################################
################### INTRODUCTION ####################
#####################################################

# • A brief explanation of what problem you are trying to solve and the data you have used
# • The approach you have taken to solving the problem

#Why this data and why this research question
#Recently been working on research for welsh government, on benefits of a transport scheme
#Decided to draw on some of my knowledge from that geodata, but apply to the health area
#Looking at what person-level data i had available, decided to look at...

#Does being closer to medical resources mean you have better health and thus less
#likely to become a patient, or is the distance an impediment?
#Unless in extreme cases, people are usually able to reach care in this country

#Hype up the data, say that people it's reallt good but people don't use it for prediction ofter, but it's
#possible to do so by manipulating the data structure: what happens to the person in the future
#I want to pool together survey data, web-scraped information and GIS analysis
#to explore this question

#I restrict to Wales because of recent work i've been doing for Welsh gvt so it's in my mind
#And also because the GIS functions are sometimes intensive so it's good to have smaller data

#Set out the research question: Does... ?
#Explain data you will use
#Explain the difficulties and how you will solve them brieflt
#Take them through the code to show what it looks like

#Edit code to make it run faster (only import what you need to)

######################################################
################### LOAD PACKAGES ####################
######################################################

if (!require("pacman")) install.packages("pacman")

pacman::p_load(dplyr,stringr,sp,ggplot2,plyr,gmodels,Rmisc,DescTools,data.table,
               Hmisc,tibble,rgdal,leaflet,rgeos,raster,plotly,pbapply,pbmcapply,
               skimr,ROCR,pROC,margins,jtools)

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
setwd("Documents/GitHub/THF-model-wales/Files/")

############################################################################
################### PROJECTIONS FOR GEOSPATIAL ANALYSIS ####################
############################################################################

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
#We would like to describe the likelihood of being being an inpatient our outpatient
#Using a set of standard socio-economic predictors, but also location-based variables
#That we obtain using web-scraping and GIS analysis

################## Import dataset

USoc <- fread("Understanding-Society-Wave8.csv", header=TRUE, sep=",", check.names=T) %>% 
          filter(.,gor_dv=="[10] wales") %>% as_tibble()
skim(USoc)

################## Explore dataset

#10.3% of sample required in-patient treatment the year being surveyed
round(mean(USoc$inpatient_nexttyear)*100,1)

#The median age of those who went to hospital was 6 years higher
mu_age <- ddply(USoc, "inpatient_nexttyear", summarise, age.median=median(age))
USoc %>% ggplot(., aes(x=age, fill=factor(inpatient_nexttyear), color=factor(inpatient_nexttyear))) +
  geom_density(alpha=0.5) + theme(panel.background = element_blank()) + ggtitle("Distribution of age") +
  geom_vline(data=mu_age, aes(xintercept=age.median, color=factor(inpatient_nexttyear)),
             linetype="dashed") + scale_colour_brewer(type="qual",labels = c("No", "Yes"),palette=4) +
  scale_fill_brewer(type="qual",labels = c("No", "Yes"),palette=4) + labs(fill = "Inpatient care next 12m",col="Inpatient care next 12m")
rm(mu_age)

#The majority (almost 60%) of thsoe who went to hospital already had a long-term condition
#Compared to 40% in the rest of the cohort
USoc %>% ddply(., "LT_health", summarise, rate.inpatient=mean(inpatient_nexttyear)*100) %>% round(.,1) %>%
  ggplot(., aes(x=factor(LT_health), y=rate.inpatient, fill=factor(LT_health))) +
  geom_bar(stat="identity") + geom_text(aes(label=rate.inpatient), position=position_dodge(width=0.9)) +
  scale_fill_brewer(type="qual",labels = c("No", "Yes"),palette=1) + labs(fill = "Long-term health condition") + xlab("Long-term health condition") + ylab("%") + ggtitle("% requiring inpatient care next 12m") + theme_minimal()

#There is, at first sight, no relationship between living in an urban area and being admitted to hospital
USoc %>% ddply(., "urban", summarise, rate.inpatient=mean(inpatient_nexttyear)*100) %>% round(.,1) %>%
  ggplot(., aes(x=factor(urban), y=rate.inpatient, fill=factor(urban))) +
  geom_bar(stat="identity") + geom_text(aes(label=rate.inpatient), position=position_dodge(width=0.9)) +
  scale_fill_brewer(type="qual",labels = c("No", "Yes"),palette=2) + labs(fill = "Living in urban area") + xlab("Living in urban area") + ylab("%") + ggtitle("% requiring inpatient care next 12m") + theme_minimal()

#What if we had more precise data on distance to the nearest GP/Hospital, would we find the same thing?
#We don't know where people live in this version of the dataset, but we do have an urban/rural indicator
#So, as a thought experiment, let's impute postcodes to respondents based on that indicator

#####################################################################
################### LOOKUP TABLES FOR GEOGRAPHIES ###################
#####################################################################

################## Import directory of postcodes in the UK, Wales
Wales_postcodes_full <- fread("Welsh postcodes.csv",header=TRUE, sep=",", check.names=T)
Wales_postcodes_small <- fread("Welsh postcodes small.csv",header=TRUE, sep=",", check.names=T)

################## Sample urban postcodes for USoc dataset (with replacement)
USoc_urban <- filter(USoc,urban==1) %>% select(.,pidp)
urban_postcodes <- filter(Wales_postcodes_small,urban==1)
samples_postcodes_urban_idx <- sample(nrow(urban_postcodes),nrow(USoc_urban), replace = TRUE, prob = NULL)
USoc_imputed_pcode_urban <- urban_postcodes[samples_postcodes_urban_idx,] %>% select(.,pcode) %>% cbind.data.frame(.,USoc_urban)

################## Sample rural postcodes for USoc dataset (with replacement)
USoc_rural <- filter(USoc,urban==0) %>% select(.,pidp)
rural_postcodes <- filter(Wales_postcodes_small,urban==0)
samples_postcodes_rural_idx <- sample(nrow(rural_postcodes),nrow(USoc_rural), replace = TRUE, prob = NULL)
USoc_imputed_pcode_rural <- urban_postcodes[samples_postcodes_rural_idx,] %>% select(.,pcode) %>% cbind.data.frame(.,USoc_rural)

################## Dataset of imputed postcodes
imputed_postcodes <- rbind(USoc_imputed_pcode_urban,USoc_imputed_pcode_rural) %>% as.data.table()

################## Merge imputed postcodes back into dataset
USoc <- left_join(USoc,imputed_postcodes,by="pidp")
rm(urban_postcodes,samples_postcodes_urban_idx,USoc_imputed_pcode_urban,rural_postcodes,samples_postcodes_rural_idx,USoc_imputed_pcode_rural,USoc_rural,USoc_urban,imputed_postcodes)
  
################## Visualize (imputed) locations of survey respondents
Survey_postcodes_shp <- SpatialPointsDataFrame(cbind(Wales_postcodes_small$long,Wales_postcodes_small$lat),
                                              data = Wales_postcodes_small,
                                              proj4string = CRS(latlong)) %>% subset(., pcode %in% USoc$pcode)

leaflet(Survey_postcodes_shp,options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles(providers$Stamen.Terrain) %>% addCircleMarkers(data=Survey_postcodes_shp,fillColor = "blue",radius=5, fillOpacity = 0.5,stroke=T,col="#737373",weight = 1)

###############################################################
################### IMPORT WEB-SCRAPED DATA ###################
###############################################################

################## Import GP surgery locations (NHS Digital)

#This is a pre-cleaned dataset, where I've extracted the practice postcode from the address
#then merged in geographical coordinates based on that postcode, and kept only those in Wales
#We then convert the data frame into a shapefile

gp_surgeries <- fread("epraccur-clean.csv", header = T, sep = ',', data.table = T)

gp_surgeries_shp <- SpatialPointsDataFrame(cbind(gp_surgeries$long,gp_surgeries$lat),
                                          data = gp_surgeries[,1:2],
                                          proj4string = CRS(latlong))

################## Import hospital locations (Open Street Maps)

#Import data on Hospitals and Pharmacies web-scraped from OpenStreetMaps
#These are objects tagged with amenity=hospital, uoloaded  by users
#Extracted using Overpass Turbo, an online tool, and saving shapefile locally
#To make processing easier, we will only keep points and ignore polygons

OSM_points_shp <- readOGR("hospitals.geojson", "hospitals", require_geomType="wkbPoint") #Import shapefile
OSM_points_shp <- spTransform(OSM_points_shp, CRS(latlong)) #Set to the same projection
OSM_points_shp@data <- select(OSM_points_shp@data,name,amenity) %>%
  rename(.,Name=name,Type=amenity)

################## Merge geospatial data from NHS and OpenStreetMaps a single shapefile

healthcare_resources_shp <- raster::bind(OSM_points_shp,gp_surgeries_shp)
rm(OSM_points_shp,gp_surgeries_shp,gp_surgeries,Wales_postcodes_full,Wales_postcodes_small) #Clean up environment

################## Visualize the web-scraped geodata
################## Note areas in the middle with much lower provison (relative to population density)

palher <- colorFactor(palette=c("#e7298a","#e6ab02"), levels = c("GP","hospital"))

leaflet(healthcare_resources_shp) %>%
  addProviderTiles(providers$Stamen.Terrain) %>%
  addCircleMarkers(data=healthcare_resources_shp,fillColor = ~palher(Type),radius=5,
                   fillOpacity = 0.5,stroke=T,col="#737373",weight = 1) %>% addLegend("bottomright", col=c("#e7298a","#e6ab02"), title = 'Amenity', labels=c("GP","hospital"),opacity = 1) # legend title

######################################################################
################### PRODUCE NEW PREDICTORS IN DATA ###################
######################################################################

##################  Test the function for first 5 postcodes among survey responses

loop.support.one <- 1:5
Survey_postcodes_shp <- spTransform(Survey_postcodes_shp, CRS(ukgrid))
healthcare_resources_shp <- spTransform(healthcare_resources_shp, CRS(ukgrid))
survey.predictors.wales <- pbmclapply(loop.support.one,number.within.buffer,
                                      distpar_km=20,adminpoints.spdf=Survey_postcodes_shp,
                                      interestpoints.spdf=healthcare_resources_shp) %>% data.table::rbindlist(.)
survey.predictors.wales

##################  Applying can take up to 30min, so let's import the ready-made results instead

survey.predictors.wales <- fread("Welsh postcodes small 20km.csv",
                         header=TRUE, sep=",", check.names=T)

################## How is this new predictor distributed?
################## This confirms that, overall, access is good but some residential postcodes
################## Live more than 10km from a GP surgery

round(median(survey.predictors.wales$dist.to.point*1000),1)
ggplot(survey.predictors.wales, aes(dist.to.point, fill = cut(dist.to.point, 100))) +
  geom_histogram(show.legend = FALSE) + theme_minimal() + labs(x = "Km to nearest GP/hospital", y = "n") +
  ggtitle("Histogram") + scale_fill_discrete(h = c(240, 10), c = 120, l = 70)

################## Merge new predictor into the survey based on the imputed postcodes

USoc <- left_join(USoc,survey.predictors.wales,by="pcode")

#########################################################################
###################  REGRESSION MODELS TO ASSESS IMPACT #################
#########################################################################

#Age is associated with a higher likelihood of a stay in hospital
Model_1 <-  glm(inpatient_nexttyear ~ age+male_fe+leq_hhincome,data=USoc, family=binomial)
jtools::plot_summs(Model_1, scale = TRUE)
cplot(Model_1, "age")

#But after adjusting for health conditions, this is more likely related to comorbidities
#In fact, age is no longer a significant predictor and we see that those with pre-existing
#long-term conditions are twice as likely to need inpatient care
Model_2 <-  glm(inpatient_nexttyear ~ age+male+leq_hhincome+LT_health,data=USoc, family=binomial)
jtools::plot_summs(Model_2, scale = TRUE)

vis_model2 <- cbind.data.frame(LT_health=Model_2$data$LT_health,pred=predict(Model_2, type="response")) %>%
  ddply(., "LT_health", summarise, mean.likelihood=mean(pred*100)) %>% round(.,1) %>%
  ggplot(., aes(x=factor(LT_health), y=mean.likelihood,
                fill=factor(LT_health))) + geom_bar(stat="identity") + geom_text(aes(label=mean.likelihood), position=position_dodge(width=1)) + scale_fill_brewer(type="qual",labels = c("No", "Yes"),palette=4) +
  labs(fill = "Previous health condition") + xlab("Previous health condition") + ylab("%") + theme_minimal() + ggtitle("Predicted likelihood of needing inpatient care")
vis_model2

#Living in an urban area is not associated with more hospital stays
Model_3 <-  glm(inpatient_nexttyear ~ age+male+leq_hhincome+LT_health+urban,data=USoc, family=binomial)
jtools::plot_summs(Model_3, scale = TRUE)

#Neither is living further away from a GP or hospital
Model_4 <-  glm(inpatient_nexttyear ~ age+male+leq_hhincome+LT_health+dist.to.point,data=USoc, family=binomial)
jtools::plot_summs(Model_4, scale = TRUE)

#########################################################
############# ASSESSMENT OF PREDICTIVE MODEL ############
#########################################################

#AUC (area under the curve) of model is 0.605
#Not a good model for predicting need for inpatient care - only slightly better than random prediction (AUC of 0.5)
predict_model4 <- predict(Model_4, type="response")
AUC <- pROC::roc(Model_4$data$inpatient_nexttyear,predict_model4)
AUC

#Plot the ROC curve for a model: true positive rate vs. false positive rate
#And the optimal cut-off point for prediction using this model
ROCRpred_model4 <-  ROCR::prediction(predict_model4, Model_4$data$inpatient_nexttyear)
ROCRperf_model4 <- performance(ROCRpred_model4, "tpr", "fpr")
plot(ROCRperf_model4, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

#############################################################
################### SUMMARISE THE POINTS BELOW ##############
#############################################################

# • What you learned in this project from an analysis and coding perspective
# • Reflections on what you would do differently in another project

#Use real postcodes (otherwise just info from urban/rural plus noise)
#Perhaps look at non-urgent care where distance may be more of a factor
#Journey times by car/PT rather than as-the-crow-flies distance
#Use whole UK, allow interactions
#Focus on those more remote areas
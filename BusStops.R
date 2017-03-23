
# Geocoding Potential Bus Stop Intersections ####
library(RCurl)
library(RJSONIO)
library(plyr)

#Loading Dataset
BusStops <- read.csv("/Users/bekterra/Desktop/data/Potentail_Bust_Stops.csv")
infileBus <-"/Users/bekterra/Desktop/Hackatons/Bus Stop Optimization/Bus/Potentail_Bust_Stops.csv"

# Feature Engineering
BusStops$Intersection <- paste0(BusStops$Street_One,"/",BusStops$Street_Two,", San Francisco, CA, USA")

library(ggmap)
library(data.table)
dff <- data.frame(address=c(BusStops$Intersection))

# (change or remove this if your address already include a country etc.)
addresses = dff$address
#addresses = paste0(addresses, ", Ireland")

#define a function that will process googles server responses for us. ####
getGeoDetails <- function(address){   
  #use the gecode function to query google servers
  geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
  #now extract the bits that we need from the returned list
  answer <- data.frame(lat=NA, long=NA, accuracy=NA, formatted_address=NA, address_type=NA, status=NA)
  answer$status <- geo_reply$status
  
  #if we are over the query limit - want to pause for an hour
  while(geo_reply$status == "OVER_QUERY_LIMIT"){
    print("OVER QUERY LIMIT - Pausing for 1 hour at:") 
    time <- Sys.time()
    print(as.character(time))
    Sys.sleep(60*60)
    geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
    answer$status <- geo_reply$status
  }
  
  #return Na's if we didn't get a match:
  if (geo_reply$status != "OK"){
    return(answer)
  }   
  #else, extract what we need from the Google server reply into a dataframe:
  answer$lat <- geo_reply$results[[1]]$geometry$location$lat
  answer$long <- geo_reply$results[[1]]$geometry$location$lng   
  if (length(geo_reply$results[[1]]$types) > 0){
    answer$accuracy <- geo_reply$results[[1]]$types[[1]]
  }
  answer$address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
  answer$formatted_address <- geo_reply$results[[1]]$formatted_address
  
  return(answer)
}

#initialise a dataframe to hold the results
geocodedBus <- data.frame()
# find out where to start in the address list (if the script was interrupted before):
startindex <- 1
#if a temp file exists - load it up and count the rows!
tempfilename <- paste0(infileBus, '_temp_geocoded.rds')
if (file.exists(tempfilename)){
  print("Found temp file - resuming from index:")
  geocodedBus <- readRDS(tempfilename)
  startindex <- nrow(geocodedBus)
  print(startindex)
}

# Start the geocoding process - address by address. geocode() function takes care of query speed limit.
addresses <- as.character(addresses)
for (ii in seq(startindex, length(addresses))){
  print(paste("Working on index", ii, "of", length(addresses)))
  #query the google geocoder - this will pause here if we are over the limit.
  result = getGeoDetails(addresses[ii]) 
  print(result$status)     
  result$index <- ii
  #append the answer to the results file.
  geocodedBus <- rbind(geocodedBus, result)
  #save temporary results as we are going along
  saveRDS(geocodedBus, tempfilename)
}


# Start the geocoding process - address by address. geocode() function takes care of query speed limit.
 
geocodedBus<- geocodedBus[!duplicated(geocodedBus), ]

# removing missing rows
geocodedBus <- na.omit(geocodedBus)

#now we add the latitude and longitude to the main data

dataBus$lat <- geocodedBus$lat
dataBus$long <- geocodedBus$long
dataBus$accuracy <- geocodedBus$accuracy



sum(is.na(locations$lat))

#RANGE_INTERPOLATED" indicates that the returned result reflects an approximation (usually on a road) interpolated between two precise points (such as intersections). 
#Interpolated results are generally returned when rooftop geocodes are unavailable for a street address.





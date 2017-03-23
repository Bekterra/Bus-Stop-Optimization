# Loading Datasets
EmpAdd <- read.csv("/Users/bekterra/Desktop/Hackatons/Bus Stop Optimization/Bus/Employee_Addresses.csv")
BusStops <- read.csv("/Users/bekterra/Desktop/Hackatons/Bus Stop Optimization/Bus/Potentail_Bust_Stops.csv")


#Feature Engineering
EmplAddr <- data
BusStopPoints <- geocodedBus


# Detecting NAs and reordering observations
NewBusStop <-BusStops[-c(1,18,56,64,65,69,88), ] 
rownames(NewBusStop) <- 1:nrow(NewBusStop)



# # Geocodin Employees Addresses ####
# # find a reasonable spatial extent
# library(ggmap)
# AddressFrequency <- function(addresses){
#   tab <- table(addresses)
#   unique.points <- data.frame(address=names(tab), frequency = c(tab))
#   rownames(unique.points) <- NULL
#   unique.points
# }
# 
# #all.addresses <- c(as.character(df$From),as.character(df$To))
# freq <- AddressFrequency(EmplAddr$address)
# freq <- cbind(freq, geocode(as.character(freq$address)))
# 
# centre = c(mean(freq$lon, na.rm=TRUE), mean(freq$lat, na.rm=TRUE))
# map = get_map(location = centre, zoom=13, scale=2, source = "google", maptype="roadmap")
# # to use factos for frequencies
# # freq$frequency <- factor(freq$frequency)
# map.plot = ggmap(map)
# map.plot = map.plot + geom_point(data = freq, aes(x = lon, y = lat, color = "red"), size=2)
# # to use color brewer gradient scale:
# #library(RColorBrewer)
# # map.plot = map.plot +  scale_colour_gradientn(colours=rainbow(4))
# print(map.plot)



# Mapping spatial data points on Google Map ####
devtools::install_github("dkahle/ggmap")
devtools::install_github("hadley/ggplot2")
library(dtplyr)
library(maps)
library(dbscan)


# plotting the map with some points on it
dev.off()
install.packages("ggmap", type = "source")

SFmap <- qmap('San Francisco, USA', zoom = 12, maptype = "roadmap")
SFmap + geom_point(data=EmplAddr, aes(x=long, y=lat, colour = "red")) + scale_color_discrete("Employee Addresses")
   
# CLustering data points ####
library(geosphere)

geo.dist = function(df) {
  require(geosphere)
  d <- function(i,z){         # z[1:2] contain long, lat
    dist <- rep(0,nrow(z))
    dist[i:nrow(z)] <- distHaversine(z[i:nrow(z),1:2],z[i,1:2])
    return(dist)
  }
  dm <- do.call(cbind,lapply(1:nrow(df),d,df))
  return(as.dist(dm))
}

df<- na.omit(df)

# Clustering with Hclust 
clusters = cutree(hclust(dist(df,method = "manhattan")), k=10) # get 10 clusters

# function to find medoid in cluster i
clust.centroid = function(i, dat, clusters) {
  ind = (clusters == i)
  colMeans(dat[ind,])
}

sd<- sapply(unique(clusters), clust.centroid, df, clusters)
sd<- t(sd)
sd <- as.data.frame(sd)
df$clust <- clusters

# Mapping our clusters on the map
MapSF <- qmap('San Francisco, USA', zoom = 12, maptype = "roadmap", legend = 'topright')
MapSF + geom_point(aes(x = long, y = lat, colour = factor(clust)), data = df )+ scale_color_discrete("Clusters") +
   geom_point(data=sd, aes(x=long, y=lat), color="black")


## Finding closest Bus Stops to Cluster's Centroids
require(dplyr)
DB1 <- data.frame(location_id=1:10,LATITUDE=sd$lat,LONGITUDE=sd$long)
DB2 <- data.frame(location_id=11:122,LATITUDE=BusStopPoints$lat,LONGITUDE=BusStopPoints$long)

DistFun <- function(ID)
{
  TMP <- DB1[DB1$location_id==ID,]
  TMP1 <- distGeo(TMP[,3:2],DB2[,3:2])
  TMP2 <- data.frame(DB1ID=ID,DB2ID=DB2[which.min(TMP1),1],DistanceBetween=min(TMP1)      ) 
  print(ID)
  return(TMP2)
}

DistanceMatrix <- bind_rows(lapply(DB1$location_id, DistFun))
DistanceMatrix

library(magrittr)
busStopID <- rownames(DB2[DB2$location_id %in% DistanceMatrix$DB2ID , ])

# Getting optimal bus stop intersections
NewBusStop$Intersection
BusStopIntersections <- NewBusStop[rownames(NewBusStop) %in% busStopID,]$Intersection
BusStopIntersections

#finally write it all to the output files
path <- "/Users/bekterra/Desktop/Hackatons/Bus Stop Optimization/Bus/PotentailBus.csv"
saveRDS(BusStopIntersections, paste0(path ,".rds"))
write.table(BusStopIntersections, file=paste0(path ,".csv"), sep=",", row.names=FALSE)



# Plotting potential 10 bus stops
MapSF <- qmap('San Francisco, USA', zoom = 12, maptype = "roadmap", legend = 'topright')
MapSF + geom_point(aes(x = long, y = lat, colour = factor(clust)), data = df )+ scale_color_discrete("Clusters") +
  geom_point(data=locations, aes(x=lon, y=lat), shape=23, fill="black", color="darkred",size=3)
geom_label(aes(fill = "black"), colour = "white", fontface = "bold")
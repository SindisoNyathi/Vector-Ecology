#**************************************************************************************#
#15/05
#Sindiso Nyathi.
#Goal: Spatial autocorrelation analysis with Moran's I.
#Final Checks. May 15th.
#**************************************************************************************#

#********************************************************************************#
#Preliminary. Load libraries, set working directories, and read in files. 
library(spdep)
library(sp)

setwd("~/Box/Sindiso Nyathi's Files/Vector Control/Whole Data")

latlong <- readRDS("Data Files/Final LatLong.rds")
vecdata <- readRDS("Data Files/Vector Abundance Dataset.rds")

colnames(vecdata)
vecdata <- vecdata[,c(1, 2, 3, 4, 5, 25, 35)]
summary(vecdata)

#Aggregate the vecdata. To get total counts. 
vecdata_agg_overall <- aggregate(vecdata[,6], 
                         by = list(vecdata$Site, vecdata$HouseID),
                         FUN = mean, na.rm = TRUE)

#Rename columns. 
colnames(vecdata_agg_overall) <- c("Site", "HouseID", "MeanPupae")

#Aggregate by season. 
vecdata_agg_season <- aggregate(vecdata[,6], 
                                 by = list(vecdata$Site, vecdata$HouseID, 
                                           vecdata$Season),
                                 FUN = mean, na.rm = TRUE)

colnames(vecdata_agg_season) <- c("Site", "HouseID", "Season", "MeanPupae")
#********************************************************************************#

#********************************************************************************#
#Merge the house file with the latlong file.  
moran <- merge(vecdata_agg_overall, latlong, by = c("HouseID"))
summary(moran)

#Create list of sites. 
sites <- c("Kisumu", "Ukunda", "Msambweni", "Chulaimbo")
moran_store <- as.data.frame(matrix(nrow = 4, ncol = 6))
colnames(moran_store) <- c("Site", "I.mc", "I.a", "p.mc", "p.test",  "AveCard")
moran_store[,1] <- sites

for (i in 1:4){
  
  #Subset for each site. 
  moran_site <- moran[which(moran$Site.x == sites[[i]]),]

  #Calculate the distance matrix using the lat and long coordinates.
  site_coords <- as.matrix(cbind(moran_site$UTMLong, moran_site$UTMLat))
  site_coords <- SpatialPoints(site_coords)
  
  coordsys <- "+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  proj4string(site_coords) <- CRS(coordsys)
  
  #Use a distance of 250m
  site_dist <- dnearneigh(site_coords, 0, 500, row.names = moran_site$HouseID)
  
  #ite_nbdists <- nbdists(site_dist, cbind(moran_site$x, moran_site$y), longlat = T) 
  
  #inverse_dist_weights <- lapply(site_nbdists, function(x) 1/x)
    
  #Create the list of neighbors
  site_nblist <- nb2listw(site_dist, zero.policy = TRUE, style = "W")
  
  #The mcmc moran. 
  out <- moran.mc(moran_site$MeanPupae, site_nblist, zero.policy = TRUE, nsim = 499, adjust.n = T)
  out2 <- moran.test(moran_site$MeanPupae, site_nblist, zero.policy = TRUE, adjust.n = T, rank = T, na.action = na.omit)
  
  #Store Values
  moran_store[i,2] <- out$statistic
  moran_store[i,3] <- out2$statistic
  moran_store[i,4] <- out$p.value
  moran_store[i,5] <- out2$p.value
  moran_store[i,6] <- mean(card(site_dist))
  
  #Print outout. 
  print(paste(sites[[i]]," done", sep = ""))
  
}

#Write the file for this specific distance. 
write.csv(moran_store, "Data Files/Moran. Overall. 500.csv")

#Repeat for seasons totals. 
seasons <- unique(vecdata_agg_season$Season)
moran2 <- merge(vecdata_agg_season, latlong, by = c("HouseID"), all.x = TRUE)
summary(moran2)
moran_sea_store <- as.data.frame(matrix(nrow = 4, ncol = 6))
colnames(moran_sea_store) <- c("Season", "I.mc", "I.a", "p.mc", "p.test",  "AveCard")
moran_sea_store[,1] <- seasons

for (i in 1:4) {
  
  moran_site <- moran2[which(moran2$Site.x == sites[[i]]),]
  
  for (j in 1:4)
  {
    moran_site_sea <- moran_site[which(moran_site$Season == seasons[j]),]
    
    #Calculate the distance matrix using the lat and long coordinates.
    site_coords <- as.matrix(cbind(moran_site_sea$UTMLong, moran_site_sea$UTMLat))
    
    #Use a distance of 150m
    site_dist <- dnearneigh(site_coords, 0, 150)
    
    #Create the list of neighbors
    site_nblist <- nb2listw(site_dist, zero.policy = TRUE, style = "W")

    out <- moran.mc(moran_site_sea$MeanPupae, site_nblist, zero.policy = T, nsim = 499)
    out2 <- moran.test(moran_site_sea$MeanPupae, site_nblist, zero.policy = TRUE, adjust.n = T, rank = T, na.action = na.omit)
    
    moran_sea_store[j,1] <- seasons[j]
    moran_sea_store[j,2] <- out$statistic
    moran_sea_store[j,3] <- out2$statistic
    moran_sea_store[j,4] <- out$p.value
    moran_sea_store[j,5] <- out2$p.value
    moran_sea_store[j,6] <- mean(card(site_dist))
  }
  write.csv(moran_sea_store, paste("Data Files/Moran. Overall. ", sites[i], ".csv", sep = ""))
}
#Done No spatial autocorrelation. 
#********************************************************************************#

#********************************************************************************#


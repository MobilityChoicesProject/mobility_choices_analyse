library(R.cache)
library(dplyr)
library(foreach)
library(data.table)
library(lubridate)
library(sp)

################ Set cache root path (default: ~/.Rcache) ###################
setCacheRootPath(path="~/.Rcache")

############## LOAD AND PREPARE DATA ###################
doIT <- function(current_user_status){
########## Query data #########################################
  if("admin" %in% current_user_status$access){
    subs_geointersect =  databaseconnector("MobilityChoices","AnonymizedTrack", "find", '{}', '{"_id" : true, "sections" : true, "mobilityUserId":true, "reason":true, "date":true}')
  } else if("user" %in% current_user_status$access){
    polygon <- current_user_status$regionpolygon@polygons[[1]]@Polygons[[1]]@coords
    coordinates <- toJSON(polygon)
    queryUser = paste0('{"sections.coordinates": { "$geoIntersects": {"$geometry": {"type": "Polygon",
                                    "coordinates":[', coordinates, ']}}}}')
    subs_geointersect =  databaseconnector("MobilityChoices","AnonymizedTrack", "find", queryUser, '{"_id" : true, "sections" : true, "mobilityUserId":true, "reason":true, "date":true}')
  }

######### PREPARE DATA #######################################
  result1 <- subs_geointersect %>% select("_id", "mobilityUserId", "date", "reason", "sections")
  colnames(result1) <- c("id", "mobilityUserId", "date", "reason", "sections")
  result <- data.frame(Id = character(), User=character(), Track=data.frame())
  
  #for each track 
  for(i in 1:nrow(result1)) {
    currTrack <- result1[i,]
    
    sectionsdf <- data.frame(transportmode=character(), duration=numeric(), distance=numeric(), coords=character())
   # for each section
     foreach(x = currTrack$sections, .packages="foreach") %do% {
      currSec <- data.frame(transportmode=x$transportMode, duration=x$duration, distance=x$distance, coords=x$coordinates$json_dump, stringsAsFactors = FALSE)
      currSec["transportmode"] <- lapply(currSec["transportmode"], factor)
      sectionsdf <- rbind(sectionsdf,currSec)
     }
     
     trackdf <- data.frame(date=currTrack$date, reason=currTrack$reason, sections=sectionsdf)
     trackdf %>% group_by(date)

     currentdf <- data.frame(Id = currTrack$id, User=currTrack$mobilityUserId, Track=trackdf)
     result <- rbind(result,currentdf)
  }
  
  result <- result %>% group_by(Id, User,Track.date)
  print(nrow(nest(result)))
  return(nest(result))
}

###################### LOAD CACHE INTERFACE ####################################
loadTrackData <- function(current_user_status){
  # try to loadCache
  key <- list(current_user_status$userid)
  data <- loadCache(key)
  if(!is.null(data)){
     return(data)
  }
  
  # if data is not available in cache, generate and save it
  data <- doIT(current_user_status)
  newCacheData <- NULL
  newCacheData$data <- data
  newCacheData$timestamp <- Sys.time()
  saveCache(newCacheData, key=key, comment="loadTrackData()", compress = TRUE)
  return(newCacheData)
}

############### REFRESH CACHE ###############################################
cleanCacheAndReloadData <- function(current_user_status){
  
  withProgress(message = 'Daten neu laden', value = 0, {
  key <- list(current_user_status$userid)
  
  # delete old cache
  incProgress(0.3, detail = paste("Cache leeren"))
  file.remove(findCache(key=key))
  
  # generate new data (olap layer)
  incProgress(0.5, detail = paste("Verkehrsdaten herunterladen und aufbereiten"))
  data <- doIT(current_user_status)
  
  newCacheData <- NULL
  newCacheData$data <- data
  newCacheData$timestamp <- Sys.time()
  # save new cache data
  incProgress(0.8, detail = paste("Daten speichern"))
  saveCache(newCacheData, key=key, comment="loadTrackData()", compress=TRUE)
  
  })
  return(newCacheData)
}

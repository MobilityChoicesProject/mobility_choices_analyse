#################### AUTOMATIC CACHE RELOAD FOR EACH USER #####################
library(mongolite)
library(R.cache)
library(raster)
library(plyr)
library(dplyr)
library(foreach)
library(data.table)
library(lubridate)
library(sp)
library(tidyr)
library(jsonlite)

  url <- "mongodb://dbuser:password@url:port/dbname"
  urlMC <- "mongodb://dbuser:password@url:port/dbname"
  database <- mongo(collection = "AnalyseUser",url = url)
  databaseMC <- mongo(collection = "AnonymizedTrack",url = urlMC)
  databaseRegion <- mongo(collection = "Region", url = url)
  # LOAD ALL USERS FROM DB
  users <- database$find("{}", "{}")
  
  setCacheRootPath(path="~/.Rcache")

  # FOR EACH USER
  for(i in 1:nrow(users)) {
    newCacheFileContent <- NULL
    currentUser <- users[i,]
    key <- list(currentUser$`_id`)
    # RELOAD DATA
    if("admin" %in% currentUser$role){
      subs_geointersect =  databaseMC$find('{}', '{"_id" : true, "sections" : true, "mobilityUserId":true, "reason":true, "date":true}')
    } else if("user" %in% currentUser$role){
      regionPolygon <- NULL
      regionData <- databaseRegion$find(paste0('{"X_id" :"', currentUser$`_id`, '"}'), "{}")
      if(empty(regionData)){
        regionPolygon <- NULL
      }else{
        s <- shapefile(regionData$polygon)
        regionPolygon <- s
      }
      polygon <- regionPolygon@polygons[[1]]@Polygons[[1]]@coords
      coordinates <- toJSON(polygon)
      queryUser = paste0('{"sections.coordinates": { "$geoIntersects": {"$geometry": {"type": "Polygon",
                                    "coordinates":[', coordinates, ']}}}}')
      subs_geointersect =  databaseMC$find(queryUser, '{"_id" : true, "sections" : true, "mobilityUserId":true, "reason":true, "date":true}')
    }
    
    result1 <- subs_geointersect %>% dplyr::select("_id", "mobilityUserId", "date", "reason", "sections")
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
    
    newCacheFileContent$data <- nest(result)
    newCacheFileContent$timestamp <- Sys.time()
    
    # REMOVE OLD CACHE FILE & SAVE NEW
    if(is.null(findCache(key=key))){
      print("cache does not exist yet")
      saveCache(newCacheFileContent, key=key, comment="reloadTrackData()", compress=TRUE)
    }else{
      file.remove(findCache(key=key))
      saveCache(newCacheFileContent, key=key, comment="reloadTrackData()", compress=TRUE)
    }
    print(currentUser$`_id`)
  }




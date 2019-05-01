# update structure of mongodb AnonymizedTrack
library(mongolite)
library(purrr)
library(geojsonR)
library(jsonlite)

# DB connection
url <- "mongodb://dbuser:password@url:port/dbname"
database <- mongo(collection = "AnonymizedTrack",
                  url = url)

result <- database$find("{}", '{}')

#for each track in result
for(i in 1:nrow(result)) {
  row <- result[i,]
  
  reason <- row$reason
  date <- row$date
  # check if reason is null / set default value if necessary
  if(is.na(reason)){
    reason <- "other purpose"
  }
  
  #check if date is null  / set default value if necessary
  if(is.na(date)){
    time <- row$sections[[1]][,"start"]$timestamp[1]
    time = as_datetime(time/1000, tz="GMT") 
    tm <- as.POSIXlt(time, "UTC", "%Y-%m-%dT%H:%M:%S")
    time <- strftime(tm , "%Y-%m-%dT%H:%M:%SZ", options("digits.secs"=3))
    date <- time
  }
  
  #handle sections
  newSections <- row$sections
  coordinatesPerSection <- newSections[[1]][ , "coordinates"]
  transportmodes <- newSections[[1]][ , "transportMode"]
  
  if(is.null(coordinatesPerSection$type)){
    newList <- list()
    newTimestamps <- list()
    #for each section in a track
    for(j in 1:length(coordinatesPerSection)){
      
      #get coordinates of current section
      currentSectionCoordinates <- coordinatesPerSection[j]
      
      # save coordinates in new (structured) geojson-object
      allCoordinates <- currentSectionCoordinates[[1]]
      
      # init new GeoJSON object
      init = TO_GeoJson$new()
      linestring_WITH_dat = list()
      timestamps = list()
      
      if(length(allCoordinates) != 0){
        
        for(x in 1:nrow(allCoordinates)) {
          currentRow <- allCoordinates[x, ]
          if(!is_empty(currentRow)){
            
            lon <- currentRow$lng
            lat <- currentRow$lat
            newArray <- c(lon, lat)
            
            if(!is.null(currentRow$time)){
              timestamps[[x]] <- currentRow$time
            }
            
            linestring_WITH_dat[[x]] <- newArray
          }
        }
      }
      
      #if there are no coordinates in the list for example type STATIONARY
      if(transportmodes[[j]] == "STATIONARY"){
        #get coordinate from start
        coordinatesStart <- newSections[[1]][ , "start"]
        coordinatesDF <- coordinatesStart[j,][,"coordinates"]
        data <- c(coordinatesDF[,"lng"],coordinatesDF[,"lat"])
        line_with = init$Point(data, stringify = TRUE)
      }else{
        
        line_with = init$LineString(linestring_WITH_dat, stringify = TRUE)
          
        # check if there are at least two vertices in linestring 
          if(length(unique(line_with$coordinates)) < 2){
            coordinatesStart <- newSections[[1]][ , "start"]
            coordinatesDF <- coordinatesStart[j,][,"coordinates"]
            data <- c(coordinatesDF[,"lng"],coordinatesDF[,"lat"])
            line_with = init$Point(data, stringify = TRUE)
          }
        }
     
      newList[[j]] <- list(type=line_with$type, coordinates=line_with$coordinates, json_dump=line_with$json_dump)
      newTimestamps[[j]] <- list(timestamps)
    }
  
    row$sections[[1]]$coordinates <- newList
    row$sections[[1]]$timestamps <- unlist(newTimestamps, recursive = FALSE)
    jsonRow <- toJSON(row$sections[[1]], auto_unbox = TRUE, digits = NA)
    
    query <- paste0('{"_id": { "$oid" : "', row$`_id`, '" } }')
   
    fieldsVal <- paste('{"$set": { "sections" :', jsonRow, ', "reason" :', toJSON(reason, auto_unbox = TRUE), ', "date":', toJSON(date, auto_unbox = TRUE),'}}')
    database$update(query=query, update=fieldsVal)
  
  }
}



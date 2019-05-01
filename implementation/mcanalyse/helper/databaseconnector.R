databaseconnector <- function(db, collection, operation, query, fieldsVal, sortVal, limitVal) {
  ######################## SELECT DATABASE ###################################
   if(db == "AnalyseMC"){
     url <- "mongodb://dbuser:password@url:port/dbname"
   }else if(db == "MobilityChoices"){
     url <- "mongodb://dbuser:password@url:port/dbname"
   }
  
  ######################## CONNECT TO DB COLLECTION ###########################
  database <- mongo(collection = collection,
                    url = url
  )
  
  ######################## QUERY DATA ##########################################
  result <- NULL
  
  if (missing(sortVal) && missing(limitVal)) {
    switch (operation,
      "find" = {
          result <- database$find(query, fields=fieldsVal)
      },
      "insert" = {
          result <- database$insert(query, fields= fieldsVal)
      },
      "delete" = {
        result <- database$remove(query)
      },
      "update" = {
        result <- database$update(query, fieldsVal)
      },
      "aggregate" = {
        result <- database$aggregate(query)
      }
    )
  } else {
    result <- database$find(query = fieldsVal, sort = sortVal, limit = limitVal)
  }
}

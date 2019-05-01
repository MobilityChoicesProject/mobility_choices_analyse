################ MODULE UI FUNCTION ############################
usersUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  mainPanel(id = "mainUsers", width = 12,
            fluidRow(
              column(width = 12,
                box(id = "newUserBox",
                  collapsible = TRUE,
                  collapsed = TRUE,
                  width = 12,
                  title = "Neuen Client anlegen: ",
                  selectInput(
                    ns("role"),
                    label = "Rolle",
                    choices = list("Client" = "user", "Administrator" = "admin"),
                    selected = "user"
                  ),
                  textInput(ns("username"), "Benutzername*"),
                  textInput(ns("password1"), "Passwort*"),
                  textInput(ns("password2"), "Passwort wiederholen*"),
                  textInput(ns("name"), "Name der Kontaktperson"),
                  textInput(ns("email"), "Emailadresse der Kontaktperson"),
                  textAreaInput(ns("description"), "Beschreibung"),
                  strong("Region definieren*"),
                  shinyjs::hidden(textOutput(ns('defineRegion'))),
                  br(),
                  br(),
                  leafletOutput(ns("defineRegionMap"), height = 300),
                  br(),
                  actionButton(ns("undo"), "Schritt zurück"),
                  br(),
                  br(),
                  DT::dataTableOutput(ns('myRegion')),
                  br(),
                  actionButton(ns("createNewUser"), "Neuen Client anlegen")
                ),
                box(
                  title = "Alle Clients",
                  width = 12,
                  DT::dataTableOutput(ns('allUsersDataTable'))
                )
            )))
}

#################### MODULE SERVER FUNCTION ###########################
users <- function(input, output, session, current_user_status, places, land, bund, germany) {
    ############## DEFINE VARIABLES #####################
    reloadList <- reactiveVal()
    reloadList(TRUE)
    myusers <- reactiveVal()
    
    proxy <- leafletProxy("defineRegionMap")
    proxyRegion <- leafletProxy("currentUserRegionMap")
    myRegion <- reactiveVal()
    myRegion(data.frame(Region = character()))
    myPolygon <- reactiveVal()
    myPolygon(NULL)
    selectedPolygon <- reactiveVal()
    selectedPolygon(NULL)
    previousPolygonStates <- reactiveVal(list())
    
    #################### OBSERVE ROLE SELECTION  - SHOW MAP IF NECESSARY ################
    output$defineRegion <- renderText("Clients mit Administratorrechten brauchen keine Begrenzung der Region")
    observeEvent(input$role, {
      if (input$role == "admin") {
        shinyjs::hide("defineRegionMap")
        shinyjs::hide("myRegion")
        shinyjs::show("defineRegion")
        shinyjs::hide("undo")
      } else {
        shinyjs::show("defineRegionMap")
        shinyjs::show("myRegion")
        shinyjs::hide("defineRegion")
        shinyjs::show("undo")
      }
    })
    
    ################### CREATE NEW USER BUTTON OBSERVER ##############################
    observeEvent(input$createNewUser, {
      if (input$username != "" &&
          input$password1 != "" && input$password2 != "") {
        if (input$password1 == input$password2) {
          if (empty(myRegion()) && input$role != "admin") {
            shinyjs::alert("Es wurde noch keine Region definiert!")
          } else {
            # isolate values
            username <- isolate(input$username)
            newPassword <- isolate(input$password1)
            role <- isolate(input$role)
            name <- isolate(input$name)
            email <- isolate(input$email)
            description <- isolate(input$description)
            
            # check if duplicated entry
            if (username %in% myusers()$username) {
              shinyjs::alert("Es existiert bereits ein Benutzer mit diesem Name. Bitte verwenden Sie einen anderen.")
            } else {
              # hash password
              hashedPassword <- hashpw(newPassword)
              
              # create new user in mongodb
              username = c(username)
              password = c(hashedPassword)
              role = c(role)
              df = data.frame(username, password, role)
              jsonuser <- toJSON(df, pretty = TRUE)
              databaseconnector("AnalyseMC", "AnalyseUser", "insert", fromJSON(jsonuser), "{}")
              
              # create new profile
              id <- databaseconnector("AnalyseMC", "AnalyseUser", "find", paste0('{"username" : "', username, '"}'), '{"_id": true}')
              current_user_status$userid <- id
              userid = c(id)
              name = c(name)
              email = c(email)
              description = c(description)
              profileDf = data.frame(userid, name, email, description)
              jsonprofile <- toJSON(profileDf, pretty = TRUE)
              databaseconnector("AnalyseMC", "Profile", "insert", fromJSON(jsonprofile), "{}")
              
              #create new region
              if (role != "admin") {
                outfile <- paste('shapefiles/', username, '.shp', sep = "")
                print(file.info(outfile)[c("mode", "uname")])
                shapefile(selectedPolygon(), outfile, overwrite = TRUE)
                regionMatrix <- as.matrix(myRegion())
                regionVector <- as.vector(regionMatrix)
                result <- paste(regionVector, collapse = " - ")
                label = c(result)
                polygon = c(outfile)
                regionDf = data.frame(userid, label, polygon)
                jsonregion <- toJSON(regionDf, pretty = TRUE)
                databaseconnector("AnalyseMC", "Region", "insert", fromJSON(jsonregion), "{}")
              }
              
              # Felder leeren
              shinyjs::reset("username")
              shinyjs::reset("password1")
              shinyjs::reset("password2")
              shinyjs::reset("role")
              shinyjs::reset("name")
              shinyjs::reset("email")
              shinyjs::reset("description")
              myRegion("Bisher wurde keine Region definiert")
              shinyjs::show("defineRegionMap")
              proxy %>% removeShape("Selected")
              
              # success Meldung
              showNotification(
                "Client wurde erfolgreich angelegt",
                action = NULL,
                duration = 5,
                closeButton = TRUE
              )
              shinyjs::alert("Client wurde erfolgreich angelegt")
              
              # Liste "Alle Benutzer" aktualisieren
              reloadList(FALSE)
              reloadList(TRUE)
            }
          }
        } else{
          shinyjs::alert("Passwörter stimmen nicht überein")
        }
      } else{
        shinyjs::alert("Bitte alle mit * markierten Pflichtfelder ausfÃ¼llen!")
      }
    })
    
    #################### LOAD & SHOW ALL USERS #######################
    # load users from db
    newusers <- reactive({
      if (reloadList()) {
        # load usergroups from database
        myusers(databaseconnector("AnalyseMC", "AnalyseUser", "find", '{}', '{"username":true}'))
        if (empty(myusers())) {
          return(data.frame(Benutzer = character()))
        } else{
          # create data frame for visualisation
          keeps <- c("username")
          newusers <- myusers()[keeps]
          
          colnames(newusers) <- c("Benutzername")
          return(newusers)
        }
      }
    })
    # show users in data table
    output$allUsersDataTable = DT::renderDataTable(newusers())
    
    ##################### MY REGION MAP ################################
      ############### show list of selected cities/states/... as data table #############
      output$myRegion = DT::renderDataTable(df <- data.frame(myRegion()))
      
      ################ render leaflet map & add shapes ##########################
      output$defineRegionMap <- renderLeaflet({
        leaflet(places, land, bund, germany) %>% setView(lng = 9.8902804, lat = 47.2807386, zoom = 8)  %>% addTiles() %>%
          addPolygons(data = places, color = "#444444", weight = 1, smoothFactor = 0.5, 
                      opacity = 1.0, fillOpacity = 0, 
                      highlightOptions = highlightOptions(color = "white", weight = 2, 
                                                          bringToFront = TRUE), group = c("Ort (AT)")) %>%
          addPolygons(data = bund, color = "#444444", weight = 1, smoothFactor = 0.5,
                      opacity = 1.0, fillOpacity = 0,
                      highlightOptions = highlightOptions(color = "white", weight = 2, 
                                                          bringToFront = TRUE ), group = c("Bundesland/Kanton")) %>%
          addPolygons(data = land, color = "#444444", weight = 1, smoothFactor = 0.5, 
                      opacity = 1.0, fillOpacity = 0, 
                      highlightOptions = highlightOptions( color = "white",  weight = 2,
                                                           bringToFront = TRUE), group = "Land") %>%
          addPolygons(data = germany, color = "#444444", weight = 1, smoothFactor = 0.5, 
                      opacity = 1.0, fillOpacity = 0,
                      highlightOptions = highlightOptions(color = "white", weight = 2, 
                                                          bringToFront = TRUE), group = "Lindau") %>%
          # Layers control
          addLayersControl(
            baseGroups = c("Ort (AT)", "Bundesland/Kanton", "Land", "Lindau"),
            options = layersControlOptions(collapsed = FALSE)
          )
      })
      
      ################## observe users selection of cities/states/... merge polygon ##############
      observeEvent(input$defineRegionMap_shape_click, {
        # update the location selectInput on map clicks
        click <- input$defineRegionMap_shape_click
        
        #pulls lat and lon from shiny click event
        lat <- click$lat
        lon <- click$lng
        
        #puts lat and lon for click point into its own data frame
        coords <- as.data.frame(cbind(lon, lat))
        
        #converts click point coordinate data frame into SP object, sets CRS
        point <- SpatialPoints(coords)
        proj4string(point) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
        
        #retrieves polygon in which the click point resides
        if (!is.null(click$group)) {
          if (click$group == "Ort (AT)") {
            currentSelectedPolygon <- places[point, ]
            region <- paste(currentSelectedPolygon$locname, ", ", currentSelectedPolygon$country, sep = "")
          } else if (click$group == "Land") {
            currentSelectedPolygon <- land[point, ]
            region <- paste(currentSelectedPolygon$locname, sep = "")
          } else if (click$group == "Bundesland/Kanton") {
            currentSelectedPolygon <- bund[point, ]
            region <- paste(currentSelectedPolygon$locname, ", ", currentSelectedPolygon$country, sep = "")
          } else if (click$group == "Lindau") {
            currentSelectedPolygon <- germany[point, ]
            region <- paste("Deutschland, ", currentSelectedPolygon$locname, sep = "")
          }
          
          # add selectedPolygon (= previous selectedPolygon) to list with previous selected Polygon states
          if (length(previousPolygonStates()) == 0) {
            newList <- list(isolate(selectedPolygon()))
            previousPolygonStates(newList)
          } else {
            polyList <- previousPolygonStates()
            polyList[[length(polyList) + 1]] <- isolate(selectedPolygon())
            previousPolygonStates(polyList)
          }
          
          # add new shape to selectedPolygon
          if (is.null(selectedPolygon())) {
            selectedPolygon(spTransform(currentSelectedPolygon, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")))
          } else {
            selectedPolygon(aggregate(spRbind(selectedPolygon(), spTransform(currentSelectedPolygon, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")))))
            plot(selectedPolygon())
          }
          
          #add new region to list
          newVal <- list(Region = region)
          myRegion(rbind(myRegion(), newVal, stringsAsFactors = FALSE))
          
          proxy %>% removeShape("Selected")
          proxy %>% addPolygons(
            data = selectedPolygon(),
            fillColor = "green",
            fillOpacity = 0.5,
            color = "black",
            weight = 3,
            stroke = T,
            layerId = "Selected"
          )
        }
      })
      
      #################### undo last selection #########################
      observeEvent(input$undo, {
        if (nrow(myRegion()) > 0) {
          # remove last entry from myRegion
          newRegions <- myRegion()[-nrow(myRegion()), ]
          myRegion(data.frame(Region = character()))
          for (i in newRegions) {
            newVal <- list(Region = i)
            myRegion(rbind(myRegion(), newVal, stringsAsFactors = FALSE))
          }
          
          # retrieve last inserted Polygonstatelist and remove last state from list with previous states and show that last state on map
          previousPolygonStatesList <- previousPolygonStates()
          length(previousPolygonStatesList)
          if (length(previousPolygonStatesList) == 0) {
            proxy %>% removeShape("Selected")
          } else {
            previousPolygonState <- previousPolygonStatesList[[length(previousPolygonStatesList)]]
            previousPolygonStatesList[[length(previousPolygonStatesList)]] <- NULL
            previousPolygonStates(previousPolygonStatesList)
            selectedPolygon(previousPolygonState)
            
            proxy %>% removeShape("Selected")
            if (length(previousPolygonStatesList) > 0) {
              proxy %>% addPolygons(
                data = selectedPolygon(),
                fillColor = "green",
                fillOpacity = 0.5,
                color = "black",
                weight = 3,
                stroke = T,
                layerId = "Selected"
              )
            }
          }
        }
      })
      
  }
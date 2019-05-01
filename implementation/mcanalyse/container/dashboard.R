################ MODULE UI FUNCTION ##############################
dashboardUI<- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  mainPanel(id="mainDashboard",  width = 12,
            fluidRow(
              column( width=12,
                      box(
                        width=12,
                        title="Informationen zum Dashboard",
                        collapsible = TRUE,
                        collapsed = TRUE,
                        tagList(
                          strong("AUF DEM DASHBOARD KÖNNEN DIE FÜR SIE FREIGEGEBENEN VERKEHRSDATEN AUSGEWERTET UND ANALYSIERT WERDEN."),
                          br(),br(),
                          strong("Begriffsdefinitionen"),
                          p("Weg: ein von einem App-Benutzer zurückgelegter Weg - besteht aus einem oder mehreren Wegabschnitten. z.B. 'Fussweg - Bus - Fussweg - Zug' ist ein Weg aus 4 Wegabschnitten."),
                          p("Wegabschnitt: Teilabschnitt eines Weges - mit jedem Verkehrsmittelwechsel startet ein neuer Wegabschnitt."),
                          br(),
                          strong("Übersicht über die Verkehrsdaten"),
                          p("ALLE WEGE können gefiltert und angezeigt werden."),
                          p("Dazu können ggf. Filter aktiviert werden (durch anklicken der Checkboxen auf der linken Seite z.B. 'Zeitraum angeben'). Wird kein Filter ausgewählt, werden alle Wege angezeigt."),
                          p("Die Ergebnisse werden zum Einen in einer Tabelle in Listenform darunter angezeigt, zum Anderen auf der Karte (rechts) grafisch dargestellt. Zusätzlich wird eine Zusammenfassung der Ergebnisse übersichtlich, für jedes Verkehrsmittel in den farbigen Boxen (ganz oben) angezeigt."),
                          p("Die Ergbnisliste zeigt wieviele & welche Wege (mit Zeitstempel) den Filterkriterien entsprechen. Durch Klick auf einen Weg (Listeneintrag), werden rechts davon die Details zu diesem Weg angezeigt."),
                          p("Die farbigen Ergbenisboxen ganz oben, liefern für jedes Verkehrsmittel zusammenfassenden Informationen: Wieviel Prozent der zurückgelegten Kilometer wurden mit diesem Verkehrsmittel zurückgelegt, wieviele der gezählten Wegabschnitte für dieses Verkehrsmittel / im Verhältnis zu der Gesamtanzahl an Wegabschnitten, (wieviel Prozent der Wegabschnitte (Anzahl) mit diesem Verkehrsmittel zurückgelegt wurden), die durchschnittliche Dauer eines Wegabschnittes mit diesem Verkehrsmittel (in Minuten) und die durchschnittliche Länge eines Wegabschnittes mit diesem Verkehrsmittel in Kilometern."),
                          br(),
                          strong("Verkehrsflüsse analysieren"),
                          p("Nur Wegabschnitte die durch den definierten Radius führen, werden ausgewertet."),
                          p("Die grundlegende Aufteilung in diesem Tab entspricht dem Aufbau der Übersicht. Filter, Karte und Ergebnisse befinden sich an den gleichen Stellen. Die Liste der Ergbenisse zeigt hier jedoch nur einzelne Wegabschnitte, nicht die ganzen Wege."),
                          p("Diese Ansicht dient in erster Hinsicht der Auswertung - welche Verkehrsmittel genau den definierten Bereich passieren. Dazu dienen die farbigen Übersicht-Boxen ganz oben. Sie zeigen wieviel Prozent der Wegabschnitte, die durch den definierten Bereich führen, mit dem jeweiligen Verkehrsmittel zurückgelegt wurden und die Anzahl an Wegabschnitten für dieses Verkehrsmittel im Verhältnis zu der Gesamtanzahl an Wegabschnitten."),
                          p("Vor die Ergebnisse angezeigt werden können, muss dieser Bereich definiert werden. Dafür kann durch Klick auf die gewünschte Stelle in der Karte ein Punkt gesetzt werden und zum Vergrößern/Verkleinern des ausgewählten Bereichs, der Radius bei den Filtermöglichkeiten angepasst werden."),
                          br()
                        )
                      )
                      ,
                tabBox(
                  width = 12,
                  id = ns("analyseTabset"), 
                  tabPanel("Übersicht über die erfassten Verkehrsdaten", uiOutput(ns("overviewUI"))),
                  tabPanel("Verkehrsflüsse analysieren", uiOutput(ns("frequencyUI")))
                )
              )
            )
  )
}

###################### MODULE SERVER FUNCTION #####################
dashboard <- function(input, output, session, current_user_status) {
  ###################### DEFINE VARIABLES #########################
  usergroups <- reactiveVal()
  dataFrame <- reactiveVal(data.frame(User=character(), Track.date=character()))
  currentTrack <- reactiveVal()
  currentSection <- reactiveVal()
  sectionsDetails <- reactiveVal()
  sectionsDetailsWithData <- reactiveVal()
  overview <- reactiveVal()
  overviewFreq <- reactiveVal()
  radius <- reactiveVal()
  radius(30)
  
  oldRadius <- reactiveVal()
  oldRadius(30)
  
  currentPoint <- reactiveValues()
  currentPoint$lat <- NULL
  currentPoint$lng <- NULL
  
  oldCurrentPoint <- reactiveValues()
  oldCurrentPoint$lat <- NULL
  oldCurrentPoint$lng <- NULL
  
  numAllSections <- reactiveVal()
  numAllSectionsFreq <- reactiveVal()
  frequencies <- reactiveVal(data.frame(User=character(), Track.date=character(), Track.sections.transportmode=character()))
 
  overviewValues <- reactiveVal()
  
  
  
  ######################## OVERVIEW UI ###################################
  output$overviewUI <- renderUI({
    ns <- session$ns
    usergroups(databaseconnector("AnalyseMC", "Usergroup", "find", paste0('{"X_id" :"', current_user_status$userid, '"}'),"{}"))
    usergroupNames <- as.vector(usergroups()$name)
    fluidPage(
      fluidRow(
        column(
          width = 12,
          infoBoxOutput(ns("carBox")),
          infoBoxOutput(ns("busBox")),
          infoBoxOutput(ns("trainBox")),
          infoBoxOutput(ns("bikeBox")),
          infoBoxOutput(ns("footBox")),
          infoBoxOutput(ns("stationaryBox"))
        )
      ),
      fluidRow(
        tags$head(
          tags$style(type = 'text/css',".filterpanel {background-color: #ffffff;}")
        ),
        column(
          width=4,
          h4(strong(icon("filter"),"Filter")),
          checkboxInput(ns("showAllUsers"), "Alle Wege anzeigen", TRUE),
          checkboxInput(ns("showUsergroupSelector"), "Nur ausgewählte Usergruppen anzeigen", FALSE),
          shinyjs::hidden(
            wellPanel(
              id= ns("checkboxPanel"),
              class="filterpanel",
              checkboxGroupInput(ns("checkbox"),"Usergruppen", choices = usergroupNames, selected = usergroupNames)
            )
          ),
          checkboxInput(ns("showDateSelector"), "Zeitraum angeben", FALSE),
          shinyjs::hidden(
            wellPanel(
              id= ns("datesPanel"),
              class="filterpanel",
              dateRangeInput(ns("dates"), label = "Zeitraum definieren", start = "2018-04-01" )
            )
          ),
          checkboxInput(ns("showWeekdaySelector"), "Nur bestimmte Wochentage auswerten", FALSE),
          shinyjs::hidden(
            wellPanel(
              id= ns("checkWeekdayGroupPanel"),
              class="filterpanel",
              checkboxGroupInput(ns("checkWeekdayGroup"), label = "Wochentage auswählen", 
                                 choices = list("Montag" = "Mon", "Dienstag" = "Tue", "Mittwoch" = "Wed", "Donnerstag" = "Thu", "Freitag" = "Fri", "Samstag"="Sat", "Sonntag"="Sun"),
                                 selected = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
            )
          ),
          checkboxInput(ns("showTransportmodeSelector"), "Nur bestimmte Verkehrsmittel anzeigen", FALSE),
          shinyjs::hidden(
            wellPanel(
              id= ns("checkTransportGroupPanel"),
              class="filterpanel",
              checkboxGroupInput(ns("checkTransportGroup"), label = "Verkehrsmittel", 
                                 choices = list("Auto" = "CAR", "Fahrrad" = "BIKE", "Fussgänger" = "NON_VEHICLE", "Bus" = "BUS", "Zug" = "TRAIN", "Stillstand"="STATIONARY"),
                                 selected = c("CAR", "BIKE", "NON_VEHICLE", "BUS", "TRAIN", "STATIONARY"))
            )
          ),
          checkboxInput(ns("showReasonSelector"), "Nur  Wege, die zu einen bestimmten Zweck zurückgelegt wurden", FALSE),
          shinyjs::hidden(
            wellPanel(
              id= ns("checkReasonPanel"),
              class="filterpanel",
              checkboxGroupInput(ns("checkReasonGroup"), label = "Wegezweck", 
                                 choices = list("Arbeitsplatz" = "workplace", "Dientstlich/Geschäftlich" = "business", "Schule/Ausbildung" = "education", "Bringen/Holen/Begleiten von Personen" = "bringing accompanying persons", "Einkauf" = "shopping", "Private Erledigung" = "private execution", "Privater Besuch" = "private visit", "Freizeit" = "leisure", "Anderer Zweck" = "other purpose"),
                                 selected = c("workplace", "business", "education", "bringing accompanying persons", "shopping", "private execution", "private visit", "leisure", "other purpose"))
            )
          ),
          actionButton(ns("doFilterOverview"), "Ergebnisse laden")
        ),
        column(
          width = 8,
          h4(strong("Zurückgelegte Wege")),
          leafletOutput(ns("selectedTracks"))
        )
      ),
      br(),
      fluidRow(
        column(
          width = 6,
          DT::dataTableOutput(ns("trackDataTable")),
          downloadButton(ns("downloadTrackData"), "Download")
        ),
        column(
          width = 6,
          shinyjs::hidden(
            wellPanel(
              id = ns("trackDetailsPanel"),
              h3("Weg Details"),
              uiOutput(ns("trackDetailsOutput"))
            )
          )
        )
      )
    )
  })
  
  ##################### FILTER RESULT OVERVIEW ##############################
  observeEvent(input$doFilterOverview, {
    # check if at least one filter is selected
    if(!input$showTransportmodeSelector & !input$showDateSelector & !input$showUsergroupSelector & !input$showWeekdaySelector & !input$showAllUsers){
      updateCheckboxInput(session, "showAllUsers", value = TRUE)
    }
    
    startdate <- input$dates[1]
    enddate <- input$dates[2]
    if(enddate < startdate){
      shinyjs::alert("Fehler: Enddatum muss nach dem angegebenen Startdatum liegen")
    }else{
      if(!input$showAllUsers){
        data <- current_user_status$trackData
        # Nutzergruppenfilter
        if(input$showUsergroupSelector){
          members <- filter(usergroups(), name %in% input$checkbox) %>%select(members)
          data <- filter(current_user_status$trackData, User %in% unlist(members))
        }
        # Zeitfilter
        if(input$showDateSelector){
          #print(str(data$Track.date))
          data <- filter(data, as.Date(Track.date) >= startdate & as.Date(Track.date) <= enddate)
        }
        
        # Wochentagfilter
        if(input$showWeekdaySelector){
          data <- filter(data, wday(Track.date, label=TRUE) %in% input$checkWeekdayGroup)
        }
        
        # Wegezweckfilter
        if(input$showReasonSelector){
          data <- unnest(data)
          data <- filter(data, Track.reason %in% input$checkReasonGroup)
          data <- group_by(data, User, Track.date)
          data <- nest(data)
        }
        
        # Verkehrsmittelfilter
        if(input$showTransportmodeSelector){
          data <- unnest(data)
          data <- filter(data, Track.sections.transportmode %in% input$checkTransportGroup)
          data <- group_by(data, User, Track.date)
          data <- nest(data)
        }
        
        dataFrame(data)
        calculateOverview()
        addPolylinesToLeaflet()
      }else{
        dataFrame(current_user_status$trackData)
        calculateOverview()
        addPolylinesToLeaflet()
      }
    }
  })
  
  ################### OBSERVER FOR FILTER SELECTION - OVERVIEW ###############
  observeEvent(input$showAllUsers, {
    if(input$showAllUsers){
      updateCheckboxInput(session, "showTransportmodeSelector", value = FALSE)
      updateCheckboxInput(session, "showWeekdaySelector", value = FALSE)
      updateCheckboxInput(session, "showDateSelector", value = FALSE)
      updateCheckboxInput(session, "showUsergroupSelector", value = FALSE)
      updateCheckboxInput(session, "showReasonSelector", value = FALSE)
      updateCheckboxGroupInput(session=session,
                               inputId="checkbox",
                               choices = as.vector(usergroups()$name),
                               selected = as.vector(usergroups()$name))
      updateCheckboxGroupInput(session=session,
                               inputId="checkWeekdayGroup",
                               choices = list("Montag" = "Mon", "Dienstag" = "Tue", "Mittwoch" = "Wed", "Donnerstag" = "Thu", "Freitag" = "Fri", "Samstag"="Sat", "Sonntag"="Sun"),
                               selected = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
      updateCheckboxGroupInput(session=session,
                               inputId="checkTransportGroup",
                               choices = list("Auto" = "CAR", "Fahrrad" = "BIKE", "Fussgänger" = "NON_VEHICLE", "Bus" = "BUS", "Zug" = "TRAIN", "Stillstand"="STATIONARY"),
                               selected = c("CAR", "BIKE", "NON_VEHICLE", "BUS", "TRAIN", "STATIONARY"))
      updateCheckboxGroupInput(session=session,
                               inputId="checkReasonGroup", 
                               choices = list("Arbeitsplatz" = "workplace", "Dientstlich/Geschäftlich" = "business", "Schule/Ausbildung" = "education", "Bringen/Holen/Begleiten von Personen" = "bringing accompanying persons", "Einkauf" = "shopping", "Private Erledigung" = "private execution", "Privater Besuch" = "private visit", "Freizeit" = "leisure", "Anderer Zweck" = "other purpose"),
                               selected = c("workplace", "business", "education", "bringing accompanying persons", "shopping", "private execution", "private visit", "leisure", "other purpose"))
      
    }
  })
  
  observeEvent(input$showReasonSelector, {
    if(input$showReasonSelector){
      shinyjs::showElement("checkReasonPanel")
      updateCheckboxInput(session, "showAllUsers", value = FALSE)
    }else{
      shinyjs::hideElement("checkReasonPanel")
    }
  })
  
  observeEvent(input$showWeekdaySelector, {
    if(input$showWeekdaySelector){
      shinyjs::showElement("checkWeekdayGroupPanel")
      updateCheckboxInput(session, "showAllUsers", value = FALSE)
    }else{
      shinyjs::hideElement("checkWeekdayGroupPanel")
    }
  })
  
  observeEvent(input$showUsergroupSelector, {
    if(input$showUsergroupSelector){
      shinyjs::showElement("checkboxPanel")
      updateCheckboxInput(session, "showAllUsers", value = FALSE)
    }else{
      shinyjs::hideElement("checkboxPanel")
    }
  })
  
  observeEvent(input$showDateSelector, {
    if(input$showDateSelector){
      shinyjs::showElement("datesPanel")
      updateCheckboxInput(session, "showAllUsers", value = FALSE)
    }else{
      shinyjs::hideElement("datesPanel")
    }
  })
  
  observeEvent(input$showTransportmodeSelector, {
    if(input$showTransportmodeSelector){
      shinyjs::showElement("checkTransportGroupPanel")
      updateCheckboxInput(session, "showAllUsers", value = FALSE)
    }else{
      shinyjs::hideElement("checkTransportGroupPanel")
    }
  })
  
  
  ################# INFO BOXES - OVERVIEW ############################
  output$bikeBox <- renderInfoBox({
    if(is.null(overview())){
      infoBox(
        "Fahrrad","", icon = icon("bicycle"),
        color = "olive", fill = TRUE, width = 1
      )
    }else{
      val <- overview()[which(overview()$transportmode == "Fahrrad"), ]
      valNum <- overviewValues()[which(overviewValues()$transportmode == "Fahrrad"),]
      if(nrow(val) == 0){
        infoBox(
          "Fahrrad","", icon = icon("bicycle"),
          color = "olive", fill = TRUE, width = 1
        )
      }else{
        infoBox(
          "Fahrrad", HTML(paste0(val$percent ,"%", ",  ", valNum$number, " / ", numAllSections()," (", valNum$percentNumber,"%)",br(), val$duration, "Min, ", val$distance, "KM")), icon = icon("bicycle"),
          color = "olive", fill = TRUE, width = 1
        )
      }
    }
  })
  output$carBox <- renderInfoBox({
    if(is.null(overview())){
      infoBox(
        "Auto","", icon = icon("car"),
        color = "blue", fill = TRUE, width = 1
      )
    }else{
      val <- overview()[which(overview()$transportmode == "Auto"), ]
      valNum <- overviewValues()[which(overviewValues()$transportmode == "Auto"),]
      if(nrow(val) == 0){
        infoBox(
          "Auto","", icon = icon("car"),
          color = "blue", fill = TRUE, width = 1
        )
      }else{
        infoBox(
          "Auto", HTML(paste0(val$percent ,"%", ",  ",valNum$number, " / ", numAllSections()," (", valNum$percentNumber,"%)",br(), val$duration, "Min, ", val$distance, "KM")), icon = icon("car"),
          color = "blue", fill = TRUE, width = 1
        )
      }
    }
  })
  output$stationaryBox <- renderInfoBox({
    if(is.null(overview())){
      infoBox(
        "Stillstand","", icon = icon("map-marker"),
        color="lime", fill=TRUE, width = 1
      )
    }else{
      val <- overview()[which(overview()$transportmode == "Stillstand"), ]
      valNum <- overviewValues()[which(overviewValues()$transportmode == "Stillstand"),]
      if(nrow(val) == 0){
        infoBox(
          "Stillstand","", icon = icon("map-marker"),
          color="lime", fill=TRUE, width = 1
        )
      }else{
        infoBox(
          "Stillstand", HTML(paste0(valNum$number, " / ", numAllSections()," (", valNum$percentNumber,"%)",br(), val$duration, " Min")), icon = icon("map-marker"),
          color = "lime", fill=TRUE, width = 1
        )
      }
    }
  })
  output$footBox <- renderInfoBox({
    if(is.null(overview())){
      infoBox(
        "Fussweg","", icon = icon("male"),
        color = "green", fill = TRUE, width = 1
      )
    }else{
      val <- overview()[which(overview()$transportmode == "Fussweg"), ]
      valNum <- overviewValues()[which(overviewValues()$transportmode == "Fussweg"),]
      if(nrow(val) == 0){
        infoBox(
          "Fussweg","", icon = icon("male"),
          color = "green", fill = TRUE, width = 1
        )
      }else{
        infoBox(
          "Fussweg", HTML(paste0(val$percent ,"%", ",  ", valNum$number, " / ", numAllSections()," (", valNum$percentNumber,"%)",br(), val$duration, "Min, ", val$distance, "KM")), icon = icon("male"),
          color = "green", fill = TRUE, width = 1
        )
      }
    }
  })
  output$trainBox <- renderInfoBox({
    if(is.null(overview())){
      infoBox(
        "Zug","", icon = icon("train"),
        color = "aqua", fill = TRUE, width = 1
      )
    }else{
      val <- overview()[which(overview()$transportmode == "Zug"), ]
      valNum <- overviewValues()[which(overviewValues()$transportmode == "Zug"),]
      if(nrow(val) == 0){
        infoBox(
          "Zug","", icon = icon("train"),
          color = "aqua", fill = TRUE, width = 1
        )
      }else{
        infoBox(
          "Zug", HTML(paste0(val$percent ,"%", ",  ", valNum$number, " / ", numAllSections()," (", valNum$percentNumber,"%)", br(),  val$duration, "Min, ", val$distance, "KM")), icon = icon("train"),
          color = "aqua", fill = TRUE, width = 1
        )
      }
    }
  })
  
  output$busBox <- renderInfoBox({
    if(is.null(overview())){
      infoBox(
        "Bus","", icon = icon("bus"),
        color = "light-blue", fill = TRUE, width = 1
      )
    }else{
      val <- overview()[which(overview()$transportmode == "Bus"), ]
      valNum <- overviewValues()[which(overviewValues()$transportmode == "Bus"),]
      if(nrow(val) == 0){
        infoBox(
          "Bus","", icon = icon("bus"),
          color = "light-blue", fill = TRUE, width = 1
        )
      }else{
        infoBox(
          "Bus",HTML(paste0(val$percent ,"%",",  ", valNum$number, " / ", numAllSections(), " (", valNum$percentNumber,"%)", br(),  val$duration, "Min, ", val$distance, "KM")), icon = icon("bus"),
          color = "light-blue", fill = TRUE, width = 1
        )
      }
    }
  })
  
  ######################### CALCULATE OVERVIEW - OVERVIEW ############################
  calculateOverview <- function(){
    result <- data.frame(transportationmode=character(), percent=numeric(), time=numeric(), way=numeric())
    if(!empty(dataFrame())){
      data <- unnest(dataFrame()) %>% select(Track.sections.transportmode, Track.sections.duration, Track.sections.distance)
      grp <- group_by(data, Track.sections.transportmode, Track.sections.duration, Track.sections.distance)
      newData <- summarise(grp)
      numAllSections(nrow(newData))
      
      test <- aggregate(newData[, 2:3], list(newData$Track.sections.transportmode), mean)
      sum <- aggregate(newData[, "Track.sections.distance"], list(newData$Track.sections.transportmode), sum)
      test$percent <- (100/sum(newData$Track.sections.distance) * sum$Track.sections.distance)
      
      testData <- newData[,"Track.sections.transportmode"]
      sums <- as.data.frame(table(testData))
      per <- function(x) 100/numAllSections() * x
      testResult <- cbind(sums[1:2], lapply(sums[2], per) )
      colnames(testResult) <- c("transportmode", "number", "percentNumber")
      testResult["percentNumber"] <-  round(testResult["percentNumber"], 2)
      
      testResult$transportmode <- str_replace_all(testResult$transportmode, "STATIONARY", "Stillstand") 
      testResult$transportmode <- str_replace_all(testResult$transportmode, "NON_VEHICLE", "Fussweg") 
      testResult$transportmode <- str_replace_all(testResult$transportmode, "BUS", "Bus")
      testResult$transportmode <- str_replace_all(testResult$transportmode, "TRAIN", "Zug")
      testResult$transportmode <- str_replace_all(testResult$transportmode, "CAR", "Auto") 
      testResult$transportmode <- str_replace_all(testResult$transportmode, "BIKE", "Fahrrad")
      
      overviewValues(testResult)
      
      test["Track.sections.duration"] <-  round(test["Track.sections.duration"], 2)
      test["Track.sections.distance"] <-  round(test["Track.sections.distance"], 2)
      test["percent"] <-  round(test["percent"], 2)
      colnames(test) <- c("transportmode", "duration", "distance", "percent")
      result <- test %>% select("transportmode", "percent", "duration", "distance")
      
      result$transportmode <- str_replace_all(result$transportmode, "STATIONARY", "Stillstand") 
      result$transportmode <- str_replace_all(result$transportmode, "NON_VEHICLE", "Fussweg") 
      result$transportmode <- str_replace_all(result$transportmode, "BUS", "Bus")
      result$transportmode <- str_replace_all(result$transportmode, "TRAIN", "Zug")
      result$transportmode <- str_replace_all(result$transportmode, "CAR", "Auto") 
      result$transportmode <- str_replace_all(result$transportmode, "BIKE", "Fahrrad")
      
    }else{
      overviewValues(data.frame(transportmode=character(), number=numeric(), percentNumber=numeric()))
      numAllSections(0)
    }
    overview(result)
  }
  
  ##################### RENDER LEAFLET MAP - OVERVIEW ######################
  proxy <- leafletProxy("selectedTracks")
  output$selectedTracks <- renderLeaflet({
    if("admin" %in% current_user_status$access){
      leaflet() %>% setView(lng = 9.8902804, lat = 47.2807386, zoom = 9)  %>% addTiles()
    }else{
      x <- as(extent(-180, 180, -90, 90), "SpatialPolygons")
      bbox <- bbox(current_user_status$regionpolygon)
      leaflet() %>% addPolygons(data = symdif(x,current_user_status$regionpolygon),
                                color = "white",
                                fillOpacity = 1,
                                stroke = T,
                                layerId = "myRegionLayer") %>% addTiles(options = providerTileOptions(minZoom = 4)) %>% fitBounds(bbox[1,1], bbox[2,1], bbox[1,2], bbox[2,2])
    }
  })
  
  ################ ADD POLYLINES TO LEAFLET - OVERVIEW ################################
  addPolylinesToLeaflet <- function(){
    proxy %>% clearShapes()
    if(!empty(dataFrame())){
      points <- dataFrame() %>% select(data)
      by(points, 1:nrow(points), function(row) {
        current <- row[[1]]
        coordinates <- current[[1]][,"Track.sections.coords"]
        by(coordinates, 1:nrow(coordinates), function(row) {
          if(!is.null(row$Track.sections.coords)){
            res = FROM_GeoJson(url_file_string = row$Track.sections.coords)
            if(res$type == "LineString"){
              leafletProxy("selectedTracks") %>%
                addPolylines(lng=res$coordinates[,1], lat=res$coordinates[,2], color = "#0000ff",
                             fillOpacity = 0,
                             weight = 2)
            }
            #TODO else if type == Point --> Stationary as circle?
            
          }
        })
      })
    }
    if("user" %in% current_user_status$access){
      x <- as(extent(-180, 180, -90, 90), "SpatialPolygons")
      bbox <- bbox(current_user_status$regionpolygon)
      leafletProxy("selectedTracks") %>% addPolygons(data = symdif(x,current_user_status$regionpolygon),
                                                     color = "black",
                                                     fillOpacity = 0.2,
                                                     stroke = T,
                                                     layerId = "myRegionLayer") %>% addTiles(options = providerTileOptions(minZoom = 4)) %>% fitBounds(bbox[1,1], bbox[2,1], bbox[1,2], bbox[2,2])
    }
  }
  
  ####################### TRACK DETAILS - OVERVIEW ###########################
  output$trackDetailsOutput <- renderUI({
    ns <- session$ns
    if(!is.null(currentTrack())){
      data <- currentTrack() %>% select(data)
      currentData <-data[[1]]
      currentDataDF <- as.data.frame(currentData)
      
      currentDataDF$Track.sections.transportmode <- str_replace_all(currentDataDF$Track.sections.transportmode, "STATIONARY", "Stillstand") 
      currentDataDF$Track.sections.transportmode <- str_replace_all(currentDataDF$Track.sections.transportmode, "NON_VEHICLE", "Fussweg") 
      currentDataDF$Track.sections.transportmode <- str_replace_all(currentDataDF$Track.sections.transportmode, "BUS", "Bus")
      currentDataDF$Track.sections.transportmode <- str_replace_all(currentDataDF$Track.sections.transportmode, "TRAIN", "Zug")
      currentDataDF$Track.sections.transportmode <- str_replace_all(currentDataDF$Track.sections.transportmode, "CAR", "Auto") 
      currentDataDF$Track.sections.transportmode <- str_replace_all(currentDataDF$Track.sections.transportmode, "BIKE", "Fahrrad") 
      
      grp <- group_by(currentDataDF, Track.sections.transportmode, Track.sections.duration, Track.sections.distance)
      sectionsWithData <- nest(grp)
      
      time <- sum(sectionsWithData$Track.sections.duration)
      way <- sum(sectionsWithData$Track.sections.distance)
      
      numeric_columns <- sapply(sectionsWithData, mode) == 'numeric'
      sectionsWithData[numeric_columns] <-  round(sectionsWithData[numeric_columns], 2)
      
      sections <- sectionsWithData %>% select(Track.sections.transportmode, Track.sections.duration, Track.sections.distance)
      colnames(sections) <- c("Verkehrsmittel", "Dauer (min)", "Weg (km)")
      sectionsDetails(sections)
      sectionsDetailsWithData(sectionsWithData)
      
      currentDataDF$Track.reason<- str_replace_all(currentDataDF$Track.reason, "workplace", "Arbeitsplatz") 
      currentDataDF$Track.reason <- str_replace_all(currentDataDF$Track.reason, "business", "Dienstlich/Geschäftlich") 
      currentDataDF$Track.reason <- str_replace_all(currentDataDF$Track.reason, "education", "Schule/Ausbildung")
      currentDataDF$Track.reason <- str_replace_all(currentDataDF$Track.reason, "bringing accompanying persons", "Bringen/Holen/Begleiten von Personen")
      currentDataDF$Track.reason <- str_replace_all(currentDataDF$Track.reason, "shopping", "Einkaufen") 
      currentDataDF$Track.reason <- str_replace_all(currentDataDF$Track.reason, "private execution", "Private Erledigung")
      currentDataDF$Track.reason <- str_replace_all(currentDataDF$Track.reason, "private visit", "Privater Besuch")
      currentDataDF$Track.reason <- str_replace_all(currentDataDF$Track.reason, "leisure", "Freizeit")
      currentDataDF$Track.reason <- str_replace_all(currentDataDF$Track.reason, "other purpose", "Anderer Zweck")
      
      reason <- currentDataDF$Track.reason[1]
      
      fluidRow(
        column(
          width=12,
          p(paste("Datum:", parse_date_time(currentTrack()$Track.date, 'ymd HMS'))),
          p(paste("Dauer:", round(time, digits = 2), "min")),
          p(paste("Weg: ", round(way, digits = 2), "km")),
          p(paste("Wegezweck: ", reason)),
          dataTableOutput(ns("sumDetailsDataTable"))
        )
      )
    }
  })
  
  ############## SECTIONS OF CURRENT TRACK DATA TABLE - OVERVIEW #################
  output$sumDetailsDataTable <- renderDataTable(sectionsDetails(), selection= "single",  options = list(scrollX= T))
  observeEvent(input$sumDetailsDataTable_rows_selected, {
    if(length(input$sumDetailsDataTable_rows_selected) > 0){
      currentSection(sectionsDetailsWithData()[input$sumDetailsDataTable_rows_selected,])
      drawCurrentSectionOverview()
    }else{
      currentSection(NULL)
      leafletProxy("selectedTracks") %>% removeShape("currentSectionLayer")
    }
  }, ignoreNULL = FALSE )
  
  ################# DRAW CURRENT SECTION  - OVERVIEW #######################
  drawCurrentSectionOverview <- function(){
    current <- currentSection()$data[[1]]
    currentNew <-as.data.frame(current)
    rowCoordinates <- currentNew %>% select(Track.sections.coords)
    by(rowCoordinates, 1:nrow(rowCoordinates), function(row) {
      if(!is.null(row$Track.sections.coords)){
        res = FROM_GeoJson(url_file_string = row$Track.sections.coords)
        if(res$type == "LineString"){
          leafletProxy("selectedTracks") %>%
            addPolylines(lng=res$coordinates[,1], lat=res$coordinates[,2], color = "yellow",
                         stroke = T,
                         layerId = "currentSectionLayer")
        }
        # TODO Point
      }
    })
  }
  
  ################## TRACK DATA TABLE OUTPUT - OVERVIEW ##########################
  output$trackDataTable <-  DT::renderDataTable(
    datatable(dataFrame() %>% select(User, Track.date), selection= "single", options = list(scrollX= T)) %>%
      formatDate("Track.date", "toLocaleString")
   )
  
  # get selected track and call function to draw it on map
  observeEvent(input$trackDataTable_rows_selected, {
    if(length(input$trackDataTable_rows_selected) > 0){
      currentTrack(dataFrame()[input$trackDataTable_rows_selected,])
      updateMapView()
      shinyjs::show("trackDetailsPanel")
    }else{
      currentTrack(NULL)
      leafletProxy("selectedTracks") %>% clearGroup("currentTrackLayer")
      shinyjs::hide("trackDetailsPanel")
    }
  }, ignoreNULL = FALSE )
  
  # draw selected track on leaflet map
  updateMapView <- function(){
    leafletProxy("selectedTracks") %>% clearGroup("currentTrackLayer")
    current <- currentTrack()$data[[1]]
    currentNew <-as.data.frame(current)
    rowCoordinates <- currentNew %>% select(Track.sections.coords)
    rowCoordinates <- as_tibble(rowCoordinates)
    by(rowCoordinates, 1:nrow(rowCoordinates), function(row) {
      if(!is.null(row$Track.sections.coords)){
        res = FROM_GeoJson(url_file_string = row$Track.sections.coords)
        if(res$type == "LineString"){
           leafletProxy("selectedTracks") %>%
            addPolylines(lng=res$coordinates[,1], lat=res$coordinates[,2], color = "red",
                         stroke = T,
                         group = "currentTrackLayer")
        }
        # TODO else if type is Point
      }
    })
  }
  
  ########################## DOWNLOAD OVERVIEW RESULT #########################
  output$downloadTrackData <- downloadHandler(
    filename = function() {
      "overviewResult.xlsx"
    },
    content = function(file) {
      if(!empty(dataFrame())){
        unnestedDF <- unnest(dataFrame())
        groupedDF <- unnestedDF %>% select(User, Track.date, Track.sections.transportmode, Track.sections.duration, Track.sections.distance)
        res <- group_by(groupedDF, User, Track.date, Track.sections.transportmode, Track.sections.duration, Track.sections.distance)
        test <- nest(res)
        test <- test %>% select(User, Track.date, Track.sections.transportmode, Track.sections.duration, Track.sections.distance)
        
        test$Track.sections.transportmode <- str_replace_all(test$Track.sections.transportmode, "STATIONARY", "Stillstand") 
        test$Track.sections.transportmode <- str_replace_all(test$Track.sections.transportmode, "NON_VEHICLE", "Fussweg") 
        test$Track.sections.transportmode <- str_replace_all(test$Track.sections.transportmode, "BUS", "Bus")
        test$Track.sections.transportmode <- str_replace_all(test$Track.sections.transportmode, "TRAIN", "Zug")
        test$Track.sections.transportmode <- str_replace_all(test$Track.sections.transportmode, "CAR", "Auto") 
        test$Track.sections.transportmode <- str_replace_all(test$Track.sections.transportmode, "BIKE", "Fahrrad") 
        colnames(test) <- c("User", "Datum", "Verkehrsmittel", "Dauer (min)", "Weg (km)")
        write.xlsx(test, file)
      }else{
        write.xlsx(dataFrame(), file)
      }
    }
  )
  
  
  #################### FREQUENCIES UI ###################################
  output$frequencyUI <- renderUI({
    ns <- session$ns
    usergroups(databaseconnector("AnalyseMC", "Usergroup", "find", paste0('{"X_id" :"', current_user_status$userid, '"}'),"{}"))
    usergroupNames <- as.vector(usergroups()$name)
    fluidPage(
      fluidRow(
        column(
          width = 12,
          infoBoxOutput(ns("carBoxFreq")),
          infoBoxOutput(ns("busBoxFreq")),
          infoBoxOutput(ns("trainBoxFreq")),
          infoBoxOutput(ns("bikeBoxFreq")),
          infoBoxOutput(ns("footBoxFreq")),
          infoBoxOutput(ns("stationaryBoxFreq"))
        )
      ),
      fluidRow(
        p("Die zu analysierende Stelle in der Region auf der Karte anklicken und den gewünschten Radius des Kreises einstellen. Die Verkehrsmittel, welche innerhalb des gewählten Kreises erkannt wurden, werden nach dem Klicken auf 'Ergebnisse laden' darunter aufgelistet."),
        column(
          width = 4,
          numericInput(ns("radius"), "Radius (in m)", 30, min = 5, max = 100000, step = 5),
          checkboxInput(ns("showUsergroupSelectorFreq"), "Nur ausgewählte Usergruppen anzeigen", FALSE),
          shinyjs::hidden(
            wellPanel(
              id= ns("checkboxFreqPanel"),
              class="filterpanel",
              checkboxGroupInput(ns("checkboxFreq"),"Usergruppen", choices = usergroupNames, selected = usergroupNames)
            )
          ),
          checkboxInput(ns("showDateSelectorFreq"), "Zeitraum angeben", FALSE),
          shinyjs::hidden(
            wellPanel(
              id= ns("datesFreqPanel"),
              class="filterpanel",
              dateRangeInput(ns("datesFreq"), label = "Zeitraum definieren", start = "2018-04-01")
            )
          ),
          checkboxInput(ns("showWeekdaySelectorFreq"), "Nur bestimmte Wochentage auswerten", FALSE),
          shinyjs::hidden(
            wellPanel(
              id= ns("checkWeekdayGroupFreqPanel"),
              class="filterpanel",
              checkboxGroupInput(ns("checkWeekdayGroupFreq"), label = "Wochentage auswählen", 
                                 choices = list("Montag" = "Mon", "Dienstag" = "Tue", "Mittwoch" = "Wed", "Donnerstag" = "Thu", "Freitag" = "Fri", "Samstag"="Sat", "Sonntag"="Sun"),
                                 selected = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
            )
          ),
          checkboxInput(ns("showTransportmodeSelectorFreq"), "Nur ausgewählte Verkehrsmittel anzeigen", FALSE),
          shinyjs::hidden(
            wellPanel(
              id= ns("checkTransportmodeGroupFreqPanel"),
              class="filterpanel",
              checkboxGroupInput(ns("checkTransportGroupFreq"), label = "Verkehrsmittel", 
                             choices = list("Auto" = "CAR", "Fahrrad" = "BIKE", "Fussgänger" = "NON_VEHICLE", "Bus" = "BUS", "Zug" = "TRAIN", "Stillstand"="STATIONARY"),
                             selected = c("CAR", "BIKE", "NON_VEHICLE", "BUS", "TRAIN", "STATIONARY"))
            )
          ),
          checkboxInput(ns("showReasonSelectorFreq"), "Nur  Wege, die zu einen bestimmten Zweck zurückgelegt wurden", FALSE),
          shinyjs::hidden(
            wellPanel(
              id= ns("checkReasonFreqPanel"),
              class="filterpanel",
              checkboxGroupInput(ns("checkReasonGroupFreq"), label = "Wegezweck", 
                                 choices = list("Arbeitsplatz" = "workplace", "Dientstlich/Geschäftlich" = "business", "Schule/Ausbildung" = "education", "Bringen/Holen/Begleiten von Personen" = "bringing accompanying persons", "Einkauf" = "shopping", "Private Erledigung" = "private execution", "Privater Besuch" = "private visit", "Freizeit" = "leisure", "Anderer Zweck" = "other purpose"),
                                 selected = c("workplace", "business", "education", "bringing accompanying persons", "shopping", "private execution", "private visit", "leisure", "other purpose"))
            )
          ),
          actionButton(ns("doFilter"), "Ergebnisse laden")
        ),
        column(
          width=8,
          actionButton(ns("showall"),"Alle Wege anzeigen"),
          actionButton(ns("resetMap"),"Karte zurücksetzen"),
          br(),br(),
          leafletOutput(ns("frequencyMap"))
        )
      ),
      br(),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 8,
          dataTableOutput(ns("frequenciesTable")),
          downloadButton(ns("downloadTrackDataFreq"), "Download")
        )
      )
    )
  })
  
  #################### FILTER RESULT FOR FREQUENCIES #######################
  observeEvent(input$doFilter, {
    if(!is.null(currentPoint$lat) && !is.null(currentPoint$lng) && !is.null(radius())){
      startdate <- input$datesFreq[1]
      enddate <- input$datesFreq[2]
      if(enddate < startdate){
        shinyjs::alert("Fehler: Enddatum muss nach dem angegebenen Startdatum liegen")
      }else{
        withProgress(message = 'Daten', value = 0, {
          
          incProgress(0.1, detail = paste("...laden"))
          # use the same dataframe if coordinates and radius are the same as before
          if(!is.null(oldCurrentPoint$lat) && !is.null(oldCurrentPoint$lng) && radius() == oldRadius() && currentPoint$lng == oldCurrentPoint$lng && currentPoint$lat == oldCurrentPoint$lat){
            result <- frequencies()
            colnames(result) <- c("User", "Track.date", "Track.sections.transportmode", "Track.reason")
          }else{
              S <- SpatialPoints(cbind(currentPoint$lng,currentPoint$lat))
              proj4string(S) <- CRS('+proj=longlat +datum=WGS84')
              circ<-circles(S,d=radius())
              poly <- circ@polygons
              data <- unnest(current_user_status$trackData)
              grp <- group_by(data, Track.sections.transportmode, Track.sections.duration, Track.sections.distance)
              newData <- nest(grp)
              
              incProgress(0.5, detail = paste("...selektieren"))
              sel <- apply(newData,1,function(row){
                data <- row$data
                currentNew <-as.data.frame(data)
                rowCoordinates1 <- currentNew %>% select(Track.sections.coords)
                
                coordinatesGeoJson <- rowCoordinates1[[1]]
                if(length(coordinatesGeoJson) > 1){
                  coordinatesGeoJson <- coordinatesGeoJson[1]
                }
                res = FROM_GeoJson(url_file_string = coordinatesGeoJson)
                rowCoordinates <- data.frame(longitude = numeric(), latitude=numeric())
                if(res$type == "LineString"){
                  rowCoordinates <- data.frame(longitude=res$coordinates[,1], latitude=res$coordinates[,2])
                  crs <- CRS('+proj=longlat +datum=WGS84')
                  line <- SpatialPoints(rowCoordinates, proj4string=crs)
                  if(!is.na(over(poly, line))){
                    return(TRUE)
                  }else{
                    return(FALSE)
                  }
                }else{
                  # TODO handle Point if not empty
                 return(FALSE)
                }
              })
              
              result <- newData[sel,]
          }
          
          if(nrow(result)==0){
            result <- data.frame(User=character(), Datum=character(), Verkehrsmittel=character(), Wegezweck=character())
          }else{
            result <- unnest(result) %>% select(User, Track.date, Track.sections.transportmode, Track.reason)
            result <- group_by(result, User, Track.date, Track.sections.transportmode, Track.reason)
            result <- summarise(result)
            
            # Nutzergruppenfilter
            if(input$showUsergroupSelectorFreq){
              members <- filter(usergroups(), name %in% input$checkboxFreq) %>%select(members)
              result <- filter(result, User %in% unlist(members))
            }
            # Zeitfilter
            if(input$showDateSelectorFreq){
              result <- filter(result, as.Date(Track.date) >= startdate & as.Date(Track.date) <= enddate)
            }
            
            # Wochentagfilter
            if(input$showWeekdaySelectorFreq){
              result <- filter(result, wday(Track.date, label=TRUE) %in% input$checkWeekdayGroupFreq)
            }
            
            # Verkehrsmittelfilter
            if(input$showTransportmodeSelectorFreq){
              result <- filter(result, Track.sections.transportmode %in% input$checkTransportGroupFreq)
            }
            
            # Wegezweckfilter
            if(input$showReasonSelectorFreq){
              result <- filter(result, Track.reason %in% input$checkReasonGroupFreq)
            }

            result$Track.sections.transportmode <- str_replace_all(result$Track.sections.transportmode, "STATIONARY", "Stillstand")
            result$Track.sections.transportmode <- str_replace_all(result$Track.sections.transportmode, "NON_VEHICLE", "Fussweg")
            result$Track.sections.transportmode <- str_replace_all(result$Track.sections.transportmode, "BUS", "Bus")
            result$Track.sections.transportmode <- str_replace_all(result$Track.sections.transportmode, "TRAIN", "Zug")
            result$Track.sections.transportmode <- str_replace_all(result$Track.sections.transportmode, "CAR", "Auto")
            result$Track.sections.transportmode <- str_replace_all(result$Track.sections.transportmode, "BIKE", "Fahrrad")
            
            result$Track.reason<- str_replace_all(result$Track.reason, "workplace", "Arbeitsplatz") 
            result$Track.reason <- str_replace_all(result$Track.reason, "business", "Dienstlich/Geschäftlich") 
            result$Track.reason <- str_replace_all(result$Track.reason, "education", "Schule/Ausbildung")
            result$Track.reason <- str_replace_all(result$Track.reason, "bringing accompanying persons", "Bringen/Holen/Begleiten von Personen")
            result$Track.reason <- str_replace_all(result$Track.reason, "shopping", "Einkaufen") 
            result$Track.reason <- str_replace_all(result$Track.reason, "private execution", "Private Erledigung")
            result$Track.reason <- str_replace_all(result$Track.reason, "private visit", "Privater Besuch")
            result$Track.reason <- str_replace_all(result$Track.reason, "leisure", "Freizeit")
            result$Track.reason <- str_replace_all(result$Track.reason, "other purpose", "Anderer Zweck")
            
          }
          incProgress(0.8, detail = paste("...anzeigen"))
          colnames(result) <- c("User", "Datum", "Verkehrsmittel", "Wegezweck")
          frequencies(result)
          calculateOverviewFreq()
          addPolylinesToLeafletFreq()
          oldCurrentPoint$lat <- currentPoint$lat
          oldCurrentPoint$lng <- currentPoint$lng
          oldRadius(radius())
        })
      }
    }else{
      shinyjs::alert("Fehler: Es wurde keine Position definiert oder Radius ist ungültig. Der Radius muss zwischen 5m und 100000m liegen.")
    }
    
  })
  
  ############### CALCULATE OVERVIEW - FREQUENCIES #############################
  calculateOverviewFreq <- function(){
    newData <- frequencies()
    numAllSectionsFreq(nrow(newData))
    if(nrow(newData)==0){
      overviewFreq(data.frame(transportmode=character(), number=numeric(), percent=numeric()))
    }else{
      newData <- newData[,"Verkehrsmittel"]
      sums <- as.data.frame(table(newData))
      per <- function(x) 100/numAllSectionsFreq() * x
      result <- cbind(sums[1:2], lapply(sums[2], per) )
      colnames(result) <- c("transportmode", "number", "percent")
      result["percent"] <-  round(result["percent"], 2)
      overviewFreq(result)
    }
  }
  
 
  ##################### INFO BOXES - FREQUENCIES ################################
  output$bikeBoxFreq <- renderInfoBox({
    if(is.null(overviewFreq())){
      infoBox(
        "Fahrrad","", icon = icon("bicycle"),
        color = "olive", fill = TRUE, width = 1
      )
    }else{
      val <- overviewFreq()[which(overviewFreq()$transportmode == "Fahrrad"), ]
      if(nrow(val) == 0){
        infoBox(
          "Fahrrad","", icon = icon("bicycle"),
          color = "olive", fill = TRUE, width = 1
        )
      }else{
        infoBox(
          "Fahrrad", HTML(paste0(val$percent ,"%", br() , val$number, " / ", numAllSectionsFreq())), icon = icon("bicycle"),
          color = "olive", fill = TRUE, width = 1
        )
      }
    }
  })
  output$carBoxFreq <- renderInfoBox({
    if(is.null(overviewFreq())){
      infoBox(
        "Auto","", icon = icon("car"),
        color = "blue", fill = TRUE, width = 1
      )
    }else{
      val <- overviewFreq()[which(overviewFreq()$transportmode == "Auto"), ]
      if(nrow(val) == 0){
        infoBox(
          "Auto","", icon = icon("car"),
          color = "blue", fill = TRUE, width = 1
        )
      }else{
        infoBox(
          "Auto", HTML(paste0(val$percent ,"%", br(), val$number, " / ", numAllSectionsFreq())), icon = icon("car"),
          color = "blue", fill = TRUE, width = 1
        )
      }
    }
  })
  output$stationaryBoxFreq <- renderInfoBox({
    if(is.null(overviewFreq())){
      infoBox(
        "Stillstand","", icon = icon("map-marker"),
        color="lime", fill=TRUE, width = 1
      )
    }else{
      val <- overviewFreq()[which(overviewFreq()$transportmode == "Stillstand"), ]
      if(nrow(val) == 0){
        infoBox(
          "Stillstand","", icon = icon("map-marker"),
          color="lime", fill=TRUE, width = 1
        )
      }else{
        infoBox(
          "Stillstand", HTML(paste0(val$percent ,"%", br(), val$number, " / ", numAllSectionsFreq())), icon = icon("map-marker"),
          color = "lime", fill=TRUE, width = 1
        )
      }
    }
  })
  output$footBoxFreq <- renderInfoBox({
    if(is.null(overviewFreq())){
      infoBox(
        "Fussweg","", icon = icon("male"),
        color = "green", fill = TRUE, width = 1
      )
    }else{
      val <- overviewFreq()[which(overviewFreq()$transportmode == "Fussweg"), ]
      if(nrow(val) == 0){
        infoBox(
          "Fussweg","", icon = icon("male"),
          color = "green", fill = TRUE, width = 1
        )
      }else{
        infoBox(
          "Fussweg", HTML(paste0(val$percent ,"%", br(), val$number, " / ", numAllSectionsFreq())), icon = icon("male"),
          color = "green", fill = TRUE, width = 1
        )
      }
    }
  })
  output$trainBoxFreq <- renderInfoBox({
    if(is.null(overviewFreq())){
      infoBox(
        "Zug","", icon = icon("train"),
        color = "aqua", fill = TRUE, width = 1
      )
    }else{
      val <- overviewFreq()[which(overviewFreq()$transportmode == "Zug"), ]
      if(nrow(val) == 0){
        infoBox(
          "Zug","", icon = icon("train"),
          color = "aqua", fill = TRUE, width = 1
        )
      }else{
        infoBox(
          "Zug", HTML(paste0(val$percent ,"%", br(), val$number, " / ", numAllSectionsFreq())), icon = icon("train"),
          color = "aqua", fill = TRUE, width = 1
        )
      }
    }
  })
  
  output$busBoxFreq <- renderInfoBox({
    if(is.null(overviewFreq())){
      infoBox(
        "Bus","", icon = icon("bus"),
        color = "light-blue", fill = TRUE, width = 1
      )
    }else{
      val <- overviewFreq()[which(overviewFreq()$transportmode == "Bus"), ]
      if(nrow(val) == 0){
        infoBox(
          "Bus","", icon = icon("bus"),
          color = "light-blue", fill = TRUE, width = 1
        )
      }else{
        infoBox(
          "Bus",HTML(paste0(val$percent ,"%",br(), val$number, " / ", numAllSectionsFreq())), icon = icon("bus"),
          color = "light-blue", fill = TRUE, width = 1
        )
      }
    }
  })
  
  
  ################## OBSERVER FOR FILTER SELECTION  - FREQUENCIES ############
  observeEvent(input$showReasonSelectorFreq, {
    if(input$showReasonSelectorFreq){
      shinyjs::showElement("checkReasonFreqPanel")
    }else{
      shinyjs::hideElement("checkReasonFreqPanel")
    }
  })
  
  observeEvent(input$showWeekdaySelectorFreq, {
    if(input$showWeekdaySelectorFreq){
      shinyjs::showElement("checkWeekdayGroupFreqPanel")
    }else{
      shinyjs::hideElement("checkWeekdayGroupFreqPanel")
    }
  })
  
  observeEvent(input$showUsergroupSelectorFreq, {
    if(input$showUsergroupSelectorFreq){
      shinyjs::showElement("checkboxFreqPanel")
    }else{
      shinyjs::hideElement("checkboxFreqPanel")
    }
  })
  
  observeEvent(input$showDateSelectorFreq, {
    if(input$showDateSelectorFreq){
      shinyjs::showElement("datesFreqPanel")
    }else{
      shinyjs::hideElement("datesFreqPanel")
    }
  })
  
  observeEvent(input$showTransportmodeSelectorFreq, {
    if(input$showTransportmodeSelectorFreq){
      shinyjs::showElement("checkTransportmodeGroupFreqPanel")
    }else{
      shinyjs::hideElement("checkTransportmodeGroupFreqPanel")
    }
  })
  
  ######################## RENDER LEAFLET MAP - FREQUENCIES #####################
  output$frequencyMap <- renderLeaflet({
    ns <- session$ns
    if("admin" %in% current_user_status$access){
      leaflet() %>% setView(lng = 9.8902804, lat = 47.2807386, zoom = 9)  %>% addTiles()
    }else{
      x <- as(extent(-180, 180, -90, 90), "SpatialPolygons")
      bbox <- bbox(current_user_status$regionpolygon)
      leaflet() %>% addPolygons(data = symdif(x,current_user_status$regionpolygon),
                                color = "black",
                                stroke = T,
                                layerId = "myRegionLayer") %>% addTiles(options = providerTileOptions(minZoom = 4)) %>% fitBounds(bbox[1,1], bbox[2,1], bbox[1,2], bbox[2,2]) 
    }
  })
  
  ############## ADD POLYLINES TO LEAFLET - FREQUENCIES #############################
  dataForMap <- reactiveVal(data.frame(User=character(), Track.date=character()))
  addPolylinesToLeafletFreq <- function(){
    fullData <- current_user_status$trackData
    dataSet <- frequencies() 
    colnames(dataSet) <- c("User", "Track.date", "Track.sections.transportmode", "Track.reason")
    dataSet <- dataSet %>% select("User", "Track.date")
    if(!empty(dataSet)){
      #select relevant tracks from fullData
      newData <- dataForMap(left_join(dataSet, fullData, by=c("User", "Track.date")))
      leafletProxy("frequencyMap") %>% clearShapes()
      
      #print polylines on frequencyMap
      if(!empty(dataForMap())){
        points <- dataForMap() %>% select(data)
        
        by(points, 1:nrow(points), function(row) {
          current <- row$data[[1]]
          currentNew <-as.data.frame(current)
          rowCoordinates <- currentNew %>% select(Track.sections.coords)
          
          # rowCoordinates ist ein Data frame mit allen sections eines tracks. es ist eine schleife notwendig
          by(rowCoordinates, 1:nrow(rowCoordinates), function(section) {
            #print(str(section))
            sectionCoordinates <- section$Track.sections.coords
            res = FROM_GeoJson(url_file_string = sectionCoordinates)
            if(res$type == "LineString"){
              leafletProxy("frequencyMap") %>%
                addPolylines(lng=res$coordinates[,1], lat=res$coordinates[,2], color = "#0000ff",fillOpacity = 0,
                             weight = 2)
            }
          })
        })
      }
    }
    
    #define zoom
    zoom <- 12
    if(radius() <= 50){
      zoom <- 17
    }else if(radius() <= 100){
      zoom <- 15
    }else if(radius() <= 1500){
      zoom <- 12
    }else if(radius() > 1500){
      zoom <- 11
    }
    
    leafletProxy("frequencyMap") %>%
      addCircles(lat = currentPoint$lat,lng = currentPoint$lng,radius = radius(),
                 layerId = "marker") %>% setView(lng = currentPoint$lng, lat = currentPoint$lat, zoom = zoom)
    
    #add region shape to map (if "user")
    if("user" %in% current_user_status$access){
      x <- as(extent(-180, 180, -90, 90), "SpatialPolygons")
      bbox <- bbox(current_user_status$regionpolygon)
      leafletProxy("frequencyMap") %>% addPolygons(data = symdif(x,current_user_status$regionpolygon),
                                                   color = "black",
                                                   fillOpacity = 0.2,
                                                   stroke = T,
                                                   layerId = "myRegionLayer") %>% addTiles(options = providerTileOptions(minZoom = 4)) %>% fitBounds(bbox[1,1], bbox[2,1], bbox[1,2], bbox[2,2])
    }
  }
  
  ################## SHOW ALL TRACKS ON FREQUENCIES MAP ###################
  observeEvent(input$showall, {
    dataFrame(current_user_status$trackData)
    leafletProxy("frequencyMap") %>% clearShapes()
    if(!empty(dataFrame())){
      points <- dataFrame() %>% select(data)
      by(points, 1:nrow(points), function(row) {
        current <- row[[1]]
        currentNew <-as.data.frame(current)
        
        rowCoordinates <- currentNew %>% select(Track.sections.coords)
        # rowCoordinates ist ein Data frame mit allen Sections eines Tracks
        by(rowCoordinates, 1:nrow(rowCoordinates), function(section) {
          sectionCoordinates <- section$Track.sections.coords
          res = FROM_GeoJson(url_file_string = sectionCoordinates)
          if(res$type == "LineString"){
            leafletProxy("frequencyMap") %>%
              addPolylines(lng=res$coordinates[,1], lat=res$coordinates[,2], color = "#000000", fillOpacity = 0,weight = 2)
          }
        })
      })
    }
    
    if("user" %in% current_user_status$access){
      x <- as(extent(-180, 180, -90, 90), "SpatialPolygons")
      bbox <- bbox(current_user_status$regionpolygon)
      leafletProxy("frequencyMap") %>% addPolygons(data = symdif(x,current_user_status$regionpolygon),
                                                     color = "black",
                                                     fillOpacity = 0.2,
                                                     stroke = T,
                                                     layerId = "myRegionLayer") %>% addTiles(options = providerTileOptions(minZoom = 4)) %>% fitBounds(bbox[1,1], bbox[2,1], bbox[1,2], bbox[2,2])
    }
  })
  
  ########################### RADIUS OBSERVER - FREQUENCIES #####################
  observeEvent(input$radius,{
    radius(input$radius)
    if(!is.null(currentPoint$lat) && !is.null(currentPoint$lng)){
      leafletProxy("frequencyMap") %>%
        addCircles(lat = currentPoint$lat,lng = currentPoint$lng,radius = radius(),
                   layerId = "marker")
    }
  })
  
  ######################### MAP CLICK OBSERVER - FREQUENCIES ###################
  observeEvent(input$frequencyMap_click, {
    click <- input$frequencyMap_click
    
    #pulls lat and lon from shiny click event
    currentPoint$lat <- click$lat
    currentPoint$lng <- click$lng
    
    leafletProxy("frequencyMap", data = line) %>%
      addCircles(lat = click$lat,lng = click$lng,radius = radius(),
                 layerId = "marker")
  })
  
  ################## RESET MAP - FREQUENCIES #############################
  observeEvent(input$resetMap, {
    leafletProxy("frequencyMap") %>% clearShapes()
    
    ns <- session$ns
    if("admin" %in% current_user_status$access){
      leafletProxy("frequencyMap") %>% setView(lng = 9.8902804, lat = 47.2807386, zoom = 9)  %>% addTiles()
    }else{
      x <- as(extent(-180, 180, -90, 90), "SpatialPolygons")
      bbox <- bbox(current_user_status$regionpolygon)
      leafletProxy("frequencyMap") %>% addPolygons(data = symdif(x,current_user_status$regionpolygon),
                                                   color = "black",
                                                   stroke = T,
                                                   layerId = "myRegionLayer") %>% addTiles(options = providerTileOptions(minZoom = 4)) %>% fitBounds(bbox[1,1], bbox[2,1], bbox[1,2], bbox[2,2]) 
    }
  })
  
  
  ##################### RESULT DATA TABLE - FREQUENCIES #########################
  
  output$frequenciesTable <-  DT::renderDataTable(
    datatable(frequencies(), selection= "single", options = list(scrollX= T)) %>%
      formatDate("Datum", "toLocaleString")
  )
   
  # get selected track and call function to draw it on map
  observeEvent(input$frequenciesTable_rows_selected, {
    if(length(input$frequenciesTable_rows_selected) > 0){
      currentTrack(frequencies()[input$frequenciesTable_rows_selected,])
      updateMapViewFreq()
      # shinyjs::show("trackDetailsPanel")
    }else{
      currentTrack(NULL)
      leafletProxy("frequencyMap") %>% clearGroup("currentTrackLayer")
      # shinyjs::hide("trackDetailsPanel")
    }
  }, ignoreNULL = FALSE )
  
  # draw selected track on leaflet map
  updateMapViewFreq <- function(){
    leafletProxy("frequencyMap") %>% clearGroup("currentTrackLayer")
    fullData <- current_user_status$trackData
    dataSet <- currentTrack()
    colnames(dataSet) <- c("User", "Track.date", "Verkehrsmittel", "Wegezweck")
    dataSet <- dataSet %>% select("User", "Track.date")
    dataForMap <- left_join(dataSet, fullData, by=c("User", "Track.date"))

    current <- dataForMap$data[[1]]
    currentNew <-as.data.frame(current)
    
    rowCoordinates <- currentNew %>% select(Track.sections.coords)
    
    # rowCoordinates ist ein Data frame mit allen sections eines tracks. es ist eine schleife notwendig
    by(rowCoordinates, 1:nrow(rowCoordinates), function(section) {
      sectionCoordinates <- section$Track.sections.coords
      res = FROM_GeoJson(url_file_string = sectionCoordinates)
      if(res$type == "LineString"){
        leafletProxy("frequencyMap") %>%
          addPolylines(lng=res$coordinates[,1], lat=res$coordinates[,2], color = "red", stroke = T, group = "currentTrackLayer")
      }
    })
  }
  
  
  
  
  #################### DOWNLOAD FREQUENCIES RESULT ####################
  output$downloadTrackDataFreq <- downloadHandler(
    filename = function() {
      "frequenciesResult.xlsx"
    },
    content = function(file) {
      write.xlsx(frequencies(), file)
    }
  )
  
  
}
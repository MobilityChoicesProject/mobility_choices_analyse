##################### MODULE UI FUNCTION ##########################
profileUI<- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  mainPanel(id="mainProfile",width = 12,
            fluidRow( 
                              box(
                                width=12,
                                title="Informationen zum Profil",
                                collapsible = TRUE,
                                collapsed = TRUE,
                                tagList(
                                  strong("Hier sehen Sie Informationen zu Ihrem Profil"),
                                  p("Profilinformationen wie Name, Emailadresse und Beschreibung, sowie die zugelassene Region zur Auswertung von Daten werden auf diesem Screen angezeigt."),
                                  p("Zusätzlich gibt es hier die Möglichkeit, das eigene Passwort zu ändern.")
                              ))),
            fluidRow(
              box(
                width = 6,
                title = "Profil-Informationen",
                strong("Name"),
                textOutput(ns('name')),
                br(),
                strong("Email"),
                textOutput(ns('email')),
                br(),
                strong("Beschreibung"),
                textOutput(ns('description')),
                br()
              ),  
              box(
                width=4,
                title="Passwort ändern",
                collapsible = TRUE,
                collapsed = TRUE,
                textInput(ns('old'), "Altes Passwort"),
                br(),
                textInput(ns('new'), "Neues Passwort"),
                br(),
                textInput(ns('new2'), "Neues Passwort wiederholen"),
                br(),
                actionButton(ns("changePassword"), "Speichern")
              ),
              box(
                width = 12,
                title = "Region",
                textOutput(ns("regionlabel")),
                br(),
                leafletOutput(ns("currentUserRegionMap"), height = 300)
              )
          )
  )
}

###################### MODULE SERVER FUNCTION ##########################
profile <- function(input, output, session, current_user_status, myusers) {
  ################## TEXT OUTPUT FIELDS ################################
  output$name <- renderText(current_user_status$name)
  output$email <- renderText(current_user_status$email)
  output$description <- renderText(current_user_status$description)
  output$regionlabel <- renderText(
    if("admin" %in% current_user_status$access){
      return("Keine Begrenzung")
    }else{
      return(current_user_status$regionlabel) 
    }
  )
  
  ################### RENDER LEAFLET MAP (REGION) ######################
  output$currentUserRegionMap <- renderLeaflet({
    if("admin" %in% current_user_status$access){
      leaflet() %>% addTiles() %>% setView(lng = 9.8902804, lat = 47.2807386, zoom = 8)
    }else{
      leaflet() %>% addPolygons(data = current_user_status$regionpolygon, 
                                fillColor = "green",
                                fillOpacity = 0.5, 
                                color = "black",
                                weight = 3, 
                                stroke = T,
                                layerId = "myRegionLayer")  %>% addTiles()
    }
  })
  ##################### CHANGE PASSWORD OBSERVER ########################
  observeEvent(input$changePassword, {
    old <- isolate(input$old)
    new <- isolate(input$new)
    new2 <- isolate(input$new2)

    if(checkpw(old,myusers$password[myusers$username == current_user_status$current_user])){
      if(new == new2){
        hashedPassword <- hashpw(new)

        query <- paste0('{"username" : "', current_user_status$current_user, '"}')
        fieldsVal <- paste0('{"$set":{"password": "', hashedPassword, '" }}')
        databaseconnector("AnalyseMC", "AnalyseUser", "update", query, fieldsVal)

        shinyjs::alert("Passwort wurde erfolgreich geändert.")
        shinyjs::reset("old")
        shinyjs::reset("new")
        shinyjs::reset("new2")
      }else{
        shinyjs::alert("Fehler: Die beiden Eingaben für das neue Passwort stimmen nicht überein.")
      }
    }else{
      shinyjs::alert("Fehler: Das alte Passwort ist nicht korrekt")
    }
  })
}

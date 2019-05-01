############# MODULE UI FUNCTION ######################
usergroupUI<- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  mainPanel(id="mainUsergroup", width = 12,
    fluidRow( column( width=12,
                      box(
                        width=12,
                        title="Informationen zu den Usergruppen",
                        collapsible = TRUE,
                        collapsed = TRUE,
                        tagList(
                          strong("Hier werden die von Ihnen erstellten Usergruppen angezeigt und verwaltet."),
                          p("Eine neue Usergruppe kann erstellt werden, indem die Box 'Neue Usergruppe erstellen' aufgeklappt wird und Namen und Bescrehibung für die zu erstellende Gruppe eingegeben werden. Da Die App-Benutzer alle Usergruppen sehen können, müssen die Namen systemweit eindeutig sein. Sollte also ein anderer Nutzer dieser Webanwendung bereits eine Usergruppe mit demselben Namen erstellt haben, so müssen Sie einen anderen vergeben."),
                          p("Die Detailansicht einer Usergruppe zeigt den Namen, die Beschreibung, den Benutzernamen des Erstellers, eine Liste deren Mitglieder und wieviele Wege diese bereits zur Analyse freigegeben haben."),
                          p("Nach der Erstellung der Usergruppe können Mobility-Choices App-Benutzer eigenständig den jeweiligen Gruppen beitreten. Die Usergruppen können vom Ersteller bearbeitet werden: d.h. Name und Beschreibung können angepast werden, unerwünschte Mitglieder können entfernt werden oder die ganze Gruppe kann gelöscht werden."),
                          p("Zusätzlich bietet sich noch die Möglichkeit, Push-Nachrichten an alle Mitglieder dieser Gruppe zu senden. Titel und Nachrichtentext müssen jeweils mindestens 4 Zeichen haben und es muss mindestens eine Person in der Gruppe sein")
                        )
    ))),
    fluidRow(
        column( width=6,
            uiOutput(ns("allUsergroupsUI"))
        ),
        uiOutput(ns("usergroupDetails"))
    ),
    fluidRow(
      box(id = "newUsergroup", collapsible = TRUE, collapsed = TRUE, width = 12, title = "Neue Usergruppe erstellen",
          textInput(ns("name"), "Name*"),
          textAreaInput(ns("comment"), "Beschreibung*"),
          br(),
          actionButton(ns("createNewUsergroupBtn"), "Neue Usergruppe erstellen")
      )
    )
  )
}

############### MODULE SERVER FUNCTION #############################
usergroup <- function(input, output, session, current_user_status) {
  # load profiles from Mobility Choices App
  appuser <- databaseconnector("MobilityChoices","Profile", "find", '{}', '{"profile":true, "email":true, "city":true, "country":true, "age":true, "bike":true, "ebike":true, "train":true, "bus":true, "car":true, "ecar":true, "registerDate":true}')
  colnames(appuser) <- c("id", "email", "country", "profile", "bike", "ebike", "train", "bus", "car", "ecar", "city", "age", "registerDate")
  
  ################## DEFINE VARIABLES #########################
  reloadList <- reactiveVal()
  reloadList(TRUE)
  editGroup <- reactiveVal()
  editGroup(FALSE)
  sendMessage <- reactiveVal()
  sendMessage(FALSE)
  usergroups <- reactiveVal()
  allUsergroups <- reactiveVal()
  selectedUsergroup <- reactiveVal()
  sumTracks <- reactiveVal()
  currentMembers <- reactiveVal()
  originalMembers <- reactiveVal()
  userToRemove <- reactiveVal()
  reloadUsergroup <- reactiveVal()
  reloadUsergroup(FALSE)
  
  currentOwner <- reactiveVal()
  currentOwner(NULL)

  ############## LOAD  & SHOW USERGROUPS #########################
  newusergroupsAdmin <- reactive({
    if(reloadList()){
      # load usergroups from database
      allUsergroups(databaseconnector("AnalyseMC", "Usergroup", "find", "{}", '{"name":true}'))
      usergroups(databaseconnector("AnalyseMC", "Usergroup", "find", "{}","{}"))
      if(empty(usergroups())){
        return(data.frame(Name=character(), Comment=character(), Mitglieder=integer()))
      }else{
        # create data frame for visualisation
        keeps <- c("name", "comment", "members", "X_id")
        newusergroups <- usergroups()[keeps]
        newusergroups$members<-(lengths(newusergroups$members))
        colnames(newusergroups) <- c("Name", "Beschreibung", "User", "Erstellt von")
        return(newusergroups)
      }
    }
  })
  newusergroups <- reactive({
    if(reloadList()){
      # load usergroups from database
      allUsergroups(databaseconnector("AnalyseMC", "Usergroup", "find", "{}", '{"name":true}'))
      usergroups(databaseconnector("AnalyseMC", "Usergroup", "find", paste0('{"X_id" :"', current_user_status$userid, '"}'),"{}"))
      if(empty(usergroups())){
        return(data.frame(Name=character(), Comment=character(), Mitglieder=integer()))
      }else{
        # create data frame for visualisation
        keeps <- c("name", "comment", "members")
        newusergroups <- usergroups()[keeps]
        newusergroups$members<-(lengths(newusergroups$members))
        colnames(newusergroups) <- c("Name", "Beschreibung", "User")
        return(newusergroups)
      }
    }
  })
  
  # render data tables
  output$myUsergroupsDT <- DT::renderDataTable(newusergroups(), selection= "single", options = list(scrollX= T))
  output$myUsergroupsDTadmin <- DT::renderDataTable(newusergroupsAdmin() %>% select("Name", "Beschreibung", "User"), selection= "single", options = list(scrollX= T))
  
  # observe selected rows
  observeEvent(input$myUsergroupsDT_rows_selected, {
    selectedUsergroup(usergroups()[input$myUsergroupsDT_rows_selected,])
    loadMembers()
  })
  
  observeEvent(input$myUsergroupsDTadmin_rows_selected, {
    selectedUsergroup(usergroups()[input$myUsergroupsDTadmin_rows_selected,])
    loadMembers()
  })
  
  #################### LOAD MEMBERS OF USERGROUP ########################################
  loadMembers <- function(){
    if(reloadUsergroup()){
      usergroups(databaseconnector("AnalyseMC", "Usergroup", "find", paste0('{"X_id" :"', current_user_status$userid, '"}'),"{}"))
      indices <- which(usergroups()$name == selectedUsergroup()$name)
      new <- usergroups()[indices,]
      selectedUsergroup(new[1,])
      reloadUsergroup(FALSE)
    }
    
    #load members
    members <- selectedUsergroup()$members
    membersDF <- data.frame(members)
    
    if(nrow(membersDF) == 0){
      sumTracks(0)
      currentMembers(data.frame(User=character(), Wege=numeric()))
    }else{
      
    colnames(membersDF) <- c("profile")
    total <- merge(appuser,membersDF,by="profile", all.y = TRUE)
    #load number of tracks
    colnames(membersDF) <- c("User")
    tracks <- filter(current_user_status$trackData, User %in% membersDF$User) %>% select(User)
    tracksDF <- as.data.frame(tracks)
    new <- count(tracksDF, User)
    colnames(new) <- c("profile", "n")
    newTotal <- merge(x = total, y = new, by="profile", all.x = TRUE)
    originalMembers(newTotal)
    result <- newTotal %>% select(email, city, n)
    colnames(result) <- c("User", "Heimatort", "Wege")

    #set reactive values
    sumTracks(nrow(tracksDF))
    currentMembers(result)
    }
   
  }
  
  observeEvent(input$detailsMemberlistEditmode_rows_selected, {
    userToRemove(originalMembers()[input$detailsMemberlistEditmode_rows_selected,]$profile)
    ns <- session$ns
    showModal(modalDialog(
      title = "User wirklich entfernen?",
      "Diese Aktion kann nur vom User selbst rückgängig gemacht werden",
      easyClose = TRUE,
      footer =  tagList(
        modalButton("Abbrechen"),
        actionButton(ns("removeUser"), "Ja, Entfernen", style="color: #ff0000")
      )
    ))
  })
  
  ################## ALL USERGROUPS UI ############################
  output$allUsergroupsUI <- renderUI({
    ns <- session$ns
    if("admin" %in% current_user_status$access){
      tagList(
        box(id="allUsergroups",
            collapsible = TRUE,
            width=12,
            title= "Alle Usergruppen",
            DT::dataTableOutput(ns("myUsergroupsDTadmin"))
        )
      )
    }else if("user" %in% current_user_status$access){
      tagList(
        box(id="allUsergroups",
            collapsible = TRUE,
            width=12,
            title= "Meine Usergruppen",
            DT::dataTableOutput(ns("myUsergroupsDT"))
        )
      )
    }
  })
  
  #################### SELECTED USERGROUP - DETAILS UI ##################
  output$usergroupDetails <- renderUI({
    ns <- session$ns
    if(is.null(selectedUsergroup())){
      tagList(
        strong("Zum Anzeigen von Details, bitte eine Usergruppe anklicken")
      )
    }else if(sendMessage()==TRUE){
        tagList(
          column(
            width= 6,
            div(id = "usergroupbox-outer",
                box(id=ns("detailedUsergroup"),
                    title=selectedUsergroup()$name,
                    width=12,
                    fluidRow(
                      column(
                        width = 12,
                        textOutput(ns("infoTextboxForNotifications")),
                        br(),
                        textInput(ns('messageTitleInputBox'), "Titel der Nachricht"),
                        br(),
                        textAreaInput(ns('messageInputBox'), "Text für die Nachricht")
                      )
                    ),
                    br(),
                    fluidRow(
                      column(
                        width = 12,
                        actionButton(ns("saveMessage"), "Nachrichts senden", icon("envelope"), style="color: #009900"),
                        actionButton(ns("cancelMessage"), "Abbrechen", style="color: #ff0000")
                      )
                    )
                )
            ) # end of div
          )
        )
    }else if(editGroup()==FALSE && sendMessage() == FALSE){
      if(is.null(selectedUsergroup()$X_id)){
        currentOwner(current_user_status$current_user)
      }else{
        # get owner of usergroup by id
        user <- databaseconnector("AnalyseMC", "AnalyseUser", "find", paste0('{"_id":{"$oid":"', selectedUsergroup()$X_id, '"}}'),"{}")
        currentOwner(user$username)
      }
      if(selectedUsergroup()$X_id == current_user_status$userid){
        tagList(
          column(
            width= 6,
            div(id = "usergroupbox-outer",
                box(id=ns("detailedUsergroup"),
                    title=selectedUsergroup()$name,
                    width=12,
                    fluidRow(
                      column(
                        width = 8,
                        textOutput(ns("commentD")),
                        br(),
                        textOutput(ns("ownerD"))
                      ),
                      column(
                        width = 4,
                        #infoBox("New Orders", 10 * 2, icon = icon("credit-card"))
                        box( title = "Wege", solidHeader = TRUE, status = "info", width=12,
                             textOutput(ns("numberOfTracksBox"))
                        )
                      )
                    ),
                    fluidRow(
                      column(
                        width = 12,
                        DT::dataTableOutput(ns("detailsMemberlist")) 
                      )
                    ),
                    br(),
                    fluidRow(
                      column(
                        width = 12,
                        actionButton(ns("updateUsergroup"), "Usergruppe bearbeiten", style="color: #009900"),
                        actionButton(ns("sendMessage"), "Nachricht senden",icon("envelope")),
                        actionButton(ns("deleteUsergroup"), "Usergruppe löschen", style="color: #ff0000")
                      )
                    )
                )
            ) # end of div
          )
        )
      }else{
        tagList(
          column(
            width= 6,
            div(id = "usergroupbox-outer",
                box(id=ns("detailedUsergroup"),
                    title=selectedUsergroup()$name,
                    width=12,
                    fluidRow(
                      column(
                        width = 8,
                        textOutput(ns("commentD")),
                        br(),
                        textOutput(ns("ownerD"))
                      ),
                      column(
                        width = 4,
                        #infoBox("New Orders", 10 * 2, icon = icon("credit-card"))
                        box( title = "Wege", solidHeader = TRUE, status = "info", width=12,
                             textOutput(ns("numberOfTracksBox"))
                        )
                      )
                    ),
                    fluidRow(
                      column(
                        width = 12,
                        DT::dataTableOutput(ns("detailsMemberlist")) 
                      )
                    )
                )
            ) # end of div
          )
        )
      }
    }else{
      tagList(
        column(
          width= 6,
          div(id = "usergroupbox-outer",
              box(id=ns("detailedUsergroup"),
                  title="Usergruppe bearbeiten",
                  width=12,
                  fluidRow(
                    column(
                      width = 8,
                      textInput(ns("nameUpdate"), "Name", selectedUsergroup()$name),
                      textInput(ns("commentUpdate"), "Kommentar", selectedUsergroup()$comment)
                    )
                  ),
                  fluidRow(
                    column(
                      width = 12,
                      textOutput(ns("info")),
                      br(),
                      DT::dataTableOutput(ns("detailsMemberlistEditmode")) 
                    )
                  ),
                  br(),
                  fluidRow(
                    column(
                      width = 12,
                      actionButton(ns("saveUsergroup"), "Änderungen speichern"),
                      actionButton(ns("cancelUpdate"), "Abbrechen", style="color: #ff0000")
                    )
                  )
              )
          ) # end of div
        )
      )
    }
  })
  
  ############## ADDITIONAL FUNCTIONS FOR USERGROUP DETAILS ###################
  output$infoTextboxForNotifications <- renderText("Zum Senden einer Push-Nachricht an diese Usergruppe,
                                                   hier einen Titel sowie den Text der Nachricht eingeben")
  output$ownerD <- renderText(paste0("Erstellt von: ", currentOwner()))
  
  observeEvent(input$cancelUpdate, {
    ns <- session$ns
    editGroup(FALSE)
  })
  
  observeEvent(input$updateUsergroup, {
    ns <- session$ns
    editGroup(TRUE)
  })
  
  observeEvent(input$sendMessage, {
    ns <- session$ns
    sendMessage(TRUE)
  })
  
  observeEvent(input$saveMessage, {
    ns <- session$ns
    
    message <- isolate(input$messageInputBox)
    title <- isolate(input$messageTitleInputBox)
    if(nchar(message) <= 3){
      shinyjs::alert("Text der Nachricht ist zu kurz. (Min. 4 Zeichen)")
    }else{
      if(nchar(title) <= 3){
        shinyjs::alert("Titel der Nachricht ist zu kurz. (Min. 4 Zeichen)")
      }else{
        emailAdr <- currentMembers()$User
        if(length(emailAdr) == 0){
          shinyjs::alert("In dieser Gruppe befinden sich keine App-Benutzer")
          sendMessage(FALSE)
        }else{
          # send notification and refresh fields
          sendNotfication(emailAdr, current_user_status, message, title)
          shinyjs::reset("messageInputBox")
          shinyjs::reset("messageTitleInputBox")
          sendMessage(FALSE)
        }
      }
    }
  })
  
  observeEvent(input$cancelMessage, {
    ns <- session$ns
    sendMessage(FALSE)
  })
  
  observeEvent(input$saveUsergroup, {
    ns <- session$ns
    newName <- isolate(input$nameUpdate)
    newComment <- isolate(input$commentUpdate)
    
    if(newName %in% allUsergroups()$name){
     if(newName == selectedUsergroup()$name){
        saveGroupData(newName, newComment)
      }else{
        shinyjs::alert("Fehler: Eine Nutzergruppe mit diesem Namen existiert bereits. Bitte vergeben Sie einen anderen Namen.")
      }
    }else{
      saveGroupData(newName, newComment)
    }
    
  })
  
  saveGroupData <- function(newName, newComment){
    query <- paste0('{"name" : "', selectedUsergroup()$name, '"}')
    fieldsVal <- paste0('{"$set":{"name": "', newName, '", "comment": "', newComment ,'" }}')
    databaseconnector("AnalyseMC", "Usergroup", "update", query, fieldsVal)
    
    reloadList(FALSE)
    reloadList(TRUE)
    selectedUsergroup(NULL)
    
    editGroup(FALSE)
  }
  
  observeEvent(input$deleteUsergroup, {
    ns <- session$ns
    showModal(modalDialog(
      title = "Nutzergruppe wirklich löschen?",
      "Diese Aktion kann nicht rückgängig gemacht werden",
      easyClose = TRUE,
      footer =  tagList(
        modalButton("Abbrechen"),
        actionButton(ns("delete"), "Löschen")
      )
    ))
  })
  
  observeEvent(input$removeUser, {
    new <- setdiff(selectedUsergroup()$members[[1]], userToRemove())

    query <- paste0('{"name" : "', selectedUsergroup()$name, '"}')
    fieldsVal <- paste0('{"$set":{"members": ', toJSON(new),' }}')
    databaseconnector("AnalyseMC", "Usergroup", "update", query, fieldsVal)
    
    reloadUsergroup(TRUE)
    loadMembers()
    
    removeModal()
  })
  
  observeEvent(input$delete, {
    databaseconnector("AnalyseMC", "Usergroup", "delete", paste0('{"name" :"', selectedUsergroup()$name, '"}'), "{}")
    reloadList(FALSE)
    reloadList(TRUE)
    selectedUsergroup(NULL)
    removeModal()
  })
  
   output$info <- renderText("Zum Entfernen von einzelnen Mitglidern, diese durch Klicken auswählen: ")
   output$nameD <- renderText(selectedUsergroup()$name)
   output$commentD <- renderText(selectedUsergroup()$comment)
   output$numberOfTracksBox <- renderText({
      sumTracks()
   })
   
   output$detailsMemberlist <- DT::renderDataTable(
     currentMembers()
    ,options = list(scrollX= T, pageLength = 5)
   )
   
   output$detailsMemberlistEditmode <- DT::renderDataTable(
     currentMembers(), options = list(scrollX= T, pageLength = 5)
   )
  
  #################### CREATE NEW USERGROUP BUTTON OBSERVER ####################
  observeEvent(input$createNewUsergroupBtn, {
    if(input$name != "" && input$comment != ""){
      if(input$name %in% allUsergroups()$name){
        shinyjs::alert("Fehler: Diese Nutzergruppe existiert bereits. Bitte vergeben Sie einen anderen Namen.")
      }else{
        jsonString <- paste('{"name":"', isolate(input$name), '", "comment":"', isolate(input$comment), '", "members" : [], "X_id":"', current_user_status$userid,'"}', sep = "")
        databaseconnector("AnalyseMC","Usergroup", "insert", jsonString, "{}")
        reloadList(FALSE)
        reloadList(TRUE)
        
        # reset fields
        shinyjs::reset("name")
        shinyjs::reset("comment")

      }
     
    }else{
      shinyjs::alert("Die Felder Name und Beschreibung sind erforderlich!")
    }
  })
}
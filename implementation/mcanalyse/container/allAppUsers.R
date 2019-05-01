################ MODULE UI FUNCTION #############################
allAppUsersUI<- function(id) {
  ns <- NS(id)
  mainPanel(id="mainAppUsers", width = 12,
            fluidRow(
              column( width=6,
                      box(id="appUsers",
                          collapsible = TRUE,
                          width=12,
                          title= "Alle App-User",
                          DT::dataTableOutput(ns("allAppUsersDT"))
                      )
              ),
              column(width=6,
                box(id="sendNotification",
                    collapsible = TRUE,
                    width=12,
                    title= "Nachricht an App-Benutzer senden",
                    textOutput(ns("infoTextboxForNotifications")),
                    br(),
                    textInput(ns('messageTitleInputBox'), "Titel der Nachricht"),
                    br(),
                    textAreaInput(ns('messageInputBox'), "Text für die Nachricht"),
                    br(),
                    strong("Ausgewählte App-Benutzer"),
                    br(),
                    br(),
                    DT::dataTableOutput(ns("usersToSendNotofication")),
                    br(),
                    actionButton(ns("sendNotification"), "Nachricht senden")
                )
              ),
              column(width=6,
                box(id="activeUsersCount",
                    collapsible = TRUE,
                    width=12,
                    title= "Aktive App-User",
                    strong("Anzahl"),
                    textOutput(ns('count')),
                    DT::dataTableOutput(ns("activeUsersDT"))
                )
              )
            )
  )
}

################## MODULE SERVER FUNCTION ############################
allAppUsers <- function(input, output, session, current_user_status) {
  #################### ALL APP-USERS BOX ################################
  # load and prepare all app users data frame
  allAppUser <- reactive({
    # load profiles from Mobility Choices App
    appuser <- databaseconnector("MobilityChoices","Profile", "find", '{}', '{"email":true, "registerDate":true}')
    colnames(appuser) <- c("id", "email", "registerDate")
    keeps <- c("email", "registerDate")
    
    if (empty(appuser)) {
      return(data.frame(Email=character(), Registrierung=integer()))
    } else {
      # create data frame for visualisation
      keeps <- c("email", "registerDate")
      allAppUser <- appuser[keeps]
      dates <- allAppUser$registerDate
      splittedDates <- strsplit(dates, 'T')
      extractedDates <- unlist(splittedDates)[2*(1:length(dates))-1]
      formatedDates <- format(as.Date(extractedDates), "%d.%m.%Y")
      allAppUser$registerDate = formatedDates
      colnames(allAppUser) <- c("Email", "Datum der Registrierung")
      return(allAppUser)
    }
  })
  
  output$allAppUsersDT <- renderDataTable({
      DT::datatable(allAppUser(), selection= "multiple", options = list(scrollX= T))
  })
  
  ################## PUSH NOTIFICATION BOX ########################################  
      output$infoTextboxForNotifications <- renderText("Zum Senden einer Push-Nachricht an bestimmte App-Benutzer hier den Text eingeben,
                                                       anschließend die gewünschten Personen aus der Liste aller App-Benutzer (links) auswählen
                                                       und die Nachricht senden")
      
      # select app users to send message to
      selectedForMessage <- reactiveVal()
      selectedForMessage(data.frame(Email=character()))
      output$usersToSendNotofication <- renderDataTable({
        DT::datatable(selectedForMessage(), selection= "none",  options = list(scrollX= T))
      })
      
      observeEvent(input$allAppUsersDT_rows_selected, {
          dt <- allAppUser()[input$allAppUsersDT_rows_selected,]
          dt <- dt %>% dplyr::select("Email")
          selectedForMessage(dt)
      }, ignoreNULL = FALSE )
      
      # send notification
      observeEvent(input$sendNotification, {
        message <- isolate(input$messageInputBox)
        title <- isolate(input$messageTitleInputBox)
        if(nchar(message) <= 3){
          shinyjs::alert("Text der Nachricht ist zu kurz. (Min. 4 Zeichen)")
        }else{
          if(nchar(title) <= 3){
            shinyjs::alert("Titel der Nachricht ist zu kurz. (Min. 4 Zeichen)")
          }else{
            emailAdr <- selectedForMessage()$Email
            if(length(emailAdr) == 0){
              shinyjs::alert("Es wurden keine App-Benutzer ausgewählt!")
            }else{
              # send notification and refresh fields
              sendNotfication(emailAdr, current_user_status, message, title)
              shinyjs::reset("messageInputBox")
              shinyjs::reset("messageTitleInputBox")
              selectedForMessage(data.frame(Email=character()))
              proxyDT = dataTableProxy('allAppUsersDT')
              proxyDT %>% selectRows(NULL)
            }
          }
        }
      })
  #################### ACTIVE USERS BOX ################################
      usersTrackCountIter <- databaseconnector("MobilityChoices","Track", "aggregate", '[{"$group" : { "_id" : "$mobilityUserId", "count" : {"$sum" : 1}}}, {"$match": { "count": {"$gt": 1}}}]', '{}')
      users <- NULL
      rows = nrow(usersTrackCountIter)
      count <- 0
      i <- 1
      while(i <= rows){
          user <- usersTrackCountIter[i, ]
          latestTrack <- databaseconnector("MobilityChoices","Track", "find", '{}', sprintf('{"mobilityUserId": {"$oid":"%s"}}', user$'_id'), '{"date":-1}', 1)
          firstTrack <- databaseconnector("MobilityChoices","Track", "find", '{}', sprintf('{"mobilityUserId": {"$oid":"%s"}}', user$'_id'), '{"date":1}', 1)
          if (as.Date(latestTrack$date) - as.Date(firstTrack$date) > 6) {
            count = count + 1
            users <- append(users, user$'_id')
          }
          i <- i + 1
      }
      output$count <- renderText(count)
      
      dataframe <- data.frame(Benutzer=users) 

      # show users in data table
      output$activeUsersDT = DT::renderDataTable({
        dataframe
      })
      
      
}
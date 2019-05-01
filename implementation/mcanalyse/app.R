source("./global.R")
##################### MAINPAGE UI #######################################
ui <- dashboardPage(
    dashboardHeader(
      title = "Mobilty Choices",
      tags$li(class = "dropdown",
              tags$li(class = "dropdown", textOutput("logged_user"), style = "padding-top: 15px; padding-bottom: 15px; color: #fff;"),
              tags$li(class = "dropdown", actionLink("login", textOutput("logintext"))))
    ),
    dashboardSidebar(
      sidebarMenuOutput("menu")
    ),
    dashboardBody(
      useShinyjs(),
      fluidPage(
            fluidRow(
              column(6, offset=3,
                 br(),
                 shinyjs::hidden(
                   wellPanel(id = "loginPanel",
                     h1("Login erforderlich!"),
                     br(),
                     textInput("user",
                               "Benutzername",
                               width = "70%"),
                     passwordInput(inputId = 'password',
                                   label = 'Passwort',
                                   width = "70%"),
                     actionButton("button_login", "Login"),
                     br(),br(),
                     strong(textOutput("verification_result"))
                   )
                 ),
                 wellPanel(id="loginRequired", 
                           h1("Login erforderlich!"))
              )
            ),
            tabItems(
              tabItem(tabName = "dashboard",
                  dashboardUI("dashboard")
              ),
              tabItem(tabName = "profile",
                     profileUI("profile")
              ),
              tabItem(tabName = "usergroup",
                      usergroupUI("usergroup")
              ),
              tabItem(tabName = "users",
                     usersUI("users")
              ),
              tabItem(tabName = "allAppUsers",
                      allAppUsersUI("allAppUsers"))
            )
      ) # end fluidpage
    ) # end of body
) # end of dashboardpage

#################### MAINPAGE SERVER #######################################
server <- function(input, output, session) {
  ################ LOAD USERS FROM DB ##############################################
  myusers <- databaseconnector("AnalyseMC","AnalyseUser", "find", "{}", "{}")
  
  ################ RESET VARIABLES ######################################
  # Reset session specific userdata
  current_user_status <- reactiveValues()
  current_user_status$logged <- FALSE
  current_user_status$current_user <- NULL
  current_user_status$access <- NULL
  current_user_status$userid <- NULL
  current_user_status$name <- NULL
  current_user_status$email <- NULL
  current_user_status$description <- NULL
  current_user_status$regionlabel <- NULL
  current_user_status$regionpolygon <- NULL
  current_user_status$trackData <- NULL
  
  showLoginWindow <- reactiveVal(FALSE)
  
  ########################## AUTHENTICATION SYSTEM ###########################################
  observeEvent(input$login, {
    # switch value of showLoginWindow variable
    showLoginWindow(ifelse(showLoginWindow(), FALSE, TRUE))
    
    # delete global user-specific variables 
      current_user_status$logged <- FALSE
      current_user_status$current_user <- NULL
      current_user_status$userid <- NULL
      current_user_status$access <- NULL
      current_user_status$name <- NULL
      current_user_status$email <- NULL
      current_user_status$description <- NULL
      current_user_status$regionlabel <- NULL
      current_user_status$regionpolygon <- NULL
      current_user_status$timestamp <- NULL
      shinyjs::hideElement(id= "tabs")
      shinyjs::hide("mainDashboard")
      shinyjs::hide("mainProfile")
      shinyjs::hide("mainUsers")
      shinyjs::hide("mainUsergroup")
      shinyjs::hide("mainAppUsers")
    
      #show or hide loginPanel, depending on variable showLoginWindow
    if(showLoginWindow() == TRUE){
      shinyjs::showElement(id= "loginPanel")
      shinyjs::hideElement(id = "loginRequired")
    }else{
      shinyjs::hideElement(id= "loginPanel")
      shinyjs::showElement(id = "loginRequired")
    }
  })
  
  performLogin<-function(){
    withProgress(message = 'Login', value = 0, {
      current_user_status$logged <- TRUE
      current_user_status$current_user <- input$user
      current_user_status$access <- myusers$role[myusers$username == current_user_status$current_user]
      
      # load profile data
      incProgress(0.1, detail = paste("Clientdaten laden"))
      id <- databaseconnector("AnalyseMC","AnalyseUser", "find", paste0('{"username" : "', current_user_status$current_user, '"}'), '{"_id": true}')
      userdata <- databaseconnector("AnalyseMC","Profile", "find", paste0('{"X_id" :"', id, '"}'), '{}')
      
      # put data into global variables
      current_user_status$userid <- userdata$X_id
      current_user_status$name <- userdata$name
      current_user_status$email <- userdata$email
      current_user_status$description <- userdata$description
      
      # load region data
      incProgress(0.4, detail = paste("Region laden"))
      regionData <- databaseconnector("AnalyseMC","Region", "find", paste0('{"X_id" :"', id, '"}'), "{}")
      if(empty(regionData)){
        current_user_status$regionlabel <- NULL
        current_user_status$regionpolygon <- NULL
      }else{
        s <- shapefile(regionData$polygon)
        current_user_status$regionlabel <- regionData$label
        current_user_status$regionpolygon <- s
      }
      incProgress(0.6, detail = paste("Verkehrsdaten laden"))
      trackData <- loadTrackData(current_user_status)
      current_user_status$timestamp <- trackData$timestamp
      current_user_status$trackData <- trackData$data
      current_user_status$trackData["Track.date"] <- lapply(current_user_status$trackData["Track.date"], ymd_hms)
      incProgress(0.7, detail = paste("Login erfolgreich"))
    })
  }
  
  observeEvent(input$button_login, {
    if(input$user!="" && input$password!="" && input$user %in% myusers$username && checkpw(input$password,myusers$password[myusers$username == input$user])){
      performLogin()
      shinyjs::hideElement(id= "loginPanel")
      shinyjs::hideElement(id = "loginRequired")
      shinyjs::reset("user")
      shinyjs::reset("password")
    } else {
      current_user_status$logged <- FALSE
      current_user_status$current_user <- NULL
      output$verification_result <- renderText({
        "Login failed"
      })
    }
  })
  
  # show "Login" or "Logout" depending on whether logged out or in, show "Hide Login" if button was clicked, but user's not logged in yet
  output$logintext <- renderText({
    if(current_user_status$logged) return("Logout")
    if(showLoginWindow()) return("Hide Login")
    return("Login")
  })
  
  # show name of logged in user
  output$logged_user <- renderText({
    if( current_user_status$logged) return(current_user_status$current_user)
    return("")
  })
  
  ######################### SIDEBAR MENU ####################################
  output$menu <- renderMenu({
    if(current_user_status$logged == TRUE){
      shinyjs::show("mainDashboard")
      shinyjs::show("mainProfile")
      shinyjs::show("mainUsers")
      shinyjs::show("mainUsergroup")
      shinyjs::show("mainAppUsers")
      if("admin" %in% current_user_status$access){
        sidebarMenu(
          menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
          menuItem("Profil", icon = icon("user"), tabName = "profile"),
          menuItem("Usergruppen", icon = icon("users"), tabName = "usergroup"),
          menuItem("Clients", icon = icon("user-plus"), tabName = "users"),
          menuItem("App-User", icon = icon("user-circle"), tabName = "allAppUsers"),
          br(),
          textOutput("time_info"),
          textOutput("timestamp"),
          actionButton("cacheReload", "Daten neu laden")
        )
      }else if ("user" %in% current_user_status$access){
        sidebarMenu(
          menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
          menuItem("Profil", icon = icon("user"), tabName = "profile"),
          menuItem("Usergruppen", icon = icon("users"), tabName = "usergroup"),
          br(),
          textOutput("time_info"),
          textOutput("timestamp"),
          actionButton("cacheReload", "Daten neu laden")
        )
      }
  }else{
    sidebarMenu()
  }})
  
  output$time_info <- renderText(paste0("Letzte Aktualisierung: \n"))
  output$timestamp <- renderText(paste0(ymd_hms(current_user_status$timestamp)))
  
  ##################### RELOAD CACHE BUTTON OBSERVER ###########################
  observeEvent(input$cacheReload, {
    trackData <- cleanCacheAndReloadData(current_user_status)
    current_user_status$trackData <- trackData$data
    current_user_status$timestamp <- trackData$timestamp
  })
  
  ####################### CALL MODULES ##########################################
  # Dashboard
  dashboard <- callModule(dashboard, "dashboard", current_user_status)
  # Profil
  profile <- callModule(profile, "profile", current_user_status, myusers)
  # Usergruppen
  usergroup <- callModule(usergroup, "usergroup", current_user_status)
  # Clients
  users <- callModule(users, "users", current_user_status, places, land, bund, germany)
  # App-User
  allAppUsers <- callModule(allAppUsers, "allAppUsers", current_user_status)
}

############# RUN THE APPLICATION #########################################
shinyApp(ui = ui, server = server)
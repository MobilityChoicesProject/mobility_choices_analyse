################# SEND NOTIFICATION TO APP USERS ##############################
sendNotfication <- function(emailAdr, current_user_status, message, title){
  
  url <- "SERVER_URL/sendFirebaseMessage"
  
  # BUILD JSON OBJECT
  jsonObjectReq <- list()
  
  if(length(emailAdr) == 1){
    jsonObjectReq$userMails <- list(emailAdr)
  }else{
    jsonObjectReq$userMails <- emailAdr
  }
  jsonObjectReq$verifyToken <- current_user_status$userid
  jsonObjectReq$message <- message
  jsonObjectReq$title <- title
  
  # REST CALL
  test <- POST(url, body = jsonObjectReq, encode = "json")
  
  # HANDLE RESPONSE
  if(status_code(test) == 404){
    shinyjs::alert("Senden der Nachricht fehlgeschlagen! Funktion wird nicht unterstützt")
  }else{
    if(content(test)$error){
      shinyjs::alert(paste0("Senden der Nachricht fehlgeschlagen! Fehler: ",content(test)$errorMessage))
    }else{
      shinyjs::alert("Nachricht wurde erfolgreich gesendet")
    }  
  }
}

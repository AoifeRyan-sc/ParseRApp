# a function to stop the script when one of the variables cannot be found. and to strip quotation marks from the secrets when you supplied them. (maybe it is just easier to never use them)
error_on_missing_name <- function(name){
  var <- Sys.getenv(name, unset=NA)
  if(is.na(var)){
    stop(paste0("cannot find ",name, " !"),call. = FALSE)
  }
  gsub("\"", '',var)
}

# Authenticate
rsconnect::setAccountInfo(name   = error_on_missing_name("SHINY_ACC_NAME"),
               token  = error_on_missing_name("TOKEN"),
               secret = error_on_missing_name("SECRET"))

# Deploy the application.
rsconnect::deployApp(appDir = "app/", appName = "ParseR", forceUpdate = TRUE)
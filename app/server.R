server <- function(input, output, session) {
  r <- shiny::reactiveValues()
  
  dataUploadServer("data_upload_panel", r)
  dataDisplayServer("data_display_panel", r)
}
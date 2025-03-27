server <- function(input, output, session) {
  r <- shiny::reactiveValues(
    url_var = NULL
  )
  
  # observeEvent(input$toggle, {
  #   session$sendCustomMessage("toggle-card", "")
  # })
  
  dataUploadServer("data_upload_panel", r)
  dataDisplayServer("data_display_card", r)
  dataBigramServer("bigram_card", r)
}
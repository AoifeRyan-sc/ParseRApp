server <- function(input, output, session) {
  r <- shiny::reactiveValues(
    url_var = NULL
  )
  options(shiny.maxRequestSize=100*1024^2)
  
  # observeEvent(input$toggle, {
  #   session$sendCustomMessage("toggle-card", "")
  # })
  
  dataUploadServer("data_upload_panel", r)
  dataDisplayServer("data_display_card", r)
  bigramVizServer("bigram_viz_card", r)
  bigramDataServer("bigram_data_card", r)
  groupTermsServer("gt_card", r)
  wloServer("wlo_card", r)
}
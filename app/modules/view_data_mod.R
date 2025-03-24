dataDisplayUi <- function(id){
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Uploaded Data"),
    bslib::card_body(
      DT::dataTableOutput(ns("data_display"))
    ),
    full_screen = TRUE,
    style = bslib::css(
      gap ="0.25rem",
      resize = "horizontal"
    )
  )
}

dataDisplayServer <- function(id, r){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$data_display <- DT::renderDataTable({
      r$df
    })
    
    
  })
  
}
dataDisplayUi <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    DT::dataTableOutput(ns("data_display"))  
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
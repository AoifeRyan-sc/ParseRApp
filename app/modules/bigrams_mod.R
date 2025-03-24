dataDisplayUi <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::plotOutput(ns("bigram_viz"))
  )
}

dataDisplayServer <- function(id, r){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(r$text_var, {
      ParseR::count_ngram(r$df, r$text_var, top)
    })
    
    output$bigram_viz <- shiny::renderPlot({
      
    })
    
    
  })
  
}
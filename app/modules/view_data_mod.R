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
      resize = "vertical"
    )
  )
}

dataDisplayServer <- function(id, r){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observe({
      req(r$df)
      if (is.null(r$display_var)){
        r$cols <- colnames(r$df)
      } else {
        r$cols <- r$display_var
      }
    })
    
    output$data_display <- DT::renderDataTable(

      # DT::datatable(
      r$df[r$cols],
      filter = "top",
      # extensions = c("Buttons"),
      # options = list(
      #   select = list(maxOptions = 2000),
      #   dom = 'Bfrtip',
      #   buttons = c("copy", "csv", "excel", "pdf")
      # )
      # )
    )
    
    
  })
  
}
dataDisplayUi <- function(id){
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Uploaded Data"),
    bslib::card_body(
      dropdownButton_with_tooltip(
        select_input_with_tooltip(id = ns("url_column"), title = "URL Column",
                                  icon_info = "The name of the column with the post url"),
        select_input_with_tooltip(id = ns("display_columns"), title = "Display Columns",
                                  icon_info = "The columns you want to display in the Uploaded Data table",
                                  multiple_selections = TRUE),
        # shiny::actionButton(ns("update_table"), "Update Table"),
        dropdown_title = "Modify Table View",
        icon_info = "Click here for table view customisation"
      ),
      DT::dataTableOutput(ns("data_display"))
    ),
    full_screen = TRUE,
    style = bslib::css(
      gap ="0.25rem",
      resize = "vertical"
    ),
    height = "300px"
  )
}

dataDisplayServer <- function(id, r){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    shiny::observe({
      shiny::updateSelectizeInput(session = session, "url_column", choices = colnames(r$df), selected = NULL)
      shiny::updateSelectizeInput(session = session, "display_columns", choices = colnames(r$df), selected = NULL)
    })
    
    # observeEvent(input$update_table, {
    shiny::observe({
      req(r$df)
      r$cols <- if (is.null(input$display_columns)){
        colnames(r$df)
      } else {
        input$display_columns
      }
      
      r$url_var <- if (input$url_column == ""){
        NULL
      } else {
        input$url_column
      }
    })
    
    output$data_display <- DT::renderDataTable({
      if (is.null(r$url_var)){
        r$df[r$cols] %>%
          DT::datatable(
            filter = "top"
          )
      } else {
        r$df[r$cols] %>%
          LimpiaR::limpiar_link_click(!!rlang::sym(r$url_var)) %>% 
          DT::datatable(
            filter = "top",
            escape = FALSE
            # extensions = c("Buttons"),
            # options = list(
            #   select = list(maxOptions = 2000),
            #   dom = 'Bfrtip',
            #   buttons = c("copy", "csv", "excel", "pdf")
            # )
          )
      }
    })
    
    
  })
  
}
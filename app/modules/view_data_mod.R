dataDisplayUi <- function(id){
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Uploaded Data"),
    bslib::card_body(
      shiny::uiOutput(ns("data_view_dropdown")),
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
    
    observe({
      req(r$df)
      if (is.null(input$display_columns)){
        r$cols <- colnames(r$df)
      } else {
        r$cols <- input$display_columns
      }
    })
    
    # shiny::observe({
    #   req(r$df)
    #   r$text_var <- input$text_column
    #   r$display_var <- input$display_columns
    #   r$url_var <- input$url_column
    # }) # set reactive values
    
    output$data_view_dropdown <- shiny::renderUI({
      dropdownButton_with_tooltip(
        select_input_with_tooltip(id = ns("url_column"), title = "URL Column",
                                  icon_info = "The name of the column with the post url",
                                  choice_list = colnames(r$df)),
        select_input_with_tooltip(id = ns("display_columns"), title = "Display Columns",
                                  icon_info = "The columns you want to display in the Uploaded Data table",
                                  choice_list = colnames(r$df), multiple_selections = TRUE),
        dropdown_title = "Modify Table View",
        icon_info = "Click here for table view customisation"
      )
    })
    
    # shiny::observeEvent(input$file_upload, {
    #   req(r$df)
    #   # shiny::updateSelectizeInput(session = session, "text_column", choices = colnames(r$df), selected = NULL)
    #   shiny::updateSelectizeInput(session = session, "url_column", choices = colnames(r$df), selected = NULL)
    #   shiny::updateSelectizeInput(session = session, "display_columns", choices = colnames(r$df), selected = NULL)
    # }) # selectInput updates
    # 
    
    output$data_display <- DT::renderDataTable({
      # message(is.null(r$url_var))
      # message("url var: ", r$url_var)
      # if (!is.null(r$url_var)){
      #   message("link clicking")
      #   r$df <- LimpiaR::limpiar_link_click(r$df, r$url_var)
      # }
      print(r$cols)

      DT::datatable(
      r$df[r$cols],
      filter = "top",
      # extensions = c("Buttons"),
      # options = list(
      #   select = list(maxOptions = 2000),
      #   dom = 'Bfrtip',
      #   buttons = c("copy", "csv", "excel", "pdf")
      # )
      )
    })
    
    
  })
  
}
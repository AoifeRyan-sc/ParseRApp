wloUi <- function(id){
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header(
      shiny::HTML("Weighted Log Odds <i>(make selections in dropdown to render)</i>")),
    bslib::card_body(
      dropdownButton_with_tooltip(
        select_input_with_tooltip(id = ns("wlo_group_column"), title = "Group Column:", 
                                  icon_info = "The name of the column with the groups you want to compare."),
        numeric_input_with_tooltip(ns("wlo_n_terms"), "Number of terms:", default_value = 20, 
                                   icon_info = "The number of terms you want to show per group."),
        numeric_input_with_tooltip(ns("wlo_n_rows"), "Number of rows:", default_value = 3, 
                                   icon_info = "The number of rows you want your groups to be rendered across. The number of columns will be automatically generated based on the number of rows you select."),
        select_input_with_tooltip(id = ns("wlo_filter_column"), title = "Filter By:", 
                                  icon_info = "Do you want to show terms that are the highest frequency in each group or the terms that are most associated with each category?",
                                  choice_list = c("association", "frequency"), select = "association"),
        dropdown_title = "Group Terms Inputs", 
        icon_info = "Click here for Group Terms customisation"
      ), # need to add more parameters here
      shinycssloaders::withSpinner(
        shiny::plotOutput(ns("gt_viz"))
      )
    ),
    full_screen = TRUE,
    min_height = 500
  )
}

wloServer <- function(id, r){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    shiny::observe({
      shiny::req(r$df)
      shiny::updateSelectizeInput(session = session, "wlo_group_column", choices = colnames(r$df), selected = NULL)
    })
    
    output$gt_viz <- shiny::renderPlot({
      req("clean_text" %in% colnames(r$df), input$wlo_group_column)
      message("calculating & plotting wlo")
      
      ParseR::calculate_wlos(
        df = r$df,
        text_var = clean_text,
        topic_var = !!rlang::sym(input$wlo_group_column),
        top_n = 30,
        filter_by = input$wlo_filter_column,
        top_terms_cutoff = 500,
        nrow = input$wlo_n_rows
      )
      # message("group terms plotted")
      
    })
    
    
  })
  
}
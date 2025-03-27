groupTermsUi <- function(id){
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header(
      shiny::HTML("Group Terms <i>(select text column to render</i>)")),
    bslib::card_body(
      dropdownButton_with_tooltip(
        select_input_with_tooltip(id = ns("gt_group_column"), title = "Group Column*", "The name of the column with the groups you want to compare."),
        numeric_input_with_tooltip(ns("gt_n_terms"), "Number of terms:", default_value = 20, 
                                   icon_info = "The minimum number of times an n-gram must be observed to be included."),
        text_input_with_tooltip(ns("gt_n_terms"), "Number of terms:", default_value = 20, 
                                   icon_info = "The minimum number of times an n-gram must be observed to be included."),
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

groupTermsServer <- function(id, r){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    shiny::observe({
      r$gt_n_terms <- input$gt_n_terms
      r$gt_group_var <- input$gt_group_column
    })
    
    shiny::observe({
      shiny::req(r$df)
      shiny::updateSelectizeInput(session = session, "gt_group_column", choices = colnames(r$df), selected = NULL)
    })
    
    output$gt_viz <- shiny::renderPlot({
      # req(!is.null(r$gt_group_var), r$df)
      req(r$gt_group_var)
      message("group var: ", r$gt_group_var)
      
      message("calculating & plotting group terms")
      
      ParseR::viz_group_terms_network(
        data = r$df,
        group_var = !!rlang::sym(r$gt_group_var),
        # text_var = !!rlang::sym(r$text_var),
        text_var = clean_text,
        n_terms = r$gt_n_terms,
        text_size = 4,
        with_ties = FALSE, # need to add customisation for all of this
        group_colour_map = NULL,
        terms_colour = "black",
        selected_terms = NULL,
        selected_terms_colour = "pink"
      )
      # message("group terms plotted")

    })
    
    
  })
  
}
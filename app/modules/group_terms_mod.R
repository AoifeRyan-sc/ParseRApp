groupTermsUi <- function(id){
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header(
      shiny::HTML("Group Terms <i>(make selections in dropdown to render)</i>")),
    bslib::card_body(
      dropdownButton_with_tooltip(
        select_input_with_tooltip(id = ns("gt_group_column"), title = "Group Column*", "The name of the column with the groups you want to compare."),
        numeric_input_with_tooltip(ns("gt_n_terms"), "Number of terms:", default_value = 20, 
                                   icon_info = "The number of terms you want to show per group."),
        text_input_with_tooltip(id = ns("gt_selected_terms"), "Emphasise terms:", 
                                icon_info = "Any terms you want to stand out in the output chart."),
        text_input_with_tooltip(id = ns("gt_emphasis_colour"), "HEX Code:", 
                                icon_info = "The HEX code for the colour you want to highlight selected terms in. Defaults to "),
        shiny::actionButton(ns("gt_action"), "Plot", icon = shiny::icon("magnifying-glass-chart")),
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
      shiny::req(r$df)
      shiny::updateSelectizeInput(session = session, "gt_group_column", choices = colnames(r$df), selected = NULL)
    })
    
    shiny::observeEvent(input$gt_action, {
      message("calculating & plotting group terms")
      
      r$gt_selected_terms <- if (is.null(input$gt_selected_terms)){
        NULL
      } else {
        strsplit(input$gt_selected_terms, ",\\s*")[[1]]
      }
      
      output$gt_viz <- shiny::renderPlot({
        ParseR::viz_group_terms_network(
          data = r$df,
          group_var = !!rlang::sym(input$gt_group_column),
          # text_var = !!rlang::sym(r$text_var),
          text_var = clean_text,
          n_terms = input$gt_n_terms,
          text_size = 4,
          with_ties = FALSE, # need to add customisation for all of this
          group_colour_map = NULL,
          terms_colour = "black",
          selected_terms = r$gt_selected_terms,
          selected_terms_colour = "pink"
        )
      })
    })
    
    
    
  })
  
}
groupTermsVizUi <- function(id){
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header(
      shiny::HTML("Group Terms <i>(make selections in dropdown to render)</i>")),
    bslib::card_body(
      dropdownButton_with_tooltip(
        select_input_with_tooltip(id = ns("gt_group_column"), title = "Group Column*",
                                  icon_info = "The name of the column with the groups you want to compare."),
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

groupTermsVizServer <- function(id, r){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    shiny::observe({
      shiny::req(r$df)
      shiny::updateSelectizeInput(session = session, "gt_group_column", choices = colnames(r$df), selected = NULL)
    })
    
    shiny::observeEvent(input$gt_action, {
      message("calculating & plotting group terms")
      r$viz_gt <- NULL # To facilitate css spinner timing
      r$gt_group_var <- input$gt_group_column
      
      r$gt_selected_terms <- if (is.null(input$gt_selected_terms)){
        NULL
      } else {
        strsplit(input$gt_selected_terms, ",\\s*")[[1]]
      }
      
      r$viz_gt <- ParseR::viz_group_terms_network(
        data = r$df,
        group_var = !!rlang::sym(r$gt_group_var),
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
    
    output$gt_viz <- shiny::renderPlot({
      req(r$viz_gt)
      r$viz_gt
    })
    
    
    
  })
  
}

groupTermsDataUi <- function(id){
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Group Terms Data"),
    bslib::card_body(
      shinycssloaders::withSpinner(
        shiny::uiOutput(ns("gt_data_output"))
      )
    ),
    full_screen = TRUE,
    style = "resize: vertical; overflow: auto;",
    height = "500px"
  )
}

groupTermsDataServer <- function(id, r){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
   output$gt_data_output <- shiny::renderUI({
     req(r$viz_gt)
     gt_terms <- get_gt_terms(r$viz_gt)
     shiny::tagList(
       select_input_with_tooltip(ns("gt_term_select"), "Select Term", 
                               icon_info = "Select a term from the group terms network that you would like to see posts on.",
                               choice_list = gt_terms, multiple_selections = TRUE),
       DT::dataTableOutput(ns("gt_data_display")) 
     )
   })
   
   shiny::observe({
     req(input$gt_term_select)
     print(input$gt_term_select)
     r$gt_table <- create_group_terms_table(input$gt_term_select, r$df, r$gt_group_var)
     message(class(r$gt_table))
   })
   
   output$gt_data_display <- DT::renderDataTable({
     req(r$viz_gt, r$gt_table)
     datatable_display_app(r$gt_table)
   })
    
  })
}


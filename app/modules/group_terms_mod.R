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
                                icon_info = "Any terms you want to stand out in the output chart.",
                                placeholder = "hispanic, heritage"),
        # text_input_with_tooltip(id = ns("gt_group_colour"), "Group Colours:", 
        #                         icon_info = "The HEX code for the group node colour.",
        #                         value = "#FFFF00"), # need to implement a per term box
        text_input_with_tooltip(id = ns("gt_term_colour"), "Term Colours:", 
                                icon_info = "The HEX code for the terms.",
                                value = "#000000"),
        text_input_with_tooltip(id = ns("gt_emphasis_colour"), "Emphasised Term Colours:", 
                                icon_info = "The HEX code for any emphasised terms.",
                                value = "#FFC0CB"),
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
      r$viz_gt <- NULL # To facilitate css spinner timing
      r$gt_group_var <- input$gt_group_column
      
      r$gt_selected_terms <- if (is.null(input$gt_selected_terms)){
        NULL
      } else {
        strsplit(input$gt_selected_terms, ",\\s*")[[1]]
      }
      
      r$viz_gt <- ParseR::viz_group_terms_network(
        data = collect(r$df),
        group_var = !!rlang::sym(r$gt_group_var),
        # text_var = !!rlang::sym(r$text_var),
        text_var = clean_text,
        n_terms = input$gt_n_terms,
        text_size = 4,
        with_ties = FALSE, # need to add customisation for all of this
        # group_colour_map = input$gt_group_colour,
        terms_colour = input$gt_term_colour,
        selected_terms = r$gt_selected_terms,
        selected_terms_colour = input$gt_emphasis_colour
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
     DT::dataTableOutput(ns("gt_data_display"))
   })
   
   shiny::observe({
     shiny::req(isTruthy(r$viz_gt))
     print("making table")
     gt_terms <- get_gt_terms(r$viz_gt)
     r$gt_table <- create_terms_table(terms = gt_terms, df = r$df, group_var = r$gt_group_var, message_var = r$text_var)
     # r$gt_table <- create_terms_table(viz_terms = input$gt_term_select, df = r$df, group_var = r$gt_group_var, message_var = r$text_var)
     message(class(r$gt_table))
   })
   
   output$gt_data_display <- DT::renderDataTable({
     req(r$gt_table)
     r$gt_table %>%
       collect() %>%
       mutate(Term = as.factor(Term),
              Group = as.factor(Group)) %>%
       datatable_display_app()
   })
    
  })
}


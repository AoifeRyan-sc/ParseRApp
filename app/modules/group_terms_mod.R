groupTermsVizUi <- function(id){
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header(
      shiny::HTML("Group Terms <i>(make selections in dropdown to render)</i>")),
    bslib::card_body(
      class = "d-flex flex-column h-100",
      dropdownButton_with_tooltip(
        select_input_with_tooltip(id = ns("gt_group_column"), title = "Group Column*",
                                  icon_info = "The name of the column with the groups you want to compare."),
        shiny::uiOutput(ns("gt_group_missing_error")),
        numeric_input_with_tooltip(ns("gt_n_terms"), "Number of terms:", default_value = 20, 
                                   icon_info = "The number of terms you want to show per group."),
        text_input_with_tooltip(id = ns("gt_selected_terms"), "Emphasise terms:", 
                                icon_info = "Any terms you want to stand out in the output chart.",
                                placeholder = "hispanic, heritage"),
        text_input_with_tooltip(id = ns("gt_term_colour"), "Term Colours:", 
                                icon_info = "The HEX code for the terms.",
                                value = "#000000"),
        text_input_with_tooltip(id = ns("gt_emphasis_colour"), "Emphasised Term Colours:", 
                                icon_info = "The HEX code for any emphasised terms.",
                                value = "#FFC0CB"),
        shiny::actionButton(ns("gt_action"), "Plot", icon = shiny::icon("magnifying-glass-chart")),
        dropdown_title = "Group Terms Inputs", 
        icon_info = "Click here for Group Terms customisation"
      ),
      shinycssloaders::withSpinner(
        shiny::plotOutput(ns("gt_viz")),
        fill = T
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
    output$gt_viz <- shiny::renderPlot({
      plot.new()  # Empty plot
      text(0.5, 0.5, "Click the 'Plot' button to generate visualization", cex = 1.2)
    })
    
    shiny::observeEvent(input$gt_action, {
      r$viz_gt <- NULL # To facilitate css spinner timing
      r$gt_group_var <- input$gt_group_column
      
      if (!shiny::isTruthy(r$text_var)){
        shinyalert::shinyalert("Select text variable to analyse",
                               closeOnEsc = TRUE,
                               closeOnClickOutside = FALSE,
                               type = "warning")
      } 
      else if (!shiny::isTruthy(r$gt_group_var)) {
        print("falsy")
        print(shiny::isTruthy(r$gt_group_var))
        output$gt_group_missing_error <- shiny::renderUI({
          missing_input_error("missing-selection-error", "Please select a group column")
        })
      }
      else {
        output$gt_group_missing_error <- shiny::renderUI({})
        print(r$gt_group_var)
        print(shiny::isTruthy(r$gt_group_variable))
        
        r$gt_selected_terms <- if (is.null(input$gt_selected_terms)){
          NULL
        } else {
          strsplit(input$gt_selected_terms, ",\\s*")[[1]]
        }
        
        r$viz_gt <- ParseR::viz_group_terms_network(
          data = dplyr::collect(r$df),
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
      }
    })
    
    output$gt_viz <- shiny::renderPlot({
      # req(r$viz_gt)
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
       dplyr::collect() %>%
       dplyr::mutate(Term = as.factor(Term),
              Group = as.factor(Group)) %>%
       datatable_display_app()
   })
    
  })
}


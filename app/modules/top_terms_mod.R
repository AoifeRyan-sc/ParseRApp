topTermsVizUi <- function(id){
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header(
      shiny::HTML("Top Terms <i>(make selections in dropdown to render)</i>")),
    bslib::card_body(
      dropdownButton_with_tooltip(
        select_input_with_tooltip(ns("top_terms_chart_type"), "Chart Type", 
                                  icon_info = "Do you want the output chart to be a bar chart or a lollipop chart?",
                                  choice_list = c("bars", "lollipops"), 
                                  select = "lollipop"),
        select_input_with_tooltip(ns("top_terms_group_column"), "Group Variable", 
                                  icon_info = "The column that contains the groups you want to create Top Terms for (if applicable), e.g. Sentiment, Platform"),
        numeric_input_with_tooltip(ns("top_terms_top_n"), "Number of terms:", default_value = 25,
                                   icon_info = "The number of terms to include."),
        numeric_input_with_tooltip(ns("top_terms_nrows"), "Number of rows:", default_value = 1,
                                   icon_info = "The number of rows to include in the output chart."),
        shiny::actionButton(ns("top_terms_action"), "Plot", icon = shiny::icon("magnifying-glass-chart")),
        dropdown_title = "Top Term Inputs", 
        icon_info = "Click here for Top Term customisation"
      ), # need to add more parameters here
      shinycssloaders::withSpinner(
        shiny::plotOutput(ns("top_terms_viz"))
      )
    ),
    full_screen = TRUE,
    min_height = 500
  )
}

topTermsVizServer <- function(id, r){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    shiny::observe({
      shiny::req(r$df)
      shiny::updateSelectizeInput(session = session, "top_terms_group_column", choices = c("No Group Variable" = "none", colnames(r$df)), selected = "none")
    })
    
    shiny::observeEvent(input$top_terms_action, {
      
      if (!shiny::isTruthy(r$text_var)){
        shinyalert::shinyalert("Select text variable to analyse",
                               closeOnEsc = TRUE,
                               closeOnClickOutside = FALSE,
                               type = "warning")
      } else {
        message("calculating top terms")
        r$top_terms <- NULL
        r$top_terms_group_var <- input$top_terms_group_column
        
        grouped <- r$top_terms_group_var != "none"
        
        r$top_terms <- make_top_terms(
          df = collect(r$df),
          n_terms = input$top_terms_top_n,
          group = grouped,
          group_var = !!rlang::sym(r$top_terms_group_var)
        )
        
        if (grouped){
          r$top_terms_viz <- viz_top_terms_group(
            r$top_terms, 
            type = input$top_terms_chart_type, 
            nrow = input$top_terms_nrows,
            group_var = !!rlang::sym(r$top_terms_group_var)
            )
        } else {
          r$top_terms_viz <- viz_top_terms_no_group(
            r$top_terms, 
            type = input$top_terms_chart_type, 
            nrow = input$top_terms_nrows
          )
        }
        
      }
      
    })
    
    output$top_terms_viz <- shiny::renderPlot({
      req(r$top_terms)
      r$top_terms_viz
      
    })
    
    
  })
  
}

topTermsDataUi <- function(id){
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header(
      shiny::HTML("Top Terms Data")),
    bslib::card_body(
      shinycssloaders::withSpinner(
        shiny::uiOutput(ns("top_terms_data_output"))
      )
    ),
    full_screen = TRUE,
    min_height = 500
  )
}

topTermsDataServer <- function(id, r){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$top_terms_data_output <- shiny::renderUI({
      req(r$viz_top_terms)
      DT::dataTableOutput(ns("top_terms_data_display")) 
    })
    
    shiny::observe({
      req(r$viz_top_terms)
      top_terms_terms <- get_top_terms_terms(r$viz_top_terms$view)
      r$top_terms_table <- create_terms_table(top_terms_terms, r$df, r$top_terms_group_var, r$text_var)
      print(head(r$top_terms_table))
    })
    
    output$top_terms_data_display <- DT::renderDataTable({
      req(r$top_terms_table)
      r$top_terms_table %>% 
        collect() %>%
        mutate(Term = as.factor(Term),
               Group = as.factor(Group)) %>%
        datatable_display_app()
    })
    
  })
  
}
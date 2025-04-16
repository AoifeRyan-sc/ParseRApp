topTermsVizUi <- function(id){
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header(
      shiny::HTML("Top Terms <i>(make selections in dropdown to render)</i>")),
    bslib::card_body(
      dropdownButton_with_tooltip(
        select_input_with_tooltip(ns("top_terms_group_column"), "Group Variable", 
                                  icon_info = "The column that contains the groups you want to create Top Terms for (if applicable), e.g. Sentiment, Platform"),
        numeric_input_with_tooltip(ns("top_terms_top_n"), "Number of terms:", default_value = 25,
                                   icon_info = "The number of terms to include."),
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
      shiny::updateSelectizeInput(session = session, "top_gterms_roup_column", choices = colnames(r$df), selected = NULL)
    })
    
    shiny::observeEvent(input$top_terms_action, {
      r$viz_top_terms <- NULL
      r$top_terms_group_var <- input$top_terms_group_column
      
      r$viz_wlo <- make_stop_terms(
        df = collect(r$df),
        text_var = clean_text,
        topic_var = !!rlang::sym(r$wlo_group_var),
        top_n = 30,
        filter_by = input$wlo_filter,
        top_terms_cutoff = 500,
        nrow = input$wlo_n_rows
      )
    })
    
    output$wlo_viz <- shiny::renderPlot({
      req(r$viz_wlo)
      r$viz_wlo
    })
    
    
  })
  
}

topTermsDataUi <- function(id){
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header(
      shiny::HTML("Weighted Log Odds Data")),
    bslib::card_body(
      shinycssloaders::withSpinner(
        shiny::uiOutput(ns("wlo_data_output"))
      )
    ),
    full_screen = TRUE,
    min_height = 500
  )
}

topTermsDataServer <- function(id, r){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$wlo_data_output <- shiny::renderUI({
      req(r$viz_wlo)
      DT::dataTableOutput(ns("wlo_data_display")) 
    })
    
    shiny::observe({
      req(r$viz_wlo)
      wlo_terms <- get_wlo_terms(r$viz_wlo$view)
      r$wlo_table <- create_terms_table(wlo_terms, r$df, r$wlo_group_var, r$text_var)
      print(head(r$wlo_table))
    })
    
    output$wlo_data_display <- DT::renderDataTable({
      req(r$wlo_table)
      r$wlo_table %>% 
        collect() %>%
        mutate(Term = as.factor(Term),
               Group = as.factor(Group)) %>%
        datatable_display_app()
    })
    
  })
  
}
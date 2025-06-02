wloVizUi <- function(id){
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header(
      shiny::HTML("Weighted Log Odds <i>(make selections in dropdown to render)</i>")),
    bslib::card_body(
      dropdownButton_with_tooltip(
        select_input_with_tooltip(id = ns("wlo_group_column"), title = "Group Column:", 
                                  icon_info = "The name of the column with the groups you want to compare."),
        shiny::uiOutput(ns("wlo_group_missing_error")),
        numeric_input_with_tooltip(ns("wlo_n_terms"), "Number of terms:", default_value = 20, 
                                   icon_info = "The number of terms you want to show per group."),
        numeric_input_with_tooltip(ns("wlo_n_rows"), "Number of rows:", default_value = 3, 
                                   icon_info = "The number of rows you want your groups to be rendered across. The number of columns will be automatically generated based on the number of rows you select."),
        select_input_with_tooltip(id = ns("wlo_filter"), title = "Filter By:", 
                                  icon_info = "Do you want to show terms that are the highest frequency in each group or the terms that are most associated with each category?",
                                  choice_list = c("association", "frequency"), select = "association"),
        shiny::actionButton(ns("wlo_action"), "Plot", icon = shiny::icon("magnifying-glass-chart")),
        dropdown_title = "Weighted Log Odds Inputs", 
        icon_info = "Click here for WLO customisation"
      ), # need to add more parameters here
      save_dropdown("wlo", ns),
      shinycssloaders::withSpinner(
        shiny::plotOutput(ns("wlo_viz")),
        fill = T
      )
    ),
    full_screen = TRUE,
    style = "resize: vertical; overflow: auto;",
    height = "500px"
  )
}

wloVizServer <- function(id, r){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    shiny::observe({
      shiny::req(r$df)
      shiny::updateSelectizeInput(session = session, "wlo_group_column", choices = colnames(r$df), selected = NULL)
    })
    
    shiny::observeEvent(input$wlo_action, {
      r$viz_wlo <- NULL
      r$wlo_group_var <- input$wlo_group_column
      
      if (!shiny::isTruthy(r$text_var)){
        shinyalert::shinyalert("Select text variable to analyse",
                               closeOnEsc = TRUE,
                               closeOnClickOutside = FALSE,
                               type = "warning")
      } 
      else if (!shiny::isTruthy(r$wlo_group_var)) {
        print("falsy")
        print(shiny::isTruthy(r$gt_group_var))
        output$wlo_group_missing_error <- shiny::renderUI({
          missing_input_error("missing-selection-error", "Please select a group column")
        })
      }
      else {
        output$gt_group_missing_error <- shiny::renderUI({})
        
        r$viz_wlo <- ParseR::calculate_wlos(
          df = dplyr::collect(r$df),
          text_var = clean_text,
          topic_var = !!rlang::sym(r$wlo_group_var),
          top_n = 30,
          filter_by = input$wlo_filter,
          top_terms_cutoff = 500,
          nrow = input$wlo_n_rows
        )
      }
    })
    
    output$wlo_viz <- shiny::renderPlot({
      r$viz_wlo
    })
    
    output$wlo_save <- shiny::downloadHandler(
      filename = function(file) {
        paste0(input$wlo_save_title, ".png")
      },
      content = function(file) {
        ggplot2::ggsave(file, r$viz_wlo, plot = , width = input$wlo_save_width, bg = "white", height = input$wlo_save_height, units = input$wlo_save_units, dpi = 300)
      }
    )
    
    
  })
  
}

wloDataUi <- function(id){
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header(
      shiny::HTML("Weighted Log Odds Data")),
    bslib::card_body(
      shinycssloaders::withSpinner(
        shiny::uiOutput(ns("wlo_data_output")),
        fill = T
      )
    ),
    full_screen = TRUE,
    style = "resize: vertical; overflow: auto;",
    height = "500px"
  )
}

wloDataServer <- function(id, r){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$wlo_data_output <- shiny::renderUI({
      # req(r$viz_wlo)
      DT::dataTableOutput(ns("wlo_data_display")) 
    })
    
    shiny::observe({
      req(r$viz_wlo)
      wlo_terms <- get_wlo_terms(r$viz_wlo$view)
      r$wlo_table <- create_terms_table(wlo_terms, r$df, r$wlo_group_var, r$text_var)
    })
    
    output$wlo_data_display <- DT::renderDataTable({
      req(r$wlo_table)
      r$wlo_table %>% 
        dplyr::collect() %>%
        dplyr::mutate(Term = as.factor(Term),
               Group = as.factor(Group)) %>%
        datatable_display_app()
    })
    
  })
  
}
bigramVizUi <- function(id){
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header(
      shiny::HTML("Bigram <i>(make modifications in dropdown</i>)")),
    bslib::card_body(
      dropdownButton_with_tooltip(
        select_input_with_tooltip(ns("bigram_group_column"), "Group Variable", 
                                  icon_info = "The column that contains the groups you want to create bigrams for (if applicable), e.g. Sentiment, Platform"),
        numeric_input_with_tooltip(ns("bigram_min_freq"), "Minimum frequency", default_value = 5, 
                                   icon_info = shiny::HTML("The minimum number of times an n-gram must be observed to be included. Think about the size of the dataset you're making a bigram for, is a bigram pair appearing 10 times significant? Would you want to look at pairs that occur more or less than this?.\n\n<i>Note: Be aware of long text chains, they likely represent spam you want to remove.</i>")),
        numeric_input_with_tooltip(ns("bigram_top_n"), "Number of bigrams:", default_value = 25,
                                   icon_info = "The number of n-grams to include."),
        shiny::actionButton(ns("bigram_action"), "Plot", icon = shiny::icon("magnifying-glass-chart")),
        dropdown_title = "Bigram Inputs", 
        icon_info = "Click here for bigram customisation"
      ),
      shinycssloaders::withSpinner(
        shiny::uiOutput(ns("bigram_card_layout"))
      )
    ),
    full_screen = TRUE,
    min_height = "300px"
  )
}

bigramVizServer <- function(id, r){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    shiny::observe({
      shiny::req(r$df)
      shiny::updateSelectizeInput(session = session, "bigram_group_column", choices = c("No Group Variable" = "none", colnames(r$df)), selected = NULL)
    }) # update select input
    
    shiny::observeEvent(input$bigram_action, {
      
      r$bigram_calculated <- FALSE
      r$bigram <- NULL
      r$n_bigrams <- NULL
      
      if (is.null(input$bigram_group_column) | input$bigram_group_column == "none"){
        r$bigram <- count_ngram_app(df = r$df, text_var = clean_text, top_n = input$bigram_top_n, min_freq = input$bigram_min_freq)
        r$n_bigrams <- 1
      } else {
        df_groups <- split(r$df, r$df[input$bigram_group_column])
        r$bigram <- lapply(df_groups, function(group_df) {
          count_ngram_app(df = group_df, text_var = clean_text, top_n = input$bigram_top_n, min_freq = input$bigram_min_freq)
        })
        r$n_bigrams <- length(r$bigram) 
      }
      
      r$bigram_calculated <- TRUE
      
      if (r$n_bigrams > 1){
        lapply(seq_along(r$bigram), function(i) {
          output[[paste0("bigram_group_", i)]] <- shiny::renderPlot({
            req(r$bigram)
            ParseR::viz_ngram(r$bigram[[i]]$viz )
          })
        })
      } else {
        output$bigram_group_1 <- shiny::renderPlot({
          req(r$bigram)
          ParseR::viz_ngram(r$bigram$viz)
        })
      } # render ui
  
      })
    
    output$bigram_card_layout <- shiny::renderUI({
      req(r$bigram)
      
      if (r$n_bigrams > 1){
        nav_panels <- lapply(seq_len(r$n_bigrams), function(i) {
          bigram_name <- names(r$bigram)[i]
          bslib::nav_panel(bigram_name, shiny::plotOutput(ns(paste0("bigram_group_", i))))
        })
        
        # shinycssloaders::withSpinner(
          bslib::navset_underline(!!!nav_panels)
        # ) 
      } else {
        # # shinycssloaders::withSpinner(
          shiny::plotOutput(ns("bigram_group_1"))
        # )
      }
    }) # ui layout
    
    })
}

bigramDataUi <- function(id){
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Bigram Data"),
    bslib::card_body(
      shiny::uiOutput(ns("bigram_data_display"))
    ),
    full_screen = TRUE,
    style = bslib::css(
      gap ="0.25rem",
      resize = "horizontal"
    ),
    min_height = "300px"
  )
}

bigramDataServer <- function(id, r){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    shiny::observeEvent(r$bigram_calculated, {
      shiny::req(isTruthy(r$bigram_calculated))
      message("calced?: ", r$bigram_calculated)
      message("n_bigrams: ", r$n_bigrams)
      
      if (r$n_bigrams > 1){
        r$bigram_table <- lapply(seq_along(r$bigram), function(i){
          bigram_pairs(r$bigram[[i]]$view, r$df)
        })
        names(r$bigram_table) <- names(r$bigram)
        message("creation multiple: ", class(r$bigram_table))
      } else {
        r$bigram_table <- bigram_pairs(r$bigram$view, r$df)
        message("creation one: ", class(r$bigram_table))
      } # create tables
      
      output$bigram_data_display <-  shiny::renderUI({
        message("ui layout deciding")
        if (r$n_bigrams > 1){
          message("multiple tables deciding")
          table_nav_panels <- lapply(seq_len(r$n_bigrams), function(i) {
            table_name <- names(r$bigram)[i]
            bslib::nav_panel(table_name, DT::dataTableOutput(ns(paste0("bigram_data_table_", i))))
          })
          
          return(bslib::navset_underline(
            !!!table_nav_panels
          ))
        } else {
          message("one table deciding")
          return(DT::dataTableOutput(ns("bigram_data_table_1")))
        }
      }) # ui layout
      
      if (r$n_bigrams > 1){
        message("mutiple tables rendering")
        lapply(seq_along(r$bigram_table), function(i) {
          output[[paste0("bigram_data_table_", i)]] <- DT::renderDataTable({
            datatable_display_app(df = r$bigram_table[[i]])
            })
        })
      } else {
        message("one table rendering")
        output$bigram_data_table_1 <- DT::renderDataTable({
          datatable_display_app(df = r$bigram_table)
        })
      } # render yu
      
    })
    
  })
}


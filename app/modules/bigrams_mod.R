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
        # shiny::plotOutput(ns("bigram_viz"))
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
      
      # r$bigram_go_pressed <- input$bigram_action
      r$bigram_calculated <- FALSE
      
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
      
      output$bigram_card_layout <- shiny::renderUI({
        req(r$n_bigrams)
        
        if (r$n_bigrams > 1){
          nav_panels <- lapply(seq_len(r$n_bigrams), function(i) {
            bigram_name <- names(r$bigram)[i]
            bslib::nav_panel(bigram_name, shiny::plotOutput(ns(paste0("bigram_group_", i))))
          })
          
          bslib::navset_underline(
            !!!nav_panels
          )
        } else {
          shiny::plotOutput(ns("bigram_group_1"))
        }
      }) # ui layout
      
      if (r$n_bigrams > 1){
        lapply(seq_along(r$bigram), function(i) {
          output[[paste0("bigram_group_", i)]] <- shiny::renderPlot({
            req(r$bigram[[i]])  # Ensure the plot exists
            ParseR::viz_ngram(r$bigram[[i]]$viz )
          })
        })
      } else {
        output$bigram_group_1 <- shiny::renderPlot({
          ParseR::viz_ngram(r$bigram$viz)
        })
      }
      
      r$bigram_calculated <- TRUE
      }) # create bigram lists
    
    
    
    # shiny::observe({
    #   req(r$bigram)
    # 
    #   if (r$n_bigrams > 1){
    #     lapply(seq_along(r$bigram), function(i) {
    #       output[[paste0("bigram_group_", i)]] <- shiny::renderPlot({
    #         req(r$bigram[[i]])  # Ensure the plot exists
    #         ParseR::viz_ngram(r$bigram[[i]]$viz )
    #       })
    #     })
    #   } else {
    #     output$bigram_group_1 <- shiny::renderPlot({
    #        ParseR::viz_ngram(r$bigram$viz)
    #     })
    #   }
    #   }) # define plot outputs
    
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
    
    shiny::observeEvent(r$bigram_calculated, { #input$bigram_viz_card-bigram_action, {
      req(shiny::isTruthy(r$bigram_calculated), shiny::isTruthy(r$n_bigrams))
      # req(shiny::isTruthy(r$bigram_calculated))
      r$bigram_table_calculated <- FALSE
      message("initiated")
      
      if (r$n_bigrams > 1){
        r$bigram_table <- lapply(seq_along(r$bigram), function(i){
          bigram_pairs(r$bigram[[i]]$view, r$df)
        }) 
        names(r$bigram_table) <- names(r$bigram)
        message("creation multiple: ", class(r$bigram_table))
      } else {
        r$bigram_table <- bigram_pairs(r$bigram$view, r$df) 
        message("creation one: ", class(r$bigram_table))
      }
      
      message("table(s) created")
      
      output$bigram_data_display <-  shiny::renderUI({
        message("ui layout deciding")
        req(r$n_bigrams)
        if (r$n_bigrams > 1){
          message("multiple tables deciding")
          nav_panels <- lapply(seq_len(r$n_bigrams), function(i) {
            bigram_name <- names(r$bigram)[i]
            bslib::nav_panel(bigram_name, DT::dataTableOutput(ns(paste0("bigram_data_table_", i))))
          })
          
          bslib::navset_underline(
            !!!nav_panels
          )
        } else {
          message("one table deciding")
          DT::dataTableOutput(ns("bigram_data_table"))
        }
        
        r$bigram_table_calculated <- TRUE
        message("ui layout decided")
      })
      
    })
    
    shiny::observeEvent(r$bigram_table_calculated, {
      req(shiny::isTruthy(r$bigram_table_calculated), shiny::isTruthy(r$n_bigrams))
      # print(class(r$bigram_table))
      # print("n bigrams: ", r$n_bigrams)
      # req(shiny::isTruthy(r$bigram_table_calculated))
      message("creating layout")
      print(r$bigram_table_calculated)
      if (r$n_bigrams > 1){
        message("multiple layout rendering")
        output$bigram_data_table_1 <- DT::renderDataTable({
          DT::datatable(r$bigram_table[[3]])
        })
        # print("trying more than 1")
        # print(class(r$bigram_table))
        # lapply(seq_along(r$bigram_table), function(i) {
        #   output[[paste0("bigram_data_table_", i)]] <- DT::renderDataTable({
        #     DT::datatable(
        #       r$bigram_table[[i]],
        #       filter = "top",
        #       "pageLength": 5,
        #       # extensions = c("Buttons"),
        #       # options = list(
        #       #   select = list(maxOptions = 2000),
        #       #   dom = 'Bfrtip',
        #       #   buttons = c("copy", "csv", "excel", "pdf")
        #       # )
        #     )
        #   })
          # output[[paste0("bigram_data_table_", i)]] <- DT::datatable({
          #   req(r$bigram_table[[i]])  # Ensure the plot exists
          #   datatable_display_app(df = r$bigram_table[[i]])
          # })
        # })
      } else {
        message("single layout rendering")
        output$bigram_data_table <- DT::renderDataTable({
           DT::datatable(r$bigram_table)
          # datatable_display_app(df = r$bigram_table)
        })
      }
    })
    
    
  })
  
}
bigramVizUi <- function(id){
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header(
      shiny::HTML("Bigram <i>(make modifications in dropdown</i>)")),
    bslib::card_body(
      dropdownButton_with_tooltip(
        numeric_input_with_tooltip(ns("bigram_min_freq"), "Minimum frequency", default_value = 5, 
                                   icon_info = shiny::HTML("The minimum number of times an n-gram must be observed to be included. Think about the size of the dataset you're making a bigram for, is a bigram pair appearing 10 times significant? Would you want to look at pairs that occur more or less than this?.\n\n<i>Note: Be aware of long text chains, they likely represent spam you want to remove.</i>")),
        numeric_input_with_tooltip(ns("bigram_top_n"), "Number of bigrams:", default_value = 25,
                                   icon_info = "The number of n-grams to include."),
        shiny::actionButton(ns("bigram_action"), "Plot", icon = shiny::icon("magnifying-glass-chart")),
        dropdown_title = "Bigram Inputs", 
        icon_info = "Click here for bigram customisation"
      ),
      shinycssloaders::withSpinner(
        shiny::plotOutput(ns("bigram_viz"))
        )
    ),
    full_screen = TRUE,
    min_height = 300
  )
}

bigramVizServer <- function(id, r){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    shiny::observeEvent(input$bigram_action, {
      output$bigram_viz <- shiny::renderPlot({
        req("clean_text" %in% colnames(r$df))
        
        message("calculating ngrams")
        
        r$bigram <- ParseR::count_ngram(
          df = r$df,
          # text_var = !!rlang::sym(r$text_var),
          text_var = clean_text,
          top_n = input$bigram_top_n,
          min_freq = input$bigram_min_freq,
          remove_stops = FALSE, # will set to true later, this is for iteration speed
          clean_text = FALSE, # will set to true later, this is for iteration speed
          hashtags = FALSE, # will set to true later, this is for iteration speed
          mentions = FALSE, # will set to true later, this is for iteration speed
          distinct = FALSE # will set to true later, this is for iteration speed
        )
        
        message("plotting")
        r$bigram %>% purrr::pluck("viz") %>%
          ParseR::viz_ngram()
      })
      
      
    })
    })
  
}

bigramDataUi <- function(id){
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Bigram Data"),
    bslib::card_body(
      DT::dataTableOutput(ns("bigram_data_display"))
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
    
    shiny::observe({
      req("clean_text" %in% colnames(r$df))
      r$bigram_table <- bigram_pairs(r$bigram$view, r$df)
    })
    
    output$bigram_data_display <- DT::renderDataTable({
     
      DT::datatable(
        r$bigram_table,
        filter = "top",
        # extensions = c("Buttons"),
        # options = list(
        #   select = list(maxOptions = 2000),
        #   dom = 'Bfrtip',
        #   buttons = c("copy", "csv", "excel", "pdf")
        # )
      )
    })
    
    
  })
  
}
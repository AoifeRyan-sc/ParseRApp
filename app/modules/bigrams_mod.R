bigramVizUi <- function(id){
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header(
      shiny::HTML("Bigram <i>(select text column to render</i>)")),
    bslib::card_body(
      dropdownButton_with_tooltip(
        numeric_input_with_tooltip(ns("bigram_min_freq"), "Minimum frequency", default_value = 5, 
                                   icon_info = "The minimum number of times an n-gram must be observed to be included."),
        numeric_input_with_tooltip(ns("bigram_top_n"), "Number of bigrams:", default_value = 25,
                                   icon_info = "The number of n-grams to include."),
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
    
    shiny::observe({
      r$bigram_min_freq <- input$bigram_min_freq
      r$bigram_top_n <- input$bigram_top_n
    })
    
    output$bigram_viz <- shiny::renderPlot({
      req("clean_text" %in% colnames(r$df))
      
      message("calculating ngrams")
      
      r$bigram <- ParseR::count_ngram(
        df = r$df,
        # text_var = !!rlang::sym(r$text_var),
        text_var = clean_text,
        top_n = r$bigram_top_n,
        min_freq = r$bigram_min_freq,
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
  
}

bigramDataUi <- function(id){
  
}

bigramDataServer <- function(id, r){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    
  })
  
}
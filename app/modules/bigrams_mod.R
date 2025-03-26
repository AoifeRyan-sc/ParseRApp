dataBigramUi <- function(id){
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Bigram"),
    bslib::card_body(
      shiny::plotOutput(ns("bigram_viz"))
    ),
    full_screen = TRUE,
    # style = bslib::css(
    #   gap ="0.25rem",
    #   resize = "vertical"
    # ),
    min_height = 500
  )
}

dataBigramServer <- function(id, r){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(r$text_var, {
      req(r$text_var)
      message("calculating ngrams")
      r$bigram <- r$df %>% 
        ParseR::count_ngram(
          # text_var = r$text_var,
          text_var = Message,
          top_n = r$bigram_top_n,
          # min_freq = r$bigram_min_freq,
          min_freq = 10,
          remove_stops = F, # will set to true later, this is for iteration speed
          clean_text = F, # will set to true later, this is for iteration speed
          hashtags = F, # will set to true later, this is for iteration speed
          mentions = F, # will set to true later, this is for iteration speed
          distinct = F, # will set to true later, this is for iteration speed
        )
      print(names(r$bigram))
    })
    
    output$bigram_viz <- shiny::renderPlot({
      req(r$bigram)
      message("plotting")
      r$bigram %>% purrr::pluck("viz") %>%
        ParseR::viz_ngram()
    })
    
    
  })
  
}
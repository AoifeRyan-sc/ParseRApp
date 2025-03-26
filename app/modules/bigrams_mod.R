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
      print(r$text_var)
      r$bigram <- ParseR::count_ngram(
        r$df,
        # "Message"
        # as.name(r$text_var)
        !!rlang::sym(r$text_var)
      )
      # r$bigram <- ParseR::count_ngram(
      #   df = r$df,
      #   text_var = r$text_var
        # top_n = r$bigram_top_n,
        # min_freq = r$bigram_min_freq,
        # remove_stops = FALSE, # will set to true later, this is for iteration speed
        # clean_text = FALSE, # will set to true later, this is for iteration speed
        # hashtags = FALSE, # will set to true later, this is for iteration speed
        # mentions = FALSE, # will set to true later, this is for iteration speed
        # distinct = FALSE # will set to true later, this is for iteration speed
      # )
      # print(names(r$bigram))
    })
    
    output$bigram_viz <- shiny::renderPlot({
      req(r$bigram)
      message("plotting")
      r$bigram %>% purrr::pluck("viz") %>%
        ParseR::viz_ngram()
    })
    
    
  })
  
}
# tooltip layouts ----
select_input_with_tooltip <- function(id, title, icon_info, choice_list = list(), select = NULL, multiple_selections = FALSE){
  
  if (is.null(select)){
    select_widget <- shiny::selectizeInput(
      id, title, choices = choice_list, 
      multiple = multiple_selections,
      options = list(
        placeholder = 'Please select an option below',
        onInitialize = I('function() { this.setValue(""); }')
      ))
  } else {
    select_widget <- shiny::selectizeInput(
      id, title, choices = choice_list, 
      multiple = multiple_selections,
      selected = select)
  }

  shiny::div(
    style = "position: relative",
    select_widget,
    shiny::div(
      style = "position: absolute; top: 0; right: 5px;",
      bslib::tooltip(
        bsicons::bs_icon("question-circle-fill"),
        icon_info
      )
    )
  )
}

numeric_input_with_tooltip <- function(id, title, default_value, icon_info){
  
  shiny::div(
    style = "position: relative",
    shiny::numericInput(id, title, value = default_value),
    shiny::div(
      style = "position: absolute; top: 0; right: 5px;",
      bslib::tooltip(
        bsicons::bs_icon("question-circle-fill"),
        icon_info
      )
    )
  )
}

text_input_with_tooltip <- function(id, title, icon_info){
  
  shiny::div(
    style = "position: relative",
    shiny::textInput(id, title, placeholder = "hispanic, heritage"),
    shiny::div(
      style = "position: absolute; top: 0; right: 5px;",
      bslib::tooltip(
        bsicons::bs_icon("question-circle-fill"),
        icon_info
      )
    )
  )
}

dropdownButton_with_tooltip <- function(..., dropdown_title, icon_info){
  shiny::div(
    style = "position: absolute; top: 3px; right: 15px;",
    bslib::tooltip(
      shinyWidgets::dropdownButton(
        shiny::tags$h3(dropdown_title),
        circle = TRUE,
        size = "sm",
        icon = shiny::icon("gear"),
        width = "200px",
        status = "primary",
        tags$style(HTML("
          .dropdown-toggle::after {
            display: none !important;
          }
        ")),
        ...
      ),
      icon_info
    )
  )
}

# Text processing ----
detect_factor <- function(x, threshold = 0.1) {
  if (is.numeric(x)) {
    return(FALSE)
  }
  unique_ratio <- length(unique(x)) / length(x)
  has_repeats <- any(table(x) > 1)
  
  return(unique_ratio <= threshold | has_repeats)
}

convert_to_factor <- function(x, factor){
  if (factor){
    x <- as.factor(x)
  }
  return(x)
}

# Chart output processing ----

bigram_pairs <- function(bigram_output, df){

  bigram_pairs <- paste(bigram_output$word1, bigram_output$word2, sep = " ")
  bigram_df <- purrr::map_dfr(bigram_pairs, function(bigram_pairs) {
    df %>%
      dplyr::filter(stringr::str_detect(clean_text, stringr::fixed(bigram_pairs, ignore_case = TRUE))) %>%
      dplyr::mutate(bigram_pairs = bigram_pairs) %>%
      dplyr::select(bigram_pairs, Message, clean_text) %>%
      dplyr::mutate(bigram_pairs = as.factor(bigram_pairs))
  })
    
  
  return(bigram_df)
}


# Personalised functions ----
count_ngram_app <- function(df, text_var, top_n, min_freq){
  ParseR::count_ngram(
    df = df,
    text_var = {{text_var}},
    top_n = top_n,
    min_freq = min_freq,
    remove_stops = FALSE, # will set to true later, this is for iteration speed
    clean_text = FALSE, # will set to true later, this is for iteration speed
    hashtags = FALSE, # will set to true later, this is for iteration speed
    mentions = FALSE, # will set to true later, this is for iteration speed
    distinct = FALSE # will set to true later, this is for iteration speed
  )
}

datatable_display_app <- function(df){
  DT::datatable(
    df,
    filter = "top",
    # "pageLength": 5,
    # extensions = c("Buttons"),
    # options = list(
    #   select = list(maxOptions = 2000),
    #   dom = 'Bfrtip',
    #   buttons = c("copy", "csv", "excel", "pdf")
    # )
  )
}
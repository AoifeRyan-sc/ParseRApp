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

text_input_with_tooltip <- function(id, title, icon_info, placeholder = NULL, value = NULL){
  
  shiny::div(
    style = "position: relative",
    shiny::textInput(id, title, placeholder = placeholder, value = value),
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

bigram_pairs <- function(bigram_output, df, message_var){

  bigram_pairs <- paste(bigram_output$word1, bigram_output$word2, sep = " ")
  bigram_df <- purrr::map_dfr(bigram_pairs, function(bigram_pairs) {
    df %>%
      dplyr::filter(stringr::str_detect(clean_text, stringr::fixed(bigram_pairs, ignore_case = TRUE))) %>%
      dplyr::mutate(bigram_pairs = bigram_pairs) %>%
      dplyr::select(bigram_pairs, message_var, clean_text) %>%
      dplyr::mutate(bigram_pairs = as.factor(bigram_pairs))
  })
    
  
  return(bigram_df)
}

bigram_pairs_wip <-function(bigram_output, df, message_var){
  
  bigram_pairs <- paste(bigram_output$word1, bigram_output$word2, sep = " ")
  bigram_df <- purrr::map_dfr(bigram_pairs, function(bigram_pair) {
    df_filtered <- df[grep(bigram_pair, df$clean_text, fixed = T),]
    if (nrow(df_filtered) > 0){
      print(class(bigram_pair))
      print(bigram_pair)
      df_filtered$bigram_pair <- as.factor(bigram_pair)
    } 
    
    df_filtered <- df_filtered[c("bigram_pair", message_var, "clean_text")]
    print("0 rows")
    
    # 
    # df_filtered$bigram_pair <- as.factor(df_filtered$bigram_pair)
    return(df_filtered)
  })
  
  
  return(bigram_df)
  # return(bigram_pairs)
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
    options = list(
      select = list(maxOptions = 2000),
      dom = 'Bfrtip',
      buttons = c("copy", "csv", "excel", "pdf"),
      pageLength = 5
    )
  )
}

# Group Terms Functions ----
create_group_terms_table_opt <- function(graph_obj, df, group_var){
  
  graph_data <- graph_obj$data
  group_term_node_size <- sort(graph_data$size, decreasing = T)[1]
  group_terms <- graph_data[graph_data$size == group_term_node_size, ][["node_name"]]
  
  graph_terms <- graph_data[!graph_data$node_name %in% group_terms,][["node_name"]]
  
  df_table <- purrr::map_dfr(graph_terms, function(term){
    df_term <- df[grep(term, df$clean_text, fixed = T),]
    df_term$Term <- term
    df_term <- df_term[c(as.factor(Term), as.factor(group_var), Message)]
    return(df_term)
  }) 
  
  return(df_table)
}

get_gt_terms <- function(graph_obj){
  
  graph_data <- graph_obj$data
  group_term_node_size <- sort(graph_data$size, decreasing = T)[1]
  group_terms <- graph_data[graph_data$size == group_term_node_size, ][["node_name"]]
  graph_terms <- graph_data[!graph_data$node_name %in% group_terms,][["node_name"]]
  
  return(graph_terms)
}

create_group_terms_table <- function(graph_terms, df, group_var){

  df_table <- purrr::map_dfr(graph_terms, function(term){
    df_term <- df[grep(term, df$clean_text, fixed = T),]
    df_term$Term <- term
    df_term <- df_term[c("Term", group_var, "Message")]
    df_term$Term <- as.factor(df_term$Term)
    df_term[[group_var]] <- as.factor(df_term[[group_var]])
    return(df_term)
  }) 
  
}

# wlo functions -----
get_wlo_terms <- function(wlo_view){

  wlo_terms <- unique(wlo_view$word)
  
  return(wlo_terms)
}

# create tables ----

create_terms_table <- function(viz_terms, df, group_var, message_var){
  
  df_table <- purrr::map_dfr(viz_terms, function(term){
    df_term <- df[grep(term, df$clean_text, fixed = T),]
    df_term$Term <- term
    df_term <- df_term[c("Term", group_var, message_var)]
    df_term$Term <- as.factor(df_term$Term)
    df_term[[group_var]] <- as.factor(df_term[[group_var]])
    return(df_term)
  }) 
  
  return(df_table)
}

# ----
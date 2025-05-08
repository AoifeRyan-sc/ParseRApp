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
      dplyr::filter(stringr::str_detect(clean_text, stringr::fixed(bigram_pairs, ignore_case = FALSE))) %>%
      dplyr::mutate(bigram_pairs = bigram_pairs) %>%
      dplyr::select(bigram_pairs, message_var, clean_text) %>%
      dplyr::collect()
  })
    
  
  return(bigram_df)
}

# Personalised functions ----
count_ngram_app <- function(df, text_var, top_n, min_freq){
  ngram_data <- count_ngram_temp(
  # ParseR::count_ngram(
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
  return(ngram_data)
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
create_terms_table <- function(terms, df, group_var, message_var){
  
  df_table <- purrr::map_dfr(terms, function(term){
    df %>%
      dplyr::filter(stringr::str_detect(clean_text, stringr::fixed(term, ignore_case = FALSE))) %>%
      dplyr::mutate(Term = term) %>%
      dplyr::select(Term, Group = group_var, message_var, clean_text) %>%
      dplyr::collect()
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

create_terms_table_legacy<- function(viz_terms, df, group_var, message_var){
  
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

#  duckdb ----

make_duckdb <- function(df, con, name){
  duckdb::dbWriteTable(
    conn = con,
    name = name,
    df,
    overwrite = TRUE
  )
}

# cleaning ----
clean_df <- function(df, message_var, duckdb = F){
  
  df <- df %>%
    dplyr::mutate(clean_text = message_var) %>%
    ParseR::clean_text(
      text_var = clean_text,
      tolower = T, # should make some of this customisable
      remove_mentions = T,
      remove_punctuation = T,
      remove_digits = T,
      in_parallel = F # be aware if we are deploying this - does this work with duckdb?
    ) 
  
  if (duckdb){
    df <- df %>%
      dplyr::collect() %>%
      dplyr::mutate(clean_text = tm::removeWords(clean_text, tm::stopwords(kind = "SMART"))) %>%
      LimpiaR::limpiar_spaces(clean_text)
  } else {
    df <- df %>%
      dplyr::mutate(clean_text = tm::removeWords(clean_text, tm::stopwords(kind = "SMART"))) %>%
      LimpiaR::limpiar_spaces(clean_text)
  }
  
  return(df)
}

# popups ----
file_size_logic <- function(file, df, ns){
  
  if (file){
    shiny::showModal(shiny::modalDialog(
      title = "Select a Column",
      select_input_with_tooltip(id = ns("text_column"), title = "Text Column*",
                                icon_info = "The name of the column with the text you want to analyse",
                                choice_list = colnames(df)),
      select_input_with_tooltip(id = ns("author_column"), title = "Author Column*",
                                icon_info = "The name of the author column",
                                choice_list = colnames(df)),
      select_input_with_tooltip(id = ns("date_column"), title = "Date Column*",
                                icon_info = "The name of the date column",
                                choice_list = colnames(df)),
      footer = shiny::actionButton(ns("confirm_text_col"), "Go!")
    ))
  } else {
    shinyalert::shinyalert("File must have less than 50k rows.",
                           closeOnEsc = TRUE,
                           closeOnClickOutside = FALSE,
                           type = "warning")
  }
}

# top terms ----
make_top_terms <- function(df, n_terms){
  
  top_terms <- tmp_con %>% dplyr::collect() %>%
    tidytext::unnest_tokens(output = word, input = clean_text, token = "words", to_lower = FALSE) %>%
    count(word) %>%
    slice_max(n = n_terms, order_by = n, with_ties = F)
    
}

viz_top_terms <- function(top_terms){
  
}

#' Generate counts for the most frequent n-grams in text.
#'
#' Function returns a list with a viz and a view object. The viz object can be fed into ParseR's `viz_ngram` function to produce a network visualisation.
#'
#' @param df A dataframe.
#' @param text_var The variable containing the text.
#' @param n The number of terms to include in the n-gram. E.g. 2 produces a bi-gram.
#' @param top_n The number of n-grams to include.
#' @param min_freq The minimum number of times an n-gram must be observed to be included.
#' @param distinct If TRUE, will count # of unique posts for each n-gram.
#' @param hashtags Should hashtags be included in the n-grams?
#' @param mentions Should mentions be included in the n-grams?
#' @param clean_text Should the text variable be cleaned?
#' @param remove_stops Should stopwords be removed?
#' @param tolower Should all tokens be lower cased in calls to unnest_tokens?
#' @param ... fed to the `ParseR::clean_text()` function
#'
#' @return A list containing a summary table and a tidygraph object suitable for a network visualisation.
#' @usage count_ngram(
#'  df,
#'  text_var = Message,
#'  n = 2,
#'  top_n = 50,
#'  min_freq = 10,
#'  distinct = FALSE,
#'  hashtags = FALSE,
#'  mentions = FALSE,
#'  clean_text  = FALSE,
#'  remove_stops = TRUE,
#'  tolower = TRUE, 
#'  ...
#' )
#' @importFrom magrittr "%>%"
#' @export
count_ngram_temp <- function(df,
                        text_var = Message,
                        n = 2,
                        top_n = 50,
                        min_freq = 10,
                        distinct = FALSE,
                        hashtags = FALSE,
                        mentions = FALSE,
                        clean_text = FALSE,
                        remove_stops = TRUE,
                        tolower = TRUE,
                        ...) {
  
  
  # Tidy evaluate supplied text variable
  text_quo <- rlang::enquo(text_var)
  text_sym <- rlang::ensym(text_var)
  
  # Loading the new stop words list
  stopwords <- ParseR::stopwords
  
  # use ParseR's clean_text function if the user asks
  if (clean_text) {
    clean_df <- df %>%
      ParseR::clean_text(text_var = !!text_sym, ...) %>%
      dplyr::select(!!text_sym) %>%
      dplyr::mutate(.document = dplyr::row_number()) # add a document ID column for distinct
  } else {
    clean_df <- df %>% dplyr::select(!!text_sym) %>%  #keep the only column we need
      dplyr::mutate(.document = dplyr::row_number()) # add a document ID column for distinct
  }
  
  
  
  # Avoid any NA strings
  clean_df <- clean_df %>%
    dplyr::filter(!is.na(!!text_sym))
  
  # Make edges df
  ngrams <- clean_df %>%
    tidytext::unnest_tokens(ngram,
                            !!text_sym,
                            token = "ngrams",
                            n = n,
                            format = "text",
                            to_lower = tolower
    ) %>%
    dplyr::filter(!is.na(ngram)) # Make sure no NA values here or tidygraph will fail later
  
  word_names <- paste0("word", 1:n)
  
  # Separate the unnested ngrams into n (length of n-gram)columns
  edges_df <- ngrams %>%
    tidyr::separate_wider_delim(ngram, " ", names = word_names)
  
  if(remove_stops) {
    edges_df <- edges_df %>%
      dplyr::filter_at(.vars = word_names, ~ !. %in% stopwords$stopwords & !is.na(.)) 
  }
  
  # Remove copies
  if (distinct) {
    edges_df <- edges_df %>% dplyr::distinct()
  }
  
  edges_df <- edges_df %>%
    dplyr::count(!!!dplyr::syms(word_names), sort = TRUE, name = "ngram_freq") %>%
    dplyr::filter(ngram_freq >= min_freq) %>%
    dplyr::slice_max(n = top_n, order_by = ngram_freq)
  
  # Get all ngrams
  ngram_words <- edges_df %>%
    dplyr::select(-ngram_freq) %>%
    unlist() %>%
    unique()
  
  # Builds nodes df (single words) 
  nodes_df <- clean_df %>%
    tidytext::unnest_tokens(word, !!text_sym, token = "words", format = "text", to_lower = tolower)
  
  if (distinct) {
    nodes_df <- nodes_df %>% dplyr::distinct()
  }
  
  if (remove_stops) {
    nodes_df <- nodes_df %>%
      dplyr::filter(!word %in% stopwords$stopwords)
  }
  
  nodes_df <- nodes_df %>%
    dplyr::filter(!is.na(word)) %>%
    dplyr::count(word, sort = TRUE, name = "word_freq") %>%
    dplyr::filter(word %in% ngram_words)
  
  # Join together for output
  tidy_ngram <- tidygraph::tbl_graph(nodes = nodes_df, edges = edges_df)
  
  # Output list
  return(list("viz" = tidy_ngram, "view" = edges_df))
}

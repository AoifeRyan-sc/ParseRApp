samy_palette <- c("#C6492A", "#E2AC5C", "#7AAE67", "#3B589E")

create_terms_table <- function(terms, df, group_var, message_var){
  
  if (group_var != "none"){
    df_table <- purrr::map_dfr(terms, function(term){
      df %>%
        dplyr::filter(stringr::str_detect(clean_text, stringr::fixed(term, ignore_case = FALSE))) %>%
        dplyr::mutate(Term = term) %>%
        dplyr::select(Term, tidyselect::all_of(c(Group = group_var, message_var)), clean_text) %>%
        dplyr::collect()
    }) 
  } else {
    df_table <- purrr::map_dfr(terms, function(term){
      tmp <- df %>%
        dplyr::filter(stringr::str_detect(clean_text, stringr::fixed(term, ignore_case = FALSE))) %>%
        dplyr::mutate(Term = term) %>%
        dplyr::select(Term, tidyselect::all_of(message_var), clean_text) %>%
        dplyr::collect()
    }) 
  } 
  
  return(df_table)
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
  text_var <- ifelse("text_lemma" %in% colnames(df), "text_lemma", "clean_text")
  bigram_df <- purrr::map_dfr(bigram_pairs, function(bigram_pairs) {
    df %>%
      dplyr::filter(stringr::str_detect(!!rlang::sym(text_var), stringr::fixed(bigram_pairs, ignore_case = FALSE))) %>%
      dplyr::mutate(bigram_pairs = bigram_pairs) %>%
      dplyr::select(bigram_pairs, tidyselect::all_of(message_var), text_var) %>%
      dplyr::collect()
  })
    
  
  return(bigram_df)
}

# Personalised functions ----
count_ngram_app <- function(df, text_var, top_n, min_freq){
  ngram_data <- ParseR::count_ngram(
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

get_gt_terms <- function(graph_obj){
  
  graph_data <- graph_obj$data
  group_term_node_size <- sort(graph_data$size, decreasing = T)[1]
  group_terms <- graph_data[graph_data$size == group_term_node_size, ][["node_name"]]
  graph_terms <- graph_data[!graph_data$node_name %in% group_terms,][["node_name"]]
  
  return(graph_terms)
}

# wlo functions -----
get_wlo_terms <- function(wlo_view){

  wlo_terms <- unique(wlo_view$word)
  
  return(wlo_terms)
} 

# top terms ----
make_top_terms <- function(df, n_terms, group_var = NULL, group = F){
  
  text_var <- ifelse("text_lemma" %in% colnames(df), "text_lemma", "clean_text")
  
  all_terms <- df %>% 
    tidytext::unnest_tokens(
      output = word, 
      input = !!rlang::sym(text_var),
      token = "words", 
      to_lower = FALSE)
  
  if (group){
    all_terms <- all_terms %>% dplyr::group_by({{ group_var }})
  }
    
  top_terms <- all_terms %>%
    dplyr::count(word) %>%
    dplyr::slice_max(n = n_terms, order_by = n, with_ties = F) %>%
    dplyr::ungroup()
    
}

top_terms_theme <- function(){
  theme <- ggplot2::theme_minimal() +
    ggplot2::theme(title = ggplot2::element_text(size = 16),
                   text = ggplot2::element_text(size = 10),
                   strip.text = ggplot2::element_text(size = 14),
                   axis.title.y = ggplot2::element_text(angle = 0, vjust = 0.5),
                   axis.title.x = ggplot2::element_text(size = 14))
  
  return(theme)
}

viz_top_terms_group <- function(top_terms, type = c("lollipops", "bars"), nrow = 1, group_var){
  type <- match.arg(type)
  
  plot <- top_terms %>%
    dplyr::mutate(group_word = paste0({{ group_var }}, "_", word),
                  group_word = forcats::fct_reorder(group_word, n)) %>%
    ggplot2::ggplot(ggplot2::aes(x = group_word, y = n, 
                                 fill = {{ group_var }}, colour = {{ group_var }}
                                 )) +
    ggplot2::scale_x_discrete(name= "Term", labels=function(x) sub('^.*_(.*)$', '\\1', x))
    
    if (type == "lollipops"){
      
      plot <- plot + 
        ggplot2::geom_segment(ggplot2::aes(x = group_word, xend = group_word,
                                         y = 0, yend = n),
                            show.legend = FALSE) +
        ggplot2::geom_point(size = 3,
                          shape = 21,
                          show.legend = FALSE)
  } else {
    plot <- plot + ggplot2::geom_col(show.legend = F)
  }
  
  plot <- plot +
    ggplot2::facet_wrap(ggplot2::vars({{ group_var }}),
                        scales = "free",
                        nrow = nrow) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(name = "Number of occurences") +
    top_terms_theme()
  
  
  return(plot)
    
}

viz_top_terms_no_group <- function(top_terms, type = c("lollipops", "bars"), nrow = 1){
  type <- match.arg(type)

  plot <- top_terms %>%
    dplyr::mutate(word = forcats::fct_reorder(word, n, .desc = F)) %>%
    ggplot2::ggplot(ggplot2::aes(x = word, y = n))
  
  if (type == "lollipops"){
    plot <- plot + 
      ggplot2::geom_segment(ggplot2::aes(x = word, xend = word,
                                         y = 0, yend = n),
                            show.legend = FALSE) +
      ggplot2::geom_point(size = 3,
                          shape = 21,
                          show.legend = FALSE)
  } else {
    plot <- plot + ggplot2::geom_col(show.legend = F)
  }
  
  plot <- plot +
    ggplot2::coord_flip() +
    tidytext::scale_x_reordered("Term") +
    ggplot2::scale_y_continuous(name = "Number of occurences") +
    top_terms_theme()
  
  
  return(plot)
  
}

get_tt_terms <- function(top_terms){
  
  tt_terms <- unique(top_terms$word)
  
  return(tt_terms)
} 

# data handling ---- 

clear_reactives <- function(r){
  num_reactives <- length(names(r))
  
  for (name in names(r)) {
    r[[name]] <- NULL
  }
}

load_data <- function(r){
  
  ext <- tools::file_ext(r$file_path)
  
  validate(need(ext %in% c("csv", "xlsx", "rds"), "Please upload a csv, xlsx, or rds file"))
  
  r$master_df <- switch(ext,
                      csv = read.csv(r$file_path),
                      xlsx = readxl::read_xlsx(r$file_path),
                      rds = readRDS(r$file_path)) # maybe should use duckdb to read data in too
}

clean_df <- function(df, message_var, duckdb = F){
  
  df <- df %>%
    dplyr::mutate(clean_text = message_var, .after = message_var) %>%
    ParseR::clean_text(
      text_var = clean_text,
      tolower = T, # should make some of this customisable
      remove_mentions = T,
      remove_punctuation = T,
      remove_digits = T,
      in_parallel = F # be aware if we are deploying this - does this work with duckdb?
    ) # have seen analyst dfs with all blank space
  
  if (duckdb){
    df <- df %>%
      dplyr::collect() %>%
      dplyr::mutate(clean_text = tm::removeWords(clean_text, tm::stopwords(kind = "SMART"))) %>%
      LimpiaR::limpiar_spaces(clean_text) %>%
      dplyr::filter(!is.na(clean_text))  %>%
      dplyr::filter(!grepl("^\\s*$", clean_text))
  } else {
    df <- df %>%
      dplyr::mutate(clean_text = tm::removeWords(clean_text, tm::stopwords(kind = "SMART"))) %>%
      LimpiaR::limpiar_spaces(clean_text) %>%
      dplyr::filter(!is.na(clean_text))  %>%
      dplyr::filter(!grepl("^\\s*$", clean_text))
  }
  
  return(df)
}

lemmatise_df <- function(df, language = c("english", "spanish"), duckdb = F){
  
  model = LimpiaR::limpiar_pos_import_model(language = language)
  df <- df %>% dplyr::mutate(.docid = dplyr::row_number())
  print(nrow(df))
  ud_annotate <- LimpiaR::limpiar_pos_annotate(
    data = df,
    text_var = clean_text,
    id_var = .docid,
    pos_model = model,
    in_parallel = F, # would it cause complications down the line?
    dependency_parse = F,
    update_progress = 1000
    )
  
  df_lemma <- ud_annotate %>%
    dplyr::group_by(.docid) %>%
    dplyr::summarise(text_lemma = paste0(lemma, collapse = " ")) %>%
    dplyr::left_join(df, by = ".docid") %>%
    dplyr::select(-.docid) %>%
    dplyr::relocate(text_lemma, .after = clean_text)

  return(df_lemma)
}

process_df <- function(df, message_var, con, df_con_name, lemmatise = T, language = c("english", "spanish"), duckdb = F){
  
  shinybusy::show_modal_spinner(text = "Cleaning text, please wait...", spin = "circle")
  
  message("cleaning data & removing empty entries...")
  df_clean <- clean_df(df = df, message_var = message_var, duckdb = duckdb)
  
  if (lemmatise){
    message("lemmatising")
    df_clean <- lemmatise_df(df = df_clean, language = language, duckdb = F)
  }
  
  
  make_duckdb(df = df_clean, con = con, name = df_con_name)
  con_df <- dplyr::tbl(con, df_con_name)

  shinybusy::remove_modal_spinner()
  shiny::showNotification("Text cleaning completed!", type = "message")

  return(con_df)
}

make_duckdb <- function(df, con, name){
  duckdb::dbWriteTable(
    conn = con,
    name = name,
    df,
    overwrite = TRUE
  )
}
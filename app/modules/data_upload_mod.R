dataUploadUi <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::br(),
    # shiny::tags$head(
    #   # Ensure Font Awesome is included
    #   shiny::tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css")
    # ),
    bslib::accordion(
      bslib::accordion_panel(
        "Upload File",
        shiny::uiOutput(ns("file_upload_display"))
      ),
    # shiny::conditionalPanel( # should try as a renderUi to se if that lets accordion open
    #   condition = "output.file_uploaded == 1", ns = ns,
    #   bslib::accordion_panel(
    #     "Column Settings", icon = bsicons::bs_icon("file-earmark-spreadsheet"), open = TRUE,
    #   )
    #   )
  )
  )
}

dataUploadServer <- function(id, r){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$file_upload_display <- renderUI({
      shiny::fileInput(ns("file_upload"), label = NULL, multiple = FALSE) # add some widgets?  
    }) # fileInput display
    
    shiny::observeEvent(input$file_upload, {
      con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
      ext <- tools::file_ext(input$file_upload$datapath)
      file_path <- input$file_upload$datapath
      
      req(ext)
      validate(need(ext %in% c("csv", "xlsx", "rds"), "Please upload a csv, xlsx, or rds file"))
      
      # DBI::dbExecute(con, glue::glue("INSTALL httpfs; LOAD httpfs;"))  # optional, if using remote files
      # DBI::dbExecute(con, glue::glue("CREATE VIEW uploaded AS SELECT * FROM read_csv_auto('{file_path}')"))
      
      # Now you can do efficient queries on it
      # r$df <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n_rows FROM uploaded")
      
      r$df <- switch(ext,
                     csv = read.csv(input$file_upload$datapath),
                     xlsx = readxl::read_xlsx(input$file_upload$datapath),
                     rds = readRDS(input$file_upload$datapath)) %>%
        as_duckplyr_df()
     
      if (nrow(r$df) > 50000){
        r$df <- NULL
        
        shinyalert::shinyalert("File must have less than 50k rows.",
                     closeOnEsc = TRUE,
                     closeOnClickOutside = FALSE,
                     type = "warning")
        output$file_upload_display <- renderUI({
          shiny::fileInput(ns("file_upload"), label = NULL, multiple = FALSE) # add some widgets?
        })
      } else {
        shiny::showModal(shiny::modalDialog(
          title = "Select a Column",
          select_input_with_tooltip(id = ns("text_column"), title = "Text Column*",
                                    icon_info = "The name of the column with the text you want to analyse",
                                    choice_list = colnames(r$df)),
          select_input_with_tooltip(id = ns("author_column"), title = "Author Column*",
                                    icon_info = "The name of the author column",
                                    choice_list = colnames(r$df)),
          select_input_with_tooltip(id = ns("date_column"), title = "Date Column*",
                                    icon_info = "The name of the date column",
                                    choice_list = colnames(r$df)),
          footer = shiny::actionButton(ns("confirm_text_col"), "Go!")
        ))
      }
      
    }) # deal with uploaded file
    
  
    # shiny::observeEvent(input$file_upload, {
    #   message("sapplying")
    #   col_factor <- sapply(r$df, detect_factor)
    #   message("mapplying")
    #   df_mat <- mapply(convert_to_factor, r$df, col_factor, SIMPLIFY = F)
    #   r$df <- as.data.frame(df_mat)
    #   message("finished")
    # })

    output$file_uploaded <- shiny::reactive({
      return(!is.null(r$df))
      # return(is.null(r$df)) # need to remove this - just so I don't have to keep uploading
    }) # logic for conditional panel in ui
    shiny::outputOptions(output, "file_uploaded", suspendWhenHidden = FALSE)

    shiny::observeEvent(input$confirm_text_col, { # should this be done in a separate session? Am I just inviting problems if we try to put it on docker or anything
      shiny::removeModal()
      
      shinybusy::show_modal_spinner(text = "Cleaning text, please wait...", spin = "circle")
      
      r$text_var <- input$text_column
      r$df <- r$df %>%
        mutate(clean_text = r$text_var)
      r$date_var <- input$date_column
      r$sender_var <- input$author_column
      
      r$df <- r$df %>%
        collect() %>%
        ParseR::clean_text(
          text_var = clean_text,
          tolower = T, # should make some of this customisable
          remove_mentions = T,
          remove_punctuation = T,
          remove_digits = T,
          in_parallel = F # be aware if we are deploying this - does this work with duckdb?
        ) %>%
        as_duckdb_tibble()
      
      r$df <- r$df %>%
        collect() %>%
        mutate(clean_text = tm::removeWords(clean_text, tm::stopwords((kind = "SMART")))) %>%
        as_duckdb_tibble()
      
      shinybusy::remove_modal_spinner()
      shiny::showNotification("Text cleaning completed!", type = "message")
    }) # clean text - maybe need to change
    
    
  })
  
}

# me testing timnings - not sure I should bother with this? ----
# start <- Sys.time()
# 
# tmp_duckplyr <- tmp %>% as_duckdb_tibble()
# test <- tmp_duckplyr %>%
#   mutate(clean_text = Message,
#          clean_text = tm::removeWords(clean_text, tm::stopwords((kind = "SMART")))) 
# 
# timer_duckplyr <- Sys.time() - start
# 
# start <- Sys.time()
# 
# test <- tmp %>%
#   mutate(clean_text = Message,
#          clean_text = tm::removeWords(clean_text, tm::stopwords((kind = "SMART")))) 
# timer_dplyr <- Sys.time() - start
# 
# 
# start <- Sys.time()
# # tmp_duckplyr <- tmp %>% as_duckdb_tibble()
# test <- tbl(con, "test_df") %>%
#   mutate(sentiment = tolower(Sentiment)) |> 
#   summarise(
#     n = n(),
#     median_rt = median(`Sender Followers Count`, na.rm = TRUE),
#     mean_rt = mean(`Sender Followers Count`, na.rm = TRUE),
#     max_rt = max(`Sender Followers Count`, na.rm = TRUE),
#     .by = SenderScreenName
#   ) |> 
#   arrange(desc(median_rt)) |> 
#   filter(n > 10)
# 
# timer_duckplyr <- Sys.time() - start
# 
# start <- Sys.time()
# 
# test <- tmp %>%
#   mutate(sentiment = tolower(Sentiment)) |> 
#   summarise(
#     n = n(),
#     median_rt = median(`Sender Followers Count`, na.rm = TRUE),
#     mean_rt = mean(`Sender Followers Count`, na.rm = TRUE),
#     max_rt = max(`Sender Followers Count`, na.rm = TRUE),
#     .by = SenderScreenName
#   ) |> 
#   arrange(desc(median_rt)) |> 
#   filter(n > 10)
# timer_dplyr <- Sys.time() - start

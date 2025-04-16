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
      )
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
      
      ext <- tools::file_ext(input$file_upload$datapath)
      file_path <- input$file_upload$datapath
      
      req(ext)
      validate(need(ext %in% c("csv", "xlsx", "rds"), "Please upload a csv, xlsx, or rds file"))
      
      master_df <- switch(ext,
                     csv = read.csv(input$file_upload$datapath),
                     xlsx = readxl::read_xlsx(input$file_upload$datapath),
                     rds = readRDS(input$file_upload$datapath)) # maybe should use duckdb to read data in too
     
      if (nrow(master_df) > 50000){
        r$df <- NULL # and maybe close connection?
        
        file_size_logic(file = F)
        
        output$file_upload_display <- renderUI({
          shiny::fileInput(ns("file_upload"), label = NULL, multiple = FALSE) # add some widgets?
        })
      } else {
        
        file_size_logic(file = T)
        
        r$con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
        make_duckdb(df = master_df, con = r$con, name = "master_df")
        r$df <- dplyr::tbl(r$con, "master_df")
        
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
    }) # logic for conditional panel in ui
    shiny::outputOptions(output, "file_uploaded", suspendWhenHidden = FALSE)

    shiny::observeEvent(input$confirm_text_col, { # should this be done in a separate session? Am I just inviting problems if we try to put it on docker or anything
      shiny::removeModal()
      
      shinybusy::show_modal_spinner(text = "Cleaning text, please wait...", spin = "circle")
      
      r$text_var <- input$text_column
      r$date_var <- input$date_column
      r$sender_var <- input$author_column
      
      df_clean <- clean_df(df = r$df, message_var = rlang::sym(r$text_var), duckdb = T)
      
      make_duckdb(df = df_clean, con = r$con, name = "master_df")
      r$df <- dplyr::tbl(r$con, "master_df")
      
      shinybusy::remove_modal_spinner()
      shiny::showNotification("Text cleaning completed!", type = "message")
    }) # clean text - maybe need to change
    
    
  })
  
}

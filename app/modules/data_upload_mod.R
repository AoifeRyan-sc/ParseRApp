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
      shiny::br(),
      shiny::uiOutput(ns("change_columns_button"))
  )
  )
}

dataUploadServer <- function(id, r){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$file_upload_display <- renderUI({
      shiny::fileInput(ns("file_upload"), label = NULL, multiple = FALSE) 
    }) # default fileInput display
    
    shiny::observeEvent(input$file_upload, {
      
      clear_reactives(r)
      r$file_path <- input$file_upload$datapath
      load_data(r)
     
      if (nrow(r$master_df) > 50000){
        r$df <- NULL # and maybe close connection?
        
        file_size_logic(file = F, df = r$master_df, ns = ns)
        
        output$file_upload_display <- renderUI({
          shiny::fileInput(ns("file_upload"), label = NULL, multiple = FALSE) # add some widgets?
        })
      } else {
        
        file_size_logic(file = T, df = r$master_df, ns = ns)
        
        r$con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
        make_duckdb(df = r$master_df, con = r$con, name = "master_df")
        r$df <- dplyr::tbl(r$con, "master_df")
        
      } # what to do after data upload
      
    }) # deal with uploaded file
  
    # shiny::observeEvent(input$file_upload, {
    #   message("sapplying")
    #   col_factor <- sapply(r$df, detect_factor)
    #   message("mapplying")
    #   df_mat <- mapply(convert_to_factor, r$df, col_factor, SIMPLIFY = F)
    #   r$df <- as.data.frame(df_mat)
    #   message("finished")
    # })

    shiny::observeEvent(input$confirm_input_cols, { 
      
      if (shiny::isTruthy(input$text_column)){
        shiny::removeModal()
        
        r$text_var <- input$text_column
        r$date_var <- input$date_column
        r$sender_var <- input$author_column
        
        if (!tolower(r$text_var) %in% c("message", "text")){
          col_check(check_var = r$text_var, correct_var = "message", ns = ns)  
        } 
        # else if (isTruthy(r$sender_var) &!tolower(r$date_var) %in% c("screen_name", "screen name", "sender screen name", "sender_screen_name")){
        #   col_check(check_var = r$sender_var, correct_var = "author", ns = ns)
        # } else if (isTruthy(r$date_var) &!tolower(r$date_var) %in% c("date", "created time", "created_time", "createdtime")){
        #   col_check(check_var = r$date_var, correct_var = "date", ns = ns)
        # }
        
        else {
          shinybusy::show_modal_spinner(text = "Cleaning text, please wait...", spin = "circle")

          df_clean <- process_df(df = r$df, message_var = rlang::sym(r$text_var), duckdb = T)

          make_duckdb(df = df_clean, con = r$con, name = "master_df")
          r$df <- dplyr::tbl(r$con, "master_df")

          shinybusy::remove_modal_spinner()
          shiny::showNotification("Text cleaning completed!", type = "message")
        }
      
        } else {

          output$text_col_missing_error <- shiny::renderUI({
            missing_input_error("missing-selection-error", "Please select a text column")
          })
        }
      
    }) # clean text - maybe need to change
    
    shiny::observeEvent(input$confirm_var , {
      shiny::removeModal()
      
      r$text_var <- input$text_column
      r$date_var <- input$date_column
      r$sender_var <- input$author_column
      
      shinybusy::show_modal_spinner(text = "Cleaning text, please wait...", spin = "circle")
      
      df_clean <- process_df(df = r$df, message_var = rlang::sym(r$text_var), duckdb = T)
      
      make_duckdb(df = df_clean, con = r$con, name = "master_df")
      r$df <- dplyr::tbl(r$con, "master_df")
      
      shinybusy::remove_modal_spinner()
      shiny::showNotification("Text cleaning completed!", type = "message")
    })
    
    shiny::observeEvent(input$reselect_var, {
      file_size_logic(file = T, df = r$master_df, ns = ns)
    })
    
    output$change_columns_button <- shiny::renderUI({
      req(r$df)
      shiny::actionButton(ns("reset_columns"), 
                          "Change Column Selections",
                          icon = shiny::icon("arrow-rotate-right", lib = "font-awesome"),
                          # class = "btn-info"
                          )
    })
    
    shiny::observeEvent(input$reset_columns, {
      file_size_logic(file = T, df = r$master_df, ns = ns)
    })
    
    shiny::observeEvent(input$restart_app, {
      shiny::removeModal()
    })
    
    
  })
  
}

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
        shiny::fileInput(ns("file_upload"), NULL , multiple = FALSE), # add some widgets?  
      ),
    shiny::conditionalPanel(
      condition = "output.file_uploaded == 1", ns = ns,
      bslib::accordion_panel(
        "Column Settings", icon = bsicons::bs_icon("file-earmark-spreadsheet"), open = TRUE,
        select_input_with_tooltip(id = ns("text_column"), title = "Text Column*", "The name of the column with the text you want to analyse"),
        select_input_with_tooltip(id = ns("url_column"), title = "URL Column", "The name of the column with the post url"),
        select_input_with_tooltip(id = ns("display_columns"), multiple_selections = TRUE, title = "Display Columns", "The columns you want to display in the Uploaded Data table"),
      )
      )
  )
  )
}

dataUploadServer <- function(id, r){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    shiny::observe({
      ext <- tools::file_ext(input$file_upload$datapath)
      
      req(ext)
      validate(need(ext %in% c("csv", "xlsx", "rds"), "Please upload a csv, xlsx, or rds file"))
      
      r$df <- switch(ext,
                     csv = read.csv(input$file_upload$datapath),
                     xlsx = readxl::read_xlsx(input$file_upload$datapath),
                     rds = readRDS(input$file_upload$datapath))
    })
    
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
    })
    shiny::outputOptions(output, "file_uploaded", suspendWhenHidden = FALSE)
    
    shiny::observe({
      req(r$df)
      shiny::updateSelectizeInput(session = session, "text_column", choices = colnames(r$df), selected = NULL)
      shiny::updateSelectizeInput(session = session, "url_column", choices = colnames(r$df), selected = NULL)
      shiny::updateSelectizeInput(session = session, "display_columns", choices = colnames(r$df), selected = NULL)
    })
    
    
    shiny::observe({
      req(r$df)
      r$text_var <- input$text_column
      r$display_var <- input$display_columns
    })
    
    # shiny::observe({
    #   req(input$bigram_min_freq, input$bigram_top_n)
    #   r$bigram_min_freq <- input$bigram_min_freq
    #   r$bigram_top_n <- input$bigram_top_n
    #   if (!is.null(input$url_column)){
    #     r$url_column <- input$url_columns
    #   }
    #   # print("min freq input: ", input$bigram_min_freq)
    # })
    
  })
  
}


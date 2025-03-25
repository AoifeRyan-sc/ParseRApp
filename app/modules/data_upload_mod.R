dataUploadUi <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::br(),
    # shiny::tags$head(
    #   # Ensure Font Awesome is included
    #   shiny::tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css")
    # ),
    shiny::fileInput(ns("file_upload"), "Upload File", multiple = FALSE), # add some widgets? 
    shiny::conditionalPanel(
      condition = "output.file_uploaded == 1", ns = ns,
      bslib::accordion(
        bslib::accordion_panel(
          "Column Settings", icon = bsicons::bs_icon("share-fill"), open = TRUE,
          shiny::selectizeInput(
            ns("text_column"), "Text Column:", choices = list(),
            options = list(
              placeholder = 'Please select an option below',
              onInitialize = I('function() { this.setValue(""); }')
            )),
          shiny::selectizeInput(
            ns("url_column"), "URL Column:", choices = list(),
            options = list(
              placeholder = 'Please select an option below',
              onInitialize = I('function() { this.setValue(""); }')
            ))
        )
      ),
    ),
    shiny::conditionalPanel(
      condition = "input.text_column", ns = ns,
      shiny::numericInput(ns("bigram_min_freq"), "Minimum frequency", value = 5),
      shiny::numericInput(ns("bigram_top_n"), "Number of bigrams:", value = 25)
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
      
      browser()
    })
    
    output$file_uploaded <- shiny::reactive({
      return(!is.null(r$df))
    })
    shiny::outputOptions(output, "file_uploaded", suspendWhenHidden = FALSE)
    
    shiny::observe({
      req(r$df)
      shiny::updateSelectizeInput(session = session, "text_column", choices = colnames(r$df), selected = NULL)
      shiny::updateSelectizeInput(session = session, "url_column", choices = colnames(r$df), selected = NULL)
    })
    
    
    shiny::observe({
      req(r$df)
      r$text_var <- input$text_column
      print(r$text_var)
      # r$bigram_min_f <- input$bigram_min_freq
      # print(r$bigram_min_freq)
      
      # print("top n r: ", r$bigram_top_n)
      # print("top n input: ", input$bigram_top_n)
    })
    
    shiny::observe({
      req(input$bigram_min_freq, input$bigram_top_n)
      r$bigram_min_freq <- input$bigram_min_freq
      r$bigram_top_n <- input$bigram_top_n
      if (!is.null(input$url_column)){
        print(input$url_col)
        r$url_column <- input$url_columns
      }
      # print("min freq input: ", input$bigram_min_freq)
    })
    
    })
    
}


dataUploadUi <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::br(),
    # shiny::tags$head(
    #   # Ensure Font Awesome is included
    #   shiny::tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css")
    # ),
    shiny::fileInput(ns("file_upload"), "Upload File", multiple = FALSE), # add some widgets? 
    shiny::selectizeInput(
      ns("text_column"), "Text Column:", choices = list(),
      options = list(
        placeholder = 'Please select an option below',
        onInitialize = I('function() { this.setValue(""); }')
      )),
    # shiny::numericInput(ns("testing"), "testing", value = 12),
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
    
    # shiny::observe({
    #   message("testing: ", input$testing)
    #   r$testing <- input$testing
    #   message("testing r: ", r$testing)
    #   
    #   message("min_freq: ", input$bigram_min_freq)
    #   r$bigram_min_freq <- input$bigram_min_freq
    #   message("min_freq r: ", r$bigram_min_freq)
    # })
    
    shiny::observe({

      ext <- tools::file_ext(input$file_upload$datapath)
      
      req(ext)
      validate(need(ext %in% c("csv", "xlsx", "rds"), "Please upload a csv, xlsx, or rds file"))
      
      r$df <- switch(ext,
                     csv = read.csv(input$file_upload$datapath),
                     xlsx = readxl::read_xlsx(input$file_upload$datapath),
                     rds = readRDS(input$file_upload$datapath))
    })
    
    
    observe({
      req(r$df)
      shiny::updateSelectizeInput(session = session, "text_column", choices = colnames(r$df), selected = NULL)
    
    })
    
    
    observe({
      req(r$df)
      r$text_var <- input$text_column
      print(r$text_var)
      # r$bigram_min_f <- input$bigram_min_freq
      # print(r$bigram_min_freq)
      
      # print("top n r: ", r$bigram_top_n)
      # print("top n input: ", input$bigram_top_n)
    })
    
    observe({
      req(input$bigram_min_freq, input$bigram_top_n)
      r$bigram_min_freq <- input$bigram_min_freq
      r$bigram_top_n <- input$bigram_top_n
      # print("min freq input: ", input$bigram_min_freq)
    })
    
    })
    
}


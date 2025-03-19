dataUploadUi <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::br(),
    # shiny::tags$head(
    #   # Ensure Font Awesome is included
    #   shiny::tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css")
    # ),
    shiny::fileInput(ns("file_upload"), "Upload File", multiple = FALSE), # add some widgets? 
    shiny::selectInput(ns("text_column"), "Text Column:", choices = "")
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
    
    
    observe({
      req(r$df)
      shiny::updateSelectInput(session = session, "text_column", choices = colnames(r$df), selected = NULL)
    
    })
    
    
    observe({
      r$text_var <- input$text_column
    })
    
      
    })
    
}


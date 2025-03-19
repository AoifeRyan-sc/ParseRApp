library(shiny)
library(plotly)
library(gridlayout)
library(bslib)
library(DT)


ui <- grid_page(
  layout = c(
    "header header header   header  ",
    "area56 table  bluePlot bluePlot",
    "area7  area5  area5    plotly  ",
    ".      area5  area5    plotly  "
  ),
  row_sizes = c(
    "45px",
    "0.99fr",
    "1.05fr",
    "0fr"
  ),
  col_sizes = c(
    "120px",
    "1fr",
    "0.59fr",
    "1.41fr"
  ),
  gap_size = "1rem",
  grid_card_text(
    area = "header",
    content = "Geysers!",
    alignment = "start",
    is_title = FALSE
  ),
  grid_card(
    area = "table",
    card_header("Table"),
    card_body(DTOutput(outputId = "myTable", width = "100%"))
  ),
  grid_card_plot(area = "bluePlot"),
  grid_card(
    area = "plotly",
    card_header("Interactive Plot"),
    card_body(
      plotlyOutput(
        outputId = "distPlot",
        width = "100%",
        height = "100%"
      )
    )
  ),
  grid_card(
    area = "area5",
    card_body(
      sliderInput(
        inputId = "bins",
        label = "Number of Bins",
        min = 12,
        max = 100,
        value = 30,
        width = "100%"
      )
    )
  ),
  grid_card(
    area = "area56",
    card_body(tabsetPanel(nav_panel(title = "Empty Tab")))
  ),
  grid_card(
    area = "area7",
    card_body(uiOutput(outputId = "dynamicUiOutput"))
  )
)


server <- function(input, output) {
   
  output$distPlot <- renderPlotly({
    # generate bins based on input$bins from ui.R
    plot_ly(x = ~ faithful[, 2], type = "histogram")
  })
  
  output$bluePlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
  
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = "steelblue", border = "white")
  })
  
  output$myTable <- renderDT({
    head(faithful, input$numRows)
  })
}

shinyApp(ui, server)
  


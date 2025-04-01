valueBoxUi <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
      bslib::value_box(
        title = "No. Data Points",
        value = shiny::textOutput(ns("n_rows")),
        showcase = bsicons::bs_icon("bar-chart"),
        theme = "purple"
      ),
      bslib::value_box(
        title = "No. Authors",
        value = shiny::textOutput(ns("n_authors")),
        showcase = bsicons::bs_icon("graph-up"),
        theme = "teal"
      ),
      bslib::value_box(
        title = "No. Weeks",
        value = shiny::textOutput(ns("n_weeks")),
        # showcase = bsicsons::bs_icon("pie-chart"),
        showcase = plotly::plotlyOutput(ns("vot_chart")),
        # showcase = vot_plot,
        theme = "pink",
        full_screen = TRUE
      )
    )
}

valueBoxServer <- function(id, r){
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$n_rows <- shiny::renderText({
      nrow(r$df)
      })
    output$n_authors <- shiny::renderText({
      req(r$sender_var)
      length(unique(r$df[[r$sender_var]]))
      })
    output$n_weeks <- shiny::renderText({
      req(r$date_var)
      r$time_info <- as.Date(r$df[[r$date_var]])
      length(unique(format(r$time_info, "%Y-%U")))
    })
    output$vot_chart <- plotly::renderPlotly({
    # shiny::observe({
      req(r$time_info)
      message("plotting chart")
      week_df <- data.frame(table(r$time_info))
      colnames(week_df) <- c("Week", "Posts")
      print(head(week_df))
      
      plotly::plot_ly(week_df) %>%
        plotly::add_lines(
            x = ~Week, y = ~Posts,
            color = I("white"), span = I(1),
            fill = 'tozeroy', alpha = 0.2
          ) %>%
        plotly::layout(
            xaxis = list(visible = F, showgrid = F, title = ""),
            yaxis = list(visible = F, showgrid = F, title = ""),
            hovermode = "x",
            margin = list(t = 0, r = 0, l = 0, b = 0),
            font = list(color = "white"),
            paper_bgcolor = "transparent",
            plot_bgcolor = "transparent"
          ) %>%
          plotly::config(displayModeBar = F) %>%
          htmlwidgets::onRender(
            "function(el) {
      el.closest('.bslib-value-box')
        .addEventListener('bslib.card', function(ev) {
          Plotly.relayout(el, {'xaxis.visible': ev.detail.fullScreen});
        })
    }"
          )
        })
    })

}
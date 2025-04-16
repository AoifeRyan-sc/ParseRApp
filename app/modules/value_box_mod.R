valueBoxUi <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
      bslib::value_box(
        title = "No. Data Points",
        value = shiny::textOutput(ns("n_rows")),
        showcase = bsicons::bs_icon("globe"),
        theme = "purple"
      ),
      bslib::value_box(
        title = "No. Authors",
        value = shiny::textOutput(ns("n_authors")),
        showcase = bsicons::bs_icon("person-raised-hand"),
        theme = "teal"
      ),
      bslib::value_box(
        title = "No. Weeks",
        value = shiny::textOutput(ns("n_weeks")),
        showcase = plotly::plotlyOutput(ns("vot_chart")),
        theme = "pink",
        full_screen = TRUE
      )
    )
}

valueBoxServer <- function(id, r){
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$n_rows <- shiny::renderText({
      req(r$con)
      DBI::dbGetQuery(r$con, "SELECT COUNT(*) AS n_rows FROM master_df")$n_rows
      })
    output$n_authors <- shiny::renderText({
      req(r$sender_var)
      length(unique(r$df %>% dplyr::pull(r$sender_var)))
      })
    output$n_weeks <- shiny::renderText({
      req(r$date_var)
      r$time_info <- as.Date(r$df %>% dplyr::pull(r$date_var))
      length(unique(format(r$time_info, "%Y-%U")))
    })
    output$vot_chart <- plotly::renderPlotly({
      req(r$time_info)
      week_df <- data.frame(table(format(r$time_info, "%Y-%U")))
      colnames(week_df) <- c("Week", "Posts")
      
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
          Plotly.relayout(el, {
          'xaxis.visible': ev.detail.fullScreen,
          'yaxis.visible': ev.detail.fullScreen});
        })
    }"
          )
        })
    })

}
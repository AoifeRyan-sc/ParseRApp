card2 <- bslib::card(
  bslib::card_header("Bigram"),
  bslib::card_body(
    dataBigramUi("bigram_panel")
  ),
  style = bslib::css(
    gap ="0.25rem",
    resize = "vertical"
  )
)
card3 <- bslib::card(
  full_screen = TRUE,
  bslib::card_header("Filling content"),
  bslib::card_body(
    class = "p-0",
    shiny::plotOutput("p")
  ),
  style = bslib::css(
    gap ="0.25rem",
    resize = "horizontal"
  )
)

ui <- bslib::page_fluid(
  tags$style(HTML("g.hovertext > path {opacity: .8;}")),

  theme = bslib::bs_theme(
    bootswatch = "sandstone",
    # heading_font = bslib::font_face(family = "Cinzel-SemiBold",
    #                                 src = "fonts/Cinzel-SemiBold.ttf"),
    # base_font = bslib::font_face(family = "Cinzel-Regular",
    # src = "fonts/Cinzel-Regular.ttf")
  ), # maybe should change this

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
    dataUploadUi("data_upload_panel"),
    open = TRUE),
  bslib::layout_column_wrap(
    style = "overflow-y: auto; max-height: 100vh;",
    width = 1,
    height = 300,
    heights_equal = "row",
    dataDisplayUi("data_display_card"),
    bslib::layout_column_wrap(
      width = 1/2,
      heights_equal = "row",
      dataBigramUi("bigram_card"),
      card3
    ),
    bslib::layout_column_wrap(
      width = 1/2,
      heights_equal = "row",
      card3, card3
    ),
  )
  )
)

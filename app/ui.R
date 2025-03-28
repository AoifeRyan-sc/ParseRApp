card2 <- bslib::card(
  bslib::card_header("Bigram"),
  bslib::card_body(
    dataBigramUi("bigram_panel")
  ),
  # style = bslib::css(
  #   gap ="0.25rem",
  #   resize = "vertical"
  # )
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
  ),
  min_height = 500
)

ui <- bslib::page_sidebar(
  tags$style(HTML("g.hovertext > path {opacity: .8;}")),

  theme = bslib::bs_theme(
    bootswatch = "sandstone",
  ),
  shinybusy::add_busy_gif(
    src = "https://jeroen.github.io/images/banana.gif",
    height = 70, width = 70
  ),
  sidebar = bslib::sidebar(
    dataUploadUi("data_upload_panel"),
    open = TRUE),
  bslib::layout_column_wrap(
    width = 1,
    fill = F,
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
      card3,
      groupTermsUi("gt_card")
    ),
    bslib::layout_column_wrap(
      width = 1/2,
      heights_equal = "row",
      wloUi("wlo_card"),
      card3
    ),
  )
)

# heading_font = bslib::font_face(family = "Cinzel-SemiBold",
#                                 src = "fonts/Cinzel-SemiBold.ttf"),
# base_font = bslib::font_face(family = "Cinzel-Regular",
# src = "fonts/Cinzel-Regular.ttf")
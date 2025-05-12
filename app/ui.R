# card3 <- bslib::card(
#   full_screen = TRUE,
#   bslib::card_header("Filling content"),
#   bslib::card_body(
#     class = "p-0",
#     shiny::plotOutput("p")
#   ),
#   style = bslib::css(
#     gap ="0.25rem",
#     resize = "horizontal"
#   ),
#   min_height = 500
# )
# 
ui <- bslib::page_sidebar(
  tags$style(HTML("g.hovertext > path {opacity: .8;}")),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),

  theme = bslib::bs_theme(
    bootswatch = "sandstone",
  ),
  # shinybusy::add_busy_gif(
  #   src = "https://jeroen.github.io/images/banana.gif",
  #   height = 70, width = 70
  # ),
  sidebar = bslib::sidebar(
    dataUploadUi("data_upload_panel"),
    open = TRUE),
  bslib::layout_column_wrap(
    bslib::layout_column_wrap(
      width = 1/3,
      !!!valueBoxUi("value_box_panel"),
    ),
  width = 1,
  fill = F,
  heights_equal = "row",
  dataDisplayUi("data_display_card"),
  bslib::layout_column_wrap(
    width = 1/2,
    heights_equal = "row",
    bigramVizUi("bigram_viz_card"),
    bigramDataUi("bigram_data_card")
  ),
  bslib::layout_column_wrap(
    width = 1/2,
    heights_equal = "row",
    groupTermsDataUi("gt_data_card"),
    groupTermsVizUi("gt_viz_card")
  ),
  bslib::layout_column_wrap(
    width = 1/2,
    heights_equal = "row",
    wloVizUi("wlo_viz_card"),
    wloDataUi("wlo_data_card")
  ),
  bslib::layout_column_wrap(
    width = 1/2,
    heights_equal = "row",
    topTermsVizUi("top_terms_viz_card"),
    topTermsDataUi("top_terms_data_card")
  )
  )
)

# heading_font = bslib::font_face(family = "Cinzel-SemiBold",
#                                 src = "fonts/Cinzel-SemiBold.ttf"),
# base_font = bslib::font_face(family = "Cinzel-Regular",
# src = "fonts/Cinzel-Regular.ttf")
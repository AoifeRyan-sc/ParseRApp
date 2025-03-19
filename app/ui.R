ui <- bslib::page_fillable(
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
    bslib::navset_card_tab(
      bslib::nav_panel(
        "View Data",
        dataDisplayUi("data_display_panel"),
        height = "900px",
        fillable = TRUE,
        fill = TRUE
      ),
      bslib::nav_panel(
        "Bigrams",
        height = "900px",
        fillable = TRUE,
        fill = TRUE
      ),
      bslib::nav_panel(
        "Group Terms Network",
        height = "900px",
        fillable = TRUE,
        fill = TRUE
      )
    )
  )
  
)

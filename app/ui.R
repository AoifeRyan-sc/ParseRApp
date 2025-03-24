
card2 <- bslib::card(
  bslib::card_header("Nothing much here"),
  "This is it.",
  style = bslib::css(
    "--bs-card-border-color" = "var(--bs-pink)",
    "--bs-card-border-width" = "0.12rem",
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
    "--bs-card-border-color" = "var(--bs-pink)",
    "--bs-card-border-width" = "0.12rem",
    gap ="0.25rem",
    resize = "horizontal"
  )
)

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
    bslib::layout_column_wrap(
      width = 1,
      # height = 300,
      # bslib::card(
      #   bslib::card_header("Uploaded Data"),
      #   bslib::card_body(
      #     dataDisplayUi("data_display_panel"),
      #   ),
      #   full_screen = TRUE,
      #   style = bslib::css(
      #     gap ="0.25rem",
      #     resize = "horizontal"
      #   )
      # ),
      heights_equal = "row",
      card2, card2
      # bslib::layout_column_wrap(
      #   width = 1,
      #   heights_equal = "row",
      #   card2, card3
      # )
    )
  )
  # bslib::layout_column_wrap(
  #   bslib::layout_column_wrap(
  #     width = 1,
  #     heights_equal = "row",
  #     style = bslib::css(
  #       resize = "horizontal",
  #       overflow = "auto"
  #     ),
  #     bslib::card(
  #       bslib::card_header("Uploaded Data"),
  #       bslib::card_body(
  #         dataDisplayUi("data_display_panel"),
  #       ),
  #       full_screen = TRUE,
  #       style = bslib::css(
  #         "--bs-card-border-color" = "var(--bs-pink)",
  #         "--bs-card-border-width" = "0.12rem",
  #         gap ="0.25rem"
  #         # Removed resize from here
  #       )
  #     )
  #   ),
    # width = 1/2,
    # height = 300,
    # style = bslib::css(
    #   resize = "horizontal",
    #   overflow = "auto"  # Adding overflow property to handle the resize behavior
    # ),
    # bslib::card(
    #   bslib::card_header("Uploaded Data"),
    #   bslib::card_body(
    #     dataDisplayUi("data_display_panel"),
    #   ),
    #   full_screen = TRUE,
    #   style = bslib::css(
    #     "--bs-card-border-color" = "var(--bs-pink)",
    #     "--bs-card-border-width" = "0.12rem",
    #     gap ="0.25rem"
    #     # Removed resize from here
    #   )
    # ),
    # bslib::layout_column_wrap(
    #   width = 1,
    #   heights_equal = "row",
    #   style = bslib::css(
    #     resize = "horizontal",
    #     overflow = "auto"
    #   ),
    #   card2, card3
    # )
  # )
)

# fluidRow(
#   box(title = "Ingredients",
#       solidHeader = T,
#       width = 4,
#       collapsible = T,
#       div(DT::DTOutput("ing_df"), style = "font-size: 70%;")),
#   box(title = "Macronutrients", solidHeader = T,
#       width = 8, collapsible = T,
#       plotlyOutput("macro_plot"))
# ), # row
# fluidRow(
#   box(title = "Nutrition Table",
#       solidHeader = T,
#       width = 4, 
#       collapsible = T,
#       collapsed = F,
#       tags$p(textOutput("serving", inline = T)),
#       div(DT::DTOutput("nutrient_table"), style = "font-size: 70%;")),
#   box(title = "Minerals", solidHeader = T,
#       width = 8, collapsible = T,
#       plotlyOutput("mineral_plot"))
# ),# row
# fluidRow(
#   box(title = "Vitamins", solidHeader=T,
#       width = 12, collapsible = T,
#       plotlyOutput("vitamin_plot"))
# ) # row
# ) #
# 
# # ----- 
# bslib::navset_card_tab(
#   bslib::nav_panel(
#     "View Data",
#     dataDisplayUi("data_display_panel"),
#     height = "900px",
#     fillable = TRUE,
#     fill = TRUE
#   ),
#   bslib::nav_panel(
#     "Bigrams",
#     height = "900px",
#     fillable = TRUE,
#     fill = TRUE
#   ),
#   bslib::nav_panel(
#     "Group Terms Network",
#     height = "900px",
#     fillable = TRUE,
#     fill = TRUE
#   )
# )
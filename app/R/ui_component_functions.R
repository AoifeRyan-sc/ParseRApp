missing_input_error <- function(id, error_message){
  shiny::div(
    class = "error-container", 
    bsicons::bs_icon("exclamation-circle-fill", class = "error-icon"),
    shiny::div(id = id, 
               class = "error-message", 
               error_message)
  )
}

# modals ----
file_size_logic <- function(file, df, ns){
  
  if (file){
    shiny::showModal(shiny::modalDialog(
      title = "Select a Column",
      select_input_with_tooltip(id = ns("text_column"), title = "Text Column*",
                                icon_info = "The name of the column with the text you want to analyse",
                                choice_list = colnames(df)),
      shiny::uiOutput(ns("text_col_missing_error")),
      select_input_with_tooltip(id = ns("author_column"), title = "Author Column",
                                icon_info = "The name of the author column",
                                choice_list = colnames(df)),
      select_input_with_tooltip(id = ns("date_column"), title = "Date Column",
                                icon_info = "The name of the date column",
                                choice_list = colnames(df)),
      input_switch_with_tooltip(id = ns("lemmatise_text"), title = "Lemmatise Text?", on = T, 
                                icon_info = "Lemmatisation is the process of grouping together different inflected forms of the same word e.g. 'girls' -> 'girl', 'walked' -> 'walk'."),
      shiny::conditionalPanel(
        condition = "input.lemmatise_text == 1", ns = ns,
        shiny::radioButtons(ns("lemma_language"), 
                            label = "Lemmatisation Language",
                            choices = c("English" = "english"),
                                        # , "Spanish" = "spanish"), # not implementing yet
                            selected = "english")
      ),
      footer = shiny::tagList(
        shiny::actionButton(ns("restart_app"), "Go back", class = "btn-danger"),
        shiny::actionButton(ns("confirm_input_cols"), "Go!", class = "btn-success")
      )
    ))
  } else {
    shinyalert::shinyalert("File must have less than 50k rows.",
                           closeOnEsc = TRUE,
                           closeOnClickOutside = FALSE,
                           type = "warning")
  }
}

col_check <- function(check_var, correct_var = c("date", "message", "author"), ns){
  shiny::showModal(shiny::modalDialog(
    title = paste0("Are you sure ", check_var, " is the ", correct_var, " variable you want to analyse?"),
    footer = shiny::tagList(
      shiny::actionButton(ns("reselect_var"), "Go back"),
      shiny::actionButton(ns("confirm_var"), "Yes, continue")
    )
  ))
}

# tooltip layouts ----
select_input_with_tooltip <- function(id, title, icon_info, choice_list = list(), select = NULL, multiple_selections = FALSE){
  
  if (is.null(select)){
    select_widget <- shiny::selectizeInput(
      id, title, choices = choice_list, 
      multiple = multiple_selections,
      options = list(
        placeholder = 'Please select an option below',
        onInitialize = I('function() { this.setValue(""); }')
      ))
  } else {
    select_widget <- shiny::selectizeInput(
      id, title, choices = choice_list, 
      multiple = multiple_selections,
      selected = select)
  }
  
  shiny::div(
    style = "position: relative",
    select_widget,
    shiny::div(
      style = "position: absolute; top: 0; right: 5px;",
      bslib::tooltip(
        bsicons::bs_icon("question-circle-fill"),
        icon_info
      )
    )
  )
}

numeric_input_with_tooltip <- function(id, title, default_value, icon_info){
  
  shiny::div(
    style = "position: relative",
    shiny::numericInput(id, title, value = default_value),
    shiny::div(
      style = "position: absolute; top: 0; right: 5px;",
      bslib::tooltip(
        bsicons::bs_icon("question-circle-fill"),
        icon_info
      )
    )
  )
}

text_input_with_tooltip <- function(id, title, icon_info, placeholder = NULL, value = NULL){
  
  shiny::div(
    style = "position: relative",
    shiny::textInput(id, title, placeholder = placeholder, value = value),
    shiny::div(
      style = "position: absolute; top: 0; right: 5px;",
      bslib::tooltip(
        bsicons::bs_icon("question-circle-fill"),
        icon_info
      )
    )
  )
}

dropdownButton_with_tooltip <- function(..., dropdown_title, icon_info){
  shiny::div(
    style = "position: absolute; top: 3px; right: 15px;",
    bslib::tooltip(
      shinyWidgets::dropdownButton(
        shiny::tags$h3(dropdown_title),
        circle = TRUE,
        size = "sm",
        icon = shiny::icon("gear"),
        width = "200px",
        status = "primary",
        tags$style(HTML("
          .dropdown-toggle::after {
            display: none !important;
          }
        ")),
        ...
      ),
      icon_info
    )
  )
}

saveButton_with_tooltip <- function(..., icon_info){
  shiny::div(
    style = "position: absolute; top: 3px; right: 65px;",
    bslib::tooltip(
      shinyWidgets::dropdownButton(
        label= "",
        circle = TRUE,
        size = "sm",
        icon = shiny::icon("download"),
        width = "200px",
        status = "primary",
        tags$style(HTML("
          .dropdown-toggle::after {
            display: none !important;
          }
        ")),
        ...
      ),
      icon_info
    )
  )
}

input_switch_with_tooltip <- function(id, title, icon_info, on = TRUE){
  shiny::div(
    style = "position: relative",
    bslib::input_switch(id, title, value = on),
    shiny::div(
      style = "position: absolute; top: 0; right: 5px;",
      bslib::tooltip(
        bsicons::bs_icon("question-circle-fill"),
        icon_info
      )
    )
  )
}

save_dropdown <- function(plot_title, ns){
  saveButton_with_tooltip(
    shiny::textInput(ns(paste0(plot_title, "_save_title")), "Save as", value = paste0(plot_title, "")),
    shiny::selectizeInput(ns(paste0(plot_title, "_save_units")), "Units", choices = c("in", "mm"), selected = "in"),
    shiny::numericInput(ns(paste0(plot_title, "_save_width")), label = "Width", value = 6),
    shiny::numericInput(ns(paste0(plot_title, "_save_height")), label = "Height", value = 4),
    shiny::downloadButton(ns(paste0(plot_title, "_save")), "Save", 
                          icon = shiny::icon("download"),
                          class = "btn-outline-primary btn-sm"
                          ),
    icon_info = paste0("Click here to save ", plot_title)
  )
}


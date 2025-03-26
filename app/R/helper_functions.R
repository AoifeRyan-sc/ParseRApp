select_input_with_tooltip <- function(id, title, icon_info, multiple_selections = FALSE){

  shiny::div(
    style = "position: relative",
    shiny::selectizeInput(
      id, title, choices = list(), 
      multiple = multiple_selections,
      options = list(
        placeholder = 'Please select an option below',
        onInitialize = I('function() { this.setValue(""); }')
      )),
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

# not sure if this is important enough to actually do
detect_factor <- function(x, threshold = 0.1) {
  if (is.numeric(x)) {
    return(FALSE)
  }
  unique_ratio <- length(unique(x)) / length(x)
  has_repeats <- any(table(x) > 1)
  
  return(unique_ratio <= threshold | has_repeats)
}

convert_to_factor <- function(x, factor){
  if (factor){
    x <- as.factor(x)
  }
  return(x)
}

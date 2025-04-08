if (!requireNamespace("ParseR", quietly = TRUE)) {
  remotes::install_github("Avery-Island/ParseR",
                          auth_token = Sys.getenv("PARSER_AUTH_KEY"))
}

library(shiny)
library(magrittr)

source("modules/data_upload_mod.R")
source("modules/view_data_mod.R")
source("modules/bigrams_mod.R")
source("modules/group_terms_mod.R")
source("modules/wlo_mod.R")
source("modules/value_box_mod.R")

source("R/helper_functions.R")

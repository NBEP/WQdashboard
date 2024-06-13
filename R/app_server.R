#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # Try to fix the dplyr problem....
  library(dplyr)

  # Add module servers ----
  selected_var <- mod_sidebar_server(
    "sidebar_1",
    selected_tab = reactive({ input$tabset }))
  mod_map_server("map_1", selected_var)
  mod_report_card_server(
    "report_card_1",
    selected_var,
    selected_tab = reactive({ input$tabset }))
  mod_graphs_server("graphs_1", selected_var)

}

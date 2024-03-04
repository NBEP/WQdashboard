#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # Add module servers ----
  mod_sidebar_server("sidebar_1",
    selected_tab = reactive({ input$tabset }))
}

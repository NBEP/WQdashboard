#' graphs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_graphs_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::navset_card_tab(
      id = "graphs_tabset",
      full_screen = FALSE,
      # title = "Graphs",
      bslib::nav_panel(
        "Long Term Trends",
        mod_graphs_graph_ui(ns("graphs_graph_1"))),
      if (length(unique(df_data$Depth)) < 1) {
        bslib::nav_panel(
          "Compare Depths",
          "up to 4 depths?")
      },
      bslib::nav_panel(
        "Compare Sites",
        "up to 5 sites?"),
      bslib::nav_panel(
        "Compare Parameters",
        "max 2 param")
    )
  )
}

#' graphs Server Functions
#'
#' @noRd
mod_graphs_server <- function(id, selected_var){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Initial filter - date & month
    df_filter <- reactive({
      req(selected_var$date_range())
      req(selected_var$month)

      df <- df_data %>%
        dplyr::filter(
          Date >= selected_var$date_range()[1] &
            Date <= selected_var$date_range()[2]) %>%
        dplyr::filter(Month %in% selected_var$month()) %>%
        dplyr::select(!Month)

      return(df)
    })

    # Tab 1 ----
    df_trends <- reactive({
      req(selected_var$sites_n())
      req(selected_var$param_n())

      sites <- selected_var$sites_n()
      param <- selected_var$param_n()
      depth <- c(NA, selected_var$depth_n())

      df <- df_filter() %>%
        dplyr::filter(Site_ID == sites & Parameter == param & Depth %in% depth)

      return(df)
    })

    # Modules ----
    mod_graphs_graph_server("graphs_graph_1",
      df = reactive({ df_trends() }),
      thresholds = TRUE)

  })
}

## To be copied in the UI
# mod_graphs_ui("graphs_1")

## To be copied in the server
# mod_graphs_server("graphs_1")

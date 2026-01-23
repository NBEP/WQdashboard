#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Set variables
  map_bounds <- list(
    lng1 = min(df_sites$Longitude),
    lat1 = min(df_sites$Latitude),
    lng2 = max(df_sites$Longitude),
    lat2 = max(df_sites$Latitude)
  )

  # Add module servers ----
  sidebar_var <- importwqd::mod_sidebar_server(
    "sidebar",
    df_sites = df_sites,
    df_data = df_data,
    df_score = df_score,
    selected_tab = reactive({
      input$tabset
    }),
    selected_site = map_var$site
  )

  map_var <- mod_map_server(
    "map",
    in_var = sidebar_var,
    map_bounds = map_bounds,
    df_raw = df_score[0, ],
    selected_tab = reactive({
      input$tabset
    }),
    shp_watershed = shp_watershed,
    shp_river = shp_river
  )

  mod_report_card_server(
    "report_card_1",
    sidebar_var,
    selected_tab = reactive({
      input$tabset
    })
  )
  mod_graphs_server("graphs_1", sidebar_var)
  mod_download_server("download_1", sidebar_var)

  # Update tabs ----
  observe({
    updateTabsetPanel(inputId = "tabset", selected = "graphs")
  }) %>%
    bindEvent(map_var$graph_link())
}

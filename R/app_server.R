#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Sidebar module ----
  sidebar_var <- importwqd::mod_sidebar_server(
    "sidebar",
    df_sites = df_sites,
    df_data = df_data,
    df_score = df_score,
    selected_tab = reactive({
      input$tabset
    }),
    selected_site = reactive({
      df_sites$Site_ID[1]
    })
  )

  # Map module ----
  map_bounds <- list(
    lat1 = min(df_sites$Latitude),
    lat2 = max(df_sites$Latitude),
    lng1 = min(df_sites$Longitude),
    lng2 = max(df_sites$Longitude)
  )

  watershed <- NULL
  river <- NULL

  if (exists("shp_watershed")) {
    watershed <- shp_watershed
  }
  if (exists("shp_river")) {
    river <- shp_river
  }

  map_var <- importwqd::mod_map_server(
    "map",
    in_var = sidebar_var,
    map_bounds = map_bounds,
    df_raw = df_score[0, ],
    selected_tab = reactive({
      input$tabset
    }),
    shp_watershed = watershed,
    shp_river = river
  )

  # Report card module ----
  keep_col <- c(
    "Site_Name", "State", "Town", "Watershed", "Group", "Depth", "Parameter",
    "score_str"
  )

  df_report_raw <- df_score[0,] |>
    dplyr::select(dplyr::any_of(keep_col))

  mod_report_card_server(
    "report_card",
    in_var = sidebar_var,
    df_raw = df_report_raw,
    selected_tab = reactive({
      input$tabset
    })
  )

  # mod_graphs_server("graphs_1", sidebar_var)
  # mod_download_server("download_1", sidebar_var)

  # Update tabs ----
  observe({
    updateTabsetPanel(inputId = "tabset", selected = "graphs")
  }) %>%
    bindEvent(map_var$graph_link())
}

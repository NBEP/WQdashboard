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
      # Trends -----
      bslib::nav_panel(
        "Long Term Trends",
        mod_graphs_graph_ui(ns("graph_trends"))),
      # Depth ----
      if (length(unique(df_data$Depth)) > 1) {
        bslib::nav_panel(
          "Compare Depths",
          select_dropdown(
            ns("extra_depth"),
            label = h3("Select Depths"),
            choices = unique(df_data$Depth)),
          mod_graphs_graph_ui(ns("graph_depth")))
      },
      # Sites ----
      bslib::nav_panel(
        "Compare Sites",
        select_dropdown(
          ns("extra_sites"),
          label = HTML(paste(
            h2("Additional Sites"),
            "Select up to four sites")),
          choices = df_sites$Site_ID,
          choice_names = df_sites$Site_Name,
          max_options = 4),
        mod_graphs_graph_ui(ns("graph_sites"))),
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

    # Graph: Trends ----
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

    mod_graphs_graph_server("graph_trends",
      df = reactive({ df_trends()}),
      thresholds = TRUE)

    # Graph: Compare Sites ----
    df_comp_sites <- reactive({
      req(selected_var$sites_n())
      req(selected_var$param_n())

      sites <- c(selected_var$sites_n(), input$extra_sites)
      param <- selected_var$param_n()
      depth <- c(NA, selected_var$depth_n())

      df <- df_filter() %>%
        dplyr::filter(Site_ID %in% sites & Parameter == param & Depth %in% depth)

      return(df)
    })

    mod_graphs_graph_server("graph_sites", df = reactive({ df_comp_sites()}))

    # Graph: Compare Depths ----
    df_comp_depth <- reactive({
      req(selected_var$sites_n())
      req(selected_var$param_n())

      sites <- selected_var$sites_n()
      param <- selected_var$param_n()
      depth <- c(selected_var$depth_n(), input$extra_depth)

      df <- df_filter() %>%
        dplyr::filter(Site_ID == sites & Parameter == param) %>%
        dplyr::filter(!is.na(Depth) & Depth %in% depth)

      return(df)
    })

    mod_graphs_graph_server("graph_depth",
      df = reactive({ df_comp_depth() }),
      group = "Depth")

  })
}

## To be copied in the UI
# mod_graphs_ui("graphs_1")

## To be copied in the server
# mod_graphs_server("graphs_1")

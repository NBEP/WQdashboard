#' graphs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_graphs_ui <- function(id, in_var) {
  ns <- NS(id)
  tagList(
    bslib::navset_card_tab(
      id = ns("tabset"),
      full_screen = FALSE,
      # title = "Graphs",
      # Trends -----
      bslib::nav_panel(
        "Long Term Trends",
        value = "trend",
        mod_graphs_trends_ui(ns("graph_trends"))
      ),
      # Depth ----
      if (!is.null(in_var$depth)) {
        bslib::nav_panel(
          "Compare Depths",
          value = "depth",
          importwqd::dropdown(
            ns("extra_depth"),
            label = h2("Select Depths"),
            choices = in_var$depth
          ),
          mod_graphs_graph_ui(ns("graph_depth"))
        )
      },
      # Sites ----
      bslib::nav_panel(
        "Compare Sites",
        value = "site",
        importwqd::dropdown(
          ns("extra_sites"),
          label = HTML(
            paste(
              h2("Select Sites"),
              "Select up to five sites"
            )
          ),
          choices = in_var$site_id,
          choice_names = in_var$site_name,
          max_options = 5
        ),
        mod_graphs_graph_ui(ns("graph_sites"))
      ),
      # Parameters ----
      bslib::nav_panel(
        "Compare Parameters",
        value = "param",
        importwqd::dropdown(
          ns("extra_param"),
          label = HTML(
            paste(
              h2("Select Indicators"),
              "Select two indicators"
            )
          ),
          choices = in_var$param,
          max_options = 2
        ),
        mod_graphs_graph_ui(ns("graph_param"))
      )
    )
  )
}

#' graphs Server Functions
#'
#' @noRd
mod_graphs_server <- function(id, in_var) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update dropdowns ----
    observe({
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "extra_sites",
        choices = in_var$site_list(),
        selected = in_var$sites_n()
      )
    }) |>
      bindEvent(in_var$site_list(), in_var$sites_n())

    observe({
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "extra_param",
        selected = in_var$param_n()
      )
    }) |>
      bindEvent(in_var$param_n())

    # Graph: Trends ----
    df_trends <- reactive({
      req(input$tabset == "trend")
      req(in_var$sites_n())
      req(in_var$param_n())

      sites <- in_var$sites_n()
      param <- in_var$param_n()

      dat <- in_var$df_graph() |>
        dplyr::filter(
          .data$Site_ID == !!sites,
          .data$Parameter == !!param
        )

      chk <- grepl("depth|height", param)
      if ("Depth" %in% colnames(dat) & !chk) {
        depth <- in_var$depth_n()
        dat <- dplyr::filter(dat, .data$Depth == !!depth)
      }

      dat
    }) |>
      bindEvent(
        input$tabset, in_var$df_graph(), in_var$sites_n(),
        in_var$param_n(), in_var$depth_n()
      )

    mod_graphs_trends_server(
      "graph_trends",
      df = reactive({
        df_trends()
      })
    )

    # Graph: Compare Sites ----
    # * Update picker input ----


    # * Create graph ----
    df_comp_sites <- reactive({
      req(in_var$param_n())

      sites <- input$extra_sites
      param <- in_var$param_n()
      depth <- c(NA, in_var$depth_n())

      df <- in_var$df_graph() |>
        dplyr::filter(
          Site_ID %in% sites &
            Parameter == param &
            Depth %in% depth
        )

      return(df)
    })

    mod_graphs_graph_server("graph_sites", df = reactive({
      df_comp_sites()
    }))

    # Graph: Compare Depths ----
    df_comp_depth <- reactive({
      req(in_var$sites_n())
      req(in_var$param_n())

      sites <- in_var$sites_n()
      param <- in_var$param_n()
      depth <- c(NA, input$extra_depth)

      df <- in_var$df_graph() |>
        dplyr::filter(
          Site_ID == sites &
            Parameter == param &
            !is.na(Depth) &
            Depth %in% depth
        )

      return(df)
    })

    mod_graphs_graph_server("graph_depth",
      df = reactive({
        df_comp_depth()
      }),
      group = "Depth"
    )

    # Graph: Compare Parameters ----
    df_comp_par <- reactive({
      req(in_var$sites_n())

      sites <- in_var$sites_n()
      param <- input$extra_param
      depth <- c(NA, in_var$depth_n())

      df <- in_var$df_graph() |>
        dplyr::filter(
          Site_ID == sites &
            Parameter %in% param &
            Depth %in% depth
        )

      return(df)
    })

    mod_graphs_graph_server("graph_param",
      df = reactive({
        df_comp_par()
      }),
      group = "Parameter"
    )
  })
}

## To be copied in the UI
# mod_graphs_ui("graphs_1")

## To be copied in the server
# mod_graphs_server("graphs_1")

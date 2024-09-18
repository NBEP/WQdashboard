#' graphs_graph UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_graphs_graph_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Enable javascript ----
    shinyjs::useShinyjs(),
    # UI ----
    tabsetPanel(
      id = ns("hide_show"),
      type = "hidden",
      # * Graph/table ----
      tabPanelBody(
        "show_graph",
        tabsetPanel(
          id = ns("graph_table"),
          tabPanel(
            "Graph",
            plotly::plotlyOutput(outputId = ns("plot")),
            tabsetPanel(
              id = ns("tabset_trendline"),
              type = "hidden",
              tabPanelBody(
                "show_btn",
                div(
                  style = "display:inline-block;text-align:center;margin:1rem",
                  actionButton(
                    ns("toggle_trends"),
                    label = "Hide Trendline",
                    width = "fit-content")
                  )
                ),
              tabPanelBody("hide_btn"))
            ),
          tabPanel(
            "Table",
            htmlOutput(ns("fig_title")),
            reactable::reactableOutput(ns("table"))
            )
          ),
        # * Trend analysis ----
        tabsetPanel(
          id = ns("tabset_trends"),
          type = "hidden",
          tabPanelBody(
            "trend_desc",
            reactable::reactableOutput(ns("trend_summary")),
            actionButton(
              ns("btn_stats"),
              label = "Show Additional Stats",
              width = "fit-content")),
          tabPanelBody(
            "trend_error",
            "Unable to calculate trends. At least five years of data required."
            ),
          tabPanelBody("trend_blank")
          )
        ),
      # * No data message ----
      tabPanelBody(
        "hide_graph",
        "No data found. Please change selection.")
    )
  )
}

#' graphs_graph Server Functions
#'
#' @noRd
mod_graphs_graph_server <- function(id, df, thresholds = FALSE,
    best_fit = FALSE, group = "Site_Name"){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Select tab ----
    hide_graph <- reactive({
      if (nrow(df()) == 0) { return("hide") } else { return("show") }
    }) %>%
      bindEvent(df())

    observe({
      if (hide_graph() == "show") {
        updateTabsetPanel(inputId = "hide_show", selected = "show_graph")
      } else {
        updateTabsetPanel(inputId = "hide_show", selected = "hide_graph")
      }
    }) %>%
      bindEvent(hide_graph())

    # Figure Title ----
    fig_title <- reactive({
      df <- df()

      if (group == "Site_Name") {
        fig_title <- df$Parameter[1]
      } else if (group == "Depth") {
        fig_title <- paste(df$Parameter[1], "at", df$Site_Name[1])
      } else {
        fig_title <- df$Site_Name[1]
      }

      return(fig_title)
    })

    # Graph options ----
    val <- reactiveValues(
      threshold = TRUE,
      trendline = TRUE)

    # Thresholds ----
    # * Calc thresholds ----
    thresh <- reactive({
      if (thresholds == FALSE) { return(NULL) }

      thresh <- find_threshold(
        site_id = df()$Site_ID[1],
        parameter = df()$Parameter[1],
        depth_cat = df()$Depth[1])

      return(thresh)
    })

    # Trend line ----
    # * Check if valid ----
    len_years <- reactive({ length(unique(df()$Year)) })

    show_fit <- reactive({
      if (best_fit == FALSE) {
        show_fit = "hide"
      } else if (len_years() < 5) {
        show_fit = "error"
      } else {
        show_fit = "show"
      }

      return(show_fit)
    })

    observe({
      if (show_fit() == "show") {
        updateTabsetPanel(inputId = "tabset_trends", selected = "trend_desc")
        updateTabsetPanel(inputId = "tabset_trendline", selected = "show_btn")
      } else if (show_fit() == "error") {
        updateTabsetPanel(inputId = "tabset_trends", selected = "trend_error")
        updateTabsetPanel(inputId = "tabset_trendline", selected = "hide_btn")
      } else {
        updateTabsetPanel(inputId = "tabset_trends", selected = "trend_blank")
        updateTabsetPanel(inputId = "tabset_trendline", selected = "hide_btn")
      }
    }) %>%
      bindEvent(show_fit())

    # * Toggle visibility ----
    observe({
      if (val$trendline == TRUE) {
        val$trendline <- FALSE
        updateActionButton(
          session, "toggle_trends",
          label = "Show Trendline")
        plotly::plotlyProxy("plot", session) %>%
          plotly::plotlyProxyInvoke(
            "restyle",
            list(visible = c(TRUE, FALSE, FALSE)))
      } else {
        val$trendline <- TRUE
        updateActionButton(
          session, "toggle_trends",
          label = "Hide Trendline")
        plotly::plotlyProxy("plot", session) %>%
          plotly::plotlyProxyInvoke(
            "restyle",
            list(visible = TRUE))
      }
    }) %>%
      bindEvent(input$toggle_trends)

    # * Calc trendline ----
    df_trendline <- reactive({
      if (show_fit() == "show") {
        trend <- trend_line(df())
      } else {
        trend <- NA
      }
      return(trend)
    })

    # Graph ----
    output$plot <- plotly::renderPlotly({
      if (group == "Parameter") {
        graph_two_var(df(), fig_title())
      } else {
        graph_one_var(
          df = df(),
          fig_title = fig_title(),
          group = group,
          thresholds = thresh(),
          show_fit = show_fit(),
          visible = val$trendline,
          df_fit = df_trendline())
      }
    })

    # Table ----
    output$fig_title <- renderUI({ h2(fig_title()) })

    output$table <- reactable::renderReactable({
      format_graph_table(df(), group)
    })

    # Trend Summary ----
    output$trend_summary <- reactable::renderReactable({
      if (show_fit() == "show") {
        format_trend_table(
          p = df_trendline()$p,
          slope = df_trendline()$slope
        )
      } else {
        format_trend_table(p = 1, slope = 1)
      }
    })

  })
}

## To be copied in the UI
# mod_graphs_graph_ui("graphs_graph_1")

## To be copied in the server
# mod_graphs_graph_server("graphs_graph_1")

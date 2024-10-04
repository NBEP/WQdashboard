#' graphs_trends UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_graphs_trends_ui <- function(id){
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
            textOutput(ns("caption")),
            div(
              style = "text-align:center;margin:1rem",
              div(
                style = "display:inline-block;",
                actionButton(
                  ns("toggle_trends"),
                  label = "Hide Trendline",
                  width = "fit-content")
              ),
              div(
                style = "display:inline-block;",
                actionButton(
                  ns("toggle_thresh"),
                  label = "Hide Thresholds",
                  width = "fit-content")
              )
            )
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
          tabPanelBody("trend_desc"),
          tabPanelBody(
            "trend_error",
            "Unable to calculate trend. At least ten years of data required.")
        )
      ),
      # * No data message ----
      tabPanelBody(
        "hide_graph",
        "No data found. Please change selection.")
    )
  )
}

#' graphs_trends Server Functions
#'
#' @noRd
mod_graphs_trends_server <- function(id, df){
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
    fig_title <- reactive({ df()$Parameter[1] })

    # Graph options ----
    val <- reactiveValues(
      threshold = TRUE,
      trendline = TRUE)

    # Thresholds ----
    # * Toggle visibility ----
    observe({
      if (val$threshold == TRUE) {
        val$threshold <- FALSE
        updateActionButton(
          session, "toggle_thresh",
          label = "Show Thresholds")
        plotly::plotlyProxy("plot", session) %>%
          plotly::plotlyProxyInvoke(
            "restyle",
            list(visible = FALSE),
            c("Does Not Meet Criteria",
              "Lowest Acceptable Value",
              "Highest Acceptable Value"))
      } else {
        val$threshold <- TRUE
        updateActionButton(
          session, "toggle_thresh",
          label = "Hide Thresholds")
        plotly::plotlyProxy("plot", session) %>%
          plotly::plotlyProxyInvoke(
            "restyle",
            list(visible = TRUE),
            c("Does Not Meet Criteria",
              "Lowest Acceptable Value",
              "Highest Acceptable Value"))
      }
    }) %>%
      bindEvent(input$toggle_thresh)

    # * Calc thresholds ----
    thresh <- reactive({
      thresh <- find_threshold(
        site_id = df()$Site_ID[1],
        parameter = df()$Parameter[1],
        depth_cat = df()$Depth[1])

      return(thresh)
    })

    # Trend line ----
    # * Check if valid ----
    len_years <- reactive({ length(unique(df()$Year)) })
    show_fit <- reactive({ if (len_years() < 10) { FALSE } else { TRUE } })

    shinyjs::disable("toggle_trends")

    observe({
      if (show_fit()) {
        updateTabsetPanel(inputId = "tabset_trends", selected = "trend_desc")
        shinyjs::enable("toggle_trends")
      } else {
        updateTabsetPanel(inputId = "tabset_trends", selected = "trend_error")
        shinyjs::disable("toggle_trends")
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
            list(visible = FALSE),
            c("Trend Line (GAM)",
              "95% Confidence Interval"))
      } else {
        val$trendline <- TRUE
        updateActionButton(
          session, "toggle_trends",
          label = "Hide Trendline")
        plotly::plotlyProxy("plot", session) %>%
          plotly::plotlyProxyInvoke(
            "restyle",
            list(visible = TRUE),
            c("Trend Line (GAM)",
              "95% Confidence Interval"))
      }
    }) %>%
      bindEvent(input$toggle_trends)

    # Caption ----
    fig_caption <- reactive({ caption_graph(df(), "Site_Name", thresh()) })

    # Graph ----
    output$plot <- plotly::renderPlotly({
      graph_trends(
        df = df(),
        fig_title = fig_title(),
        thresholds = thresh(),
        show_thresh = val$threshold,
        create_trend = show_fit(),
        show_trend = val$trendline)
    })

    # Caption ----
    output$caption <- renderText({ fig_caption() })

    # Table ----
    output$fig_title <- renderUI({
      HTML(paste( h2(fig_title()), fig_caption() ))
    })

    output$table <- reactable::renderReactable({
      format_graph_table(df(), "Site_Name")
    })

  })
}

## To be copied in the UI
# mod_graphs_trends_ui("graphs_graph_1")

## To be copied in the server
# mod_graphs_trends_server("graphs_graph_1")

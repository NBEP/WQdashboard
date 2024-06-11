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
            plotly::plotlyOutput(outputId = ns("plot")) %>%
              shinycssloaders::withSpinner(type = 5)),
          tabPanel(
            "Table",
            htmlOutput(ns("fig_title")),
            reactable::reactableOutput(ns("table")) %>%
              shinycssloaders::withSpinner(type = 5))),
        # * Trend analysis ----
        tabsetPanel(
          id = ns("tabset_trends"),
          type = "hidden",
          tabPanelBody(
            "trend_btn",
            actionButton(
              ns("btn_trends"),
              label = "Run Trend Analysis",
              width = "fit-content")),
          tabPanelBody(
            "trend_desc",
            htmlOutput(ns("trend_desc"))),
          tabPanelBody("trend_blank"))
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
    best_fit = FALSE, group = "Site_Name", par1 = NA){
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

    # Graph ----
    output$plot <- plotly::renderPlotly({
      if (group == "Parameter") {
        graph_two_var(df(), fig_title(), par1())
      } else {
        graph_one_var(df(), fig_title(), group, thresholds)
      }
    })

    # Table ----
    output$fig_title <- renderUI({ h2(fig_title()) })

    output$table <- reactable::renderReactable({
      format_graph_table(df(), group)
    })

    # Trend line ----

    # Button/tabs
    len_years <- reactive({ length(unique(df()$Year)) })

    shinyjs::disable("btn_trends")

    observe({
      if (len_years() > 10) {
        shinyjs::enable("btn_trends")
      } else {
        shinyjs::disable("btn_trends")
      }
    }) %>%
      bindEvent(len_years())

    observe({
      updateTabsetPanel(inputId = "tabset_trends", selected = "trend_desc")
    }) %>%
      bindEvent(input$btn_trends)

    observe({
      if (best_fit) {
        updateTabsetPanel(inputId = "tabset_trends", selected = "trend_btn")
      } else {
        updateTabsetPanel(inputId = "tabset_trends", selected = "trend_blank")
      }
    }) %>%
      bindEvent(df())

    # Calc trend
    df_trendline <- reactive({ trend_line(df()) }) %>%
      bindEvent(input$btn_trends)

    observe({
      df <- df_trendline()$df
      p <- df_trendline()$p

      if (p < .1) {
        dash_color = "#4e4e90"
        dash_width = 2
        dash_type = NA
      } else {
        dash_color = "#c2c2d5"
        dash_width = 1
        dash_type = "dash"
      }

      plotly::plotlyProxy("plot", session) %>%
        plotly::plotlyProxyInvoke(
          "addTraces",
          list(
            x = df$Date,
            y = df$Result_fit,
            type = "scatter",
            mode = "lines",
            line = list(
              color = dash_color,
              width = dash_width,
              dash = dash_type),
            name = "Trend Line")) %>%
        plotly::plotlyProxyInvoke(
          "addTraces",
          list(
            x = df$Date,
            y = df$Result_avg,
            type = "scatter",
            mode = "markers",
            marker = list(
              size = 10,
              color = "#4e4e90",
              line = list(
                color = "#07077e",
                width = 2)),
            name = "Yearly Average"))

    }) %>%
      bindEvent(input$btn_trends)

    output$trend_desc <- renderUI({
      p <- df_trendline()$p
      slope <- df_trendline()$slope

      desc <- "<b>Trend:</b> "

      if (slope < 0) {
        trend <- "🢃 Decreasing"
      } else if (slope > 0) {
        trend <- "🢁 Increasing"
      } else {
        trend <- "No trend"
      }

      if (p >= 0.1) {
        conf <- "Confident in no trend"
      } else if (p >= 0.05) {
        conf <- "Somewhat confident in trend"
      } else if (p >= 0.01) {
        conf <- "Confident in trend"
      } else {
        conf <- "Very confident in trend"
      }

      desc <- HTML(paste0("<b>Trend:</b> ", trend, " (", slope,
                          ")<br/><b>Confidence:</b> ", conf, " (", p, ")"))

      return(desc)
    })

  })
}

## To be copied in the UI
# mod_graphs_graph_ui("graphs_graph_1")

## To be copied in the server
# mod_graphs_graph_server("graphs_graph_1")

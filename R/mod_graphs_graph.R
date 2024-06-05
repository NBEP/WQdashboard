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
    tabsetPanel(
      id = ns("hide_show"),
      type = "hidden",
      tabPanelBody(
        "show_graph",
        tabsetPanel(
          id = ns("graph_table"),
          type = "hidden",
          tabPanelBody(
            "graph",
            actionButton(
              ns("btn_table"),
              label = "View as Table",
              width = "fit-content"),
            plotly::plotlyOutput(outputId = ns("plot")),
            "for trendlines - seperate graph? modify existing graph?"),
          tabPanelBody(
            "table",
            actionButton(
              ns("btn_graph"),
              label = "View as Graph",
              width = "fit-content"),
            "this is a table"))
        ),
      tabPanelBody(
        "hide_graph",
        "No data found. Please change selection.")
    )
  )
}

#' graphs_graph Server Functions
#'
#' @noRd
mod_graphs_graph_server <- function(id, df, thresholds = FALSE){
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

    observe({
      updateTabsetPanel(inputId = "graph_table", selected = "table")
    }) %>%
      bindEvent(input$btn_table)

    observe({
      updateTabsetPanel(inputId = "graph_table", selected = "graph")
    }) %>%
      bindEvent(input$btn_graph)

    # Graph ----
    output$plot <- plotly::renderPlotly({
      graph_one_var(df(), thresholds = thresholds)
    })

    # Table ----
  })
}

## To be copied in the UI
# mod_graphs_graph_ui("graphs_graph_1")

## To be copied in the server
# mod_graphs_graph_server("graphs_graph_1")

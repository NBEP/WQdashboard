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
          tabPanel(
            "Graph",
            plotly::plotlyOutput(outputId = ns("plot")) %>%
              shinycssloaders::withSpinner(type = 5),
            "for trendlines - seperate graph? modify existing graph?"),
          tabPanel(
            "Table",
            htmlOutput(ns("fig_title")),
            reactable::reactableOutput(ns("table")) %>%
              shinycssloaders::withSpinner(type = 5)))
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
mod_graphs_graph_server <- function(id, df, thresholds = FALSE,
    group = "Site_Name"){
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

    # Graph/Table Title ----
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
      graph_one_var(df(), fig_title(), group, thresholds)
    })

    # Table ----
    output$fig_title <- renderUI({ h2(fig_title()) })

    output$table <- reactable::renderReactable({
      format_graph_table(df(), group)
    })

  })
}

## To be copied in the UI
# mod_graphs_graph_ui("graphs_graph_1")

## To be copied in the server
# mod_graphs_graph_server("graphs_graph_1")

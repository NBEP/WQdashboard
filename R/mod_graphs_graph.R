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
            textOutput(ns("caption"))
            ),
          tabPanel(
            "Table",
            htmlOutput(ns("fig_title")),
            reactable::reactableOutput(ns("table"))
            )
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
mod_graphs_graph_server <- function(id, df, group = "Site_Name"){
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

    # Figure Title, Caption ----
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

    fig_caption <- reactive({ caption_graph(df(), group) })

    # Graph ----
    output$plot <- plotly::renderPlotly({
      if (group == "Parameter") {
        graph_two_var(df(), fig_title())
      } else {
        graph_one_var(
          df = df(),
          fig_title = fig_title(),
          group = group)
      }
    })

    # Caption ----
    output$caption <- renderText({ fig_caption() })

    # Table ----
    output$fig_title <- renderUI({
      HTML(paste( h2(fig_title()), fig_caption() ))
    })

    output$table <- reactable::renderReactable({
      format_graph_table(df(), group)
    })

  })
}

## To be copied in the UI
# mod_graphs_graph_ui("graphs_graph_1")

## To be copied in the server
# mod_graphs_graph_server("graphs_graph_1")

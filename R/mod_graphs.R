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
    bslib::card(
      min_height = 250,
      full_screen = FALSE,
      h2("Graphs"),
      plotly::plotlyOutput(outputId = ns("plot")),
      h3("Long Term Trends"),
      "bar graph?"
      ),
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
            Date <=selected_var$date_range()[2]) %>%
        dplyr::filter(Month %in% selected_var$month()) %>%
        dplyr::select(!Month)

      return(df)
    })

    # Graph
    output$plot <- plotly::renderPlotly({
      scatter_plot(
        df_filter(),
        site_id = selected_var$sites_n(),
        parameter = selected_var$param_n(),
        depth = NA)
    })

  })
}

## To be copied in the UI
# mod_graphs_ui("graphs_1")

## To be copied in the server
# mod_graphs_server("graphs_1")

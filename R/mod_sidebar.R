#' sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sidebar_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::accordion(
      multiple = TRUE,
      bslib::accordion_panel(
        title = h2("Location"),
        value = "location",
        h3("Select State"),
        h3("Select Town"),
        h3("Select Watershed"),
        conditionalPanel(
          condition = paste0("output['", ns("selected_tab"), "'] != 'graphs'"),
          h3("Select Sites")
          ),
        conditionalPanel(
          condition = paste0("output['", ns("selected_tab"), "'] == 'graphs'"),
          h3("Select Sites"),
          "Select up to N sites"
        )
      ),
      bslib::accordion_panel(
        title = h2("Indicators"),
        value = "indicators",
        conditionalPanel(
          condition = paste0("output['", ns("selected_tab"), "'] == 'report_card' |
                             output['", ns("selected_tab"), "'] == 'download'"),
          h3("Select Indicators"),
          ),
        conditionalPanel(
          condition = paste0("output['", ns("selected_tab"), "'] == 'graphs'"),
          h3("Select Indicators"),
          "Select up to N indicators"
          ),
        conditionalPanel(
          condition = paste0("output['", ns("selected_tab"), "'] == 'map'"),
          h3("Select Indicator"),
          ),
        conditionalPanel(
          condition = paste0("output['", ns("selected_tab"), "'] == 'map' |
                             output['", ns("selected_tab"), "'] == 'report_card'"),
          shinyWidgets::pickerInput(
            ns("select_score"),
            label = h3("Select Scores"),
            choices = c("Excellent", "Good", "Fair", "Poor", "No Data"),
            selected = c("Excellent", "Good", "Fair", "Poor", "No Data"),
            options = list(`selected-text-format` = "count > 2"),
            multiple = TRUE)
          )
        ),
      bslib::accordion_panel(
        title = h2("Dates"),
        value = "dates",
        conditionalPanel(
          condition = paste0("output['", ns("selected_tab"), "'] == 'map' |
                             output['", ns("selected_tab"), "'] == 'report_card'"),
          h3("Select Year")
        ),
        conditionalPanel(
          condition = paste0("output['", ns("selected_tab"), "'] == 'graphs' |
                             output['", ns("selected_tab"), "'] == 'download'"),
          h3("Select Years")
        ),
        conditionalPanel(
          condition = paste0("output['", ns("selected_tab"), "'] == 'graphs' |
                             output['", ns("selected_tab"), "'] == 'download'"),
          h3("Select Months")
        )
      )
    )
  )
}

#' sidebar Server Functions
#'
#' @param selected_tab String, selected tab ID
#'
#' @noRd
mod_sidebar_server <- function(id, selected_tab){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Send vars to ui
    output$selected_tab <- renderText({ selected_tab() })
    outputOptions(output, "selected_tab", suspendWhenHidden = FALSE)

  })
}

## To be copied in the UI
# mod_sidebar_ui("sidebar_1")

## To be copied in the server
# mod_sidebar_server("sidebar_1")

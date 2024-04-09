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
      # multiple = FALSE,
      bslib::accordion_panel(
        title = h2("Location"),
        value = "location",
        mod_select_location_ui(ns("select_location"))
      ),
      bslib::accordion_panel(
        title = h2("Indicator"),
        value = "indicators",
        tabsetPanel(
          id = ns("tabset_param"),
          type = "hidden",
          tabPanelBody(
            "param_n",
            select_dropdown(
              ns("select_param_n"),
              label = h3("Select Indicator"),
              choices = unique(df_data$Parameter),
              multiple = FALSE)),
          tabPanelBody(
            "param_all",
            select_dropdown(
              ns("select_param_all"),
              label = h3("Select Indicators"),
              choices = unique(df_data$Parameter)))),
        tabsetPanel(
          id = ns("tabset_score"),
          type = "hidden",
          tabPanelBody(
            "show_score",
            select_dropdown(
              ns("select_score"),
              label = h3("Select Scores"),
              choices = c("Excellent", "Good", "Fair", "Poor", "No Data"),
              sort_choices = FALSE)),
          tabPanelBody("hide_score"))),
      bslib::accordion_panel(
        title = h2("Date"),
        value = "dates",
        tabsetPanel(
          id = ns("tabset_dates"),
          type = "hidden",
          tabPanelBody(
            "by_year",
            select_dropdown(
              ns("select_year"),
              label = h3("Select Year"),
              choices = df_score$Year,
              sort_decreasing = TRUE,
              multiple = FALSE)),
          tabPanelBody(
            "by_date",
            dateRangeInput(
              ns("select_date_range"),
              label = h3("Select Date Range"),
              start = min(df_data$Date),
              end = max(df_data$Date),
              min = min(df_data$Date),
              max = max(df_data$Date),
              format = "mm/dd/yy"),
            select_dropdown(
              ns("select_month"),
              label = h3("Select Months"),
              choices = sort_months(df_data$Month),
              sort_choices = FALSE))
          )  # end tabsetPanel
        )  # end accordion_panel
    )  # end accordion
  )  # end taglist
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

    # Modules
    loc_server <- mod_select_location_server("select_location", selected_tab)

    # Show/hide secret tabs
    observe({
      if (selected_tab() == "map") {
        updateTabsetPanel(inputId = "tabset_param", selected = "param_n")
        updateTabsetPanel(inputId = "tabset_score", selected = "show_score")
        updateTabsetPanel(inputId = "tabset_dates", selected = "by_year")
      } else if (selected_tab() == "report_card") {
        updateTabsetPanel(inputId = "tabset_param", selected = "param_all")
        updateTabsetPanel(inputId = "tabset_score", selected = "show_score")
        updateTabsetPanel(inputId = "tabset_dates", selected = "by_year")
      } else if (selected_tab() == "graphs") {
        updateTabsetPanel(inputId = "tabset_param", selected = "param_n")
        updateTabsetPanel(inputId = "tabset_score", selected = "hide_score")
        updateTabsetPanel(inputId = "tabset_dates", selected = "by_date")
      } else {
        updateTabsetPanel(inputId = "tabset_param", selected = "param_all")
        updateTabsetPanel(inputId = "tabset_score", selected = "hide_score")
        updateTabsetPanel(inputId = "tabset_dates", selected = "by_date")
      }
    }) %>%
      bindEvent(selected_tab())


    return(
      list(
        sites_all = reactive({ loc_server$sites_all() }),
        sites_n = reactive({ loc_server$sites_n() }),
        param_all = reactive({ input$select_param_all }),
        param_n = reactive({ input$select_param_n }),
        #score = reactive({ input$select_score }),
        year = reactive({ input$select_year }),
        date_range = reactive({ input$select_date_range }),
        month = reactive({ input$select_month })
      )
    )

  })
}

## To be copied in the UI
# mod_sidebar_ui("sidebar_1")

## To be copied in the server
# mod_sidebar_server("sidebar_1")

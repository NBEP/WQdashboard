#' sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sidebar_ui <- function(id) {
  ns <- NS(id)

  tagList(
    bslib::accordion(
      multiple = FALSE,
      # Select location ------------------------------------------------------
      bslib::accordion_panel(
        title = h2("1. Location"),
        value = "location",
        mod_select_location_ui(ns("select_location"))
      ),
      # Select data -----------------------------------------------------
      bslib::accordion_panel(
        title = h2("2. Data"),
        value = "indicators",
        tabsetPanel(
          id = ns("tabset_param"),
          type = "hidden",
          tabPanelBody(
            "param_n",
            select_dropdown(
              ns("select_param_n"),
              label = h3("Select Indicator"),
              choices = dat_list$param,
              multiple = FALSE
            )
          ),
          tabPanelBody(
            "param_short",
            select_dropdown(
              ns("select_param_short"),
              label = h3("Select Indicators"),
              choices = dat_list$param_short
            )
          ),
          tabPanelBody(
            "param_all",
            select_dropdown(
              ns("select_param_all"),
              label = h3("Select Indicators"),
              choices = dat_list$param
            )
          )
        ),
        tabsetPanel(
          id = ns("tabset_score"),
          type = "hidden",
          tabPanelBody(
            "show_score",
            checkboxInput(
              ns("chk_nascore"),
              label = "Include missing scores",
              value = TRUE
            )
          ),
          tabPanelBody("hide_score")
        ),
        tabsetPanel(
          id = ns("tabset_depth"),
          type = "hidden",
          tabPanelBody(
            "depth_n",
            select_dropdown(
              ns("select_depth_n"),
              label = h3("Select Depth"),
              choices = dat_list$depth,
              sort_choices = FALSE,
              multiple = FALSE
            )
          ),
          tabPanelBody(
            "depth_all",
            select_dropdown(
              ns("select_depth_all"),
              label = h3("Select Depths"),
              choices = dat_list$depth,
              sort_choices = FALSE
            )
          ),
          tabPanelBody("depth_null")
        )
      ),
      # Select date -----------------------------------------------------------
      bslib::accordion_panel(
        title = h2("3. Date"),
        value = "dates",
        tabsetPanel(
          id = ns("tabset_dates"),
          type = "hidden",
          tabPanelBody(
            "by_year",
            select_dropdown(
              ns("select_year"),
              label = h3("Select Year"),
              choices = dat_list$year,
              sort_decreasing = TRUE,
              multiple = FALSE
            )
          ),
          tabPanelBody(
            "by_range",
            sliderInput(
              ns("select_year_range"),
              label = h3("Select Years"),
              min = dat_list$year[1],
              max = dat_list$year[length(dat_list$year)],
              value = c(
                dat_list$year[1],
                dat_list$year[length(dat_list$year)]
              ),
              sep = ""
            ),
            shinyWidgets::sliderTextInput(
              ns("select_month"),
              label = h3("Select Months"),
              choices = dat_list$month,
              selected = c(
                dat_list$month[1],
                dat_list$month[length(dat_list$month)]
              )
            )
          )
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
mod_sidebar_server <- function(id, selected_tab, selected_site) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Modules ----------------------------------------------------------------
    loc_server <- mod_select_location_server(
      "select_location",
      selected_tab,
      selected_site
    )

    # Show/hide secret tabs ---------------------------------------------------
    observe({
      if (selected_tab() == "map") {
        updateTabsetPanel(inputId = "tabset_param", selected = "param_n")
        updateTabsetPanel(inputId = "tabset_score", selected = "show_score")
        updateTabsetPanel(inputId = "tabset_dates", selected = "by_year")
      } else if (selected_tab() == "report_card") {
        updateTabsetPanel(inputId = "tabset_param", selected = "param_short")
        updateTabsetPanel(inputId = "tabset_score", selected = "show_score")
        updateTabsetPanel(inputId = "tabset_dates", selected = "by_year")
      } else if (selected_tab() == "graphs") {
        updateTabsetPanel(inputId = "tabset_param", selected = "param_n")
        updateTabsetPanel(inputId = "tabset_score", selected = "hide_score")
        updateTabsetPanel(inputId = "tabset_dates", selected = "by_range")
      } else {
        updateTabsetPanel(inputId = "tabset_param", selected = "param_all")
        updateTabsetPanel(inputId = "tabset_score", selected = "hide_score")
        updateTabsetPanel(inputId = "tabset_dates", selected = "by_range")
      }
    }) %>%
      bindEvent(selected_tab())

    observe({
      if (length(dat_list$depth) < 2) {
        updateTabsetPanel(inputId = "tabset_depth", selected = "depth_null")
      } else if (selected_tab() %in% c("map", "graphs")) {
        updateTabsetPanel(inputId = "tabset_depth", selected = "depth_n")
      } else {
        updateTabsetPanel(inputId = "tabset_depth", selected = "depth_all")
      }
    }) %>%
      bindEvent(selected_tab())

    # Filter scores -----------------------------------------------------------
    df_score_filter <- reactive({
      req(input$select_year)

      df <- dplyr::filter(df_score, Year == input$select_year)

      return(df)
    })

    # Return data -------------------------------------------------------------
    return(
      list(
        sites_all = reactive({
          loc_server$sites_all()
        }),
        sites_n = reactive({
          loc_server$sites_n()
        }),
        site_list = reactive({
          loc_server$site_list()
        }),
        param_all = reactive({
          input$select_param_all
        }),
        param_n = reactive({
          input$select_param_n
        }),
        param_short = reactive({
          input$select_param_short
        }), # used for report card
        score = reactive({
          input$chk_nascore
        }),
        depth_n = reactive({
          input$select_depth_n
        }),
        depth_all = reactive({
          input$select_depth_all
        }),
        year = reactive({
          input$select_year
        }),
        year_range = reactive({
          input$select_year_range
        }),
        month_range = reactive({
          input$select_month
        }),
        df_score_f = reactive({
          df_score_filter()
        })
      )
    )
  })
}

## To be copied in the UI
# mod_sidebar_ui("sidebar_1")

## To be copied in the server
# mod_sidebar_server("sidebar_1")

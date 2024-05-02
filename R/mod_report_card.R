#' report_card UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_report_card_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::card(
      min_height = 250,
      full_screen = FALSE,
      h2("Report Card"),
      shinycssloaders::withSpinner(
        reactable::reactableOutput(ns("table")),
        type = 5
      )
    )
  )
}

#' report_card Server Functions
#'
#' @noRd
mod_report_card_server <- function(id, selected_var){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    drop_rows <- c("Year", "Site_ID", "Unit", "score_typ", "score_num",
                   "Latitude", "Longitude")

    df_default <- df_score %>%
      dplyr::filter(Year == max(Year)) %>%
      dplyr::select(!dplyr::all_of(drop_rows))

    df_filter <- reactive({
      # Define var
      df <- selected_var$df_score_f()
      param <- c(selected_var$param_short(), "-")
      sites <- selected_var$sites_all()
      if (length(sites) == 0 | length(param) == 1) { return(df_default[0,]) }

      # Update dataframe
      df <- df %>%
        dplyr::filter(Parameter %in% param) %>%
        dplyr::filter(Site_ID %in% sites) %>%
        dplyr::select(!dplyr::all_of(drop_rows))

      if(!selected_var$score()){
        df <- dplyr::filter(df,
          !(score_str %in% c("No Data Available", "No Threshold Established")))
      }

      return(df)
    })

    # Table ------------------------------------------------------------------
    output$table <- reactable::renderReactable({
      reactable_table(df_default)
    })

    # Update table
    observe({ reactable::updateReactable("table", data = df_filter()) }) %>%
      bindEvent(df_filter())
  })
}

## To be copied in the UI
# mod_report_card_ui("report_card_1")

## To be copied in the server
# mod_report_card_server("report_card_1")

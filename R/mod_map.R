#' map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_map_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::card(
      min_height = 250,
      full_screen = FALSE,
      h2("Map"),
      shinycssloaders::withSpinner(
        reactable::reactableOutput(ns("table")),
        type = 5
      )
    )
  )
}

#' map Server Functions
#'
#' @noRd
mod_map_server <- function(id, selected_var){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    df_default <- df_score %>%
      dplyr::filter(Year == max(Year))

    df_param <- reactive({
      # Define var
      df <- selected_var$df_score_f()
      param <- c(selected_var$param_n(), "-")

      # Update dataframe
      df <- dplyr::filter(df, Parameter %in% param)

      return(df)
    })

    map_type <- reactive({
      chk <- "No Threshold Established" %in% df_param()$score_str
      if (chk) { return("score_num") } else { return("score_str") }
    })

    df_map <- reactive({
      # Define var
      df <- df_param()
      sites <- selected_var$sites_all()

      # Update dataframe
      df <- dplyr::filter(df, Site_ID %in% sites)

      if(!selected_var$score() & map_type() == "score_num"){
        df <- dplyr::filter(df, !is.na(score_num))
      } else if (!selected_var$score()) {
        df <- dplyr::filter(df,
          !(score_str %in% c("No Data Available", "No Threshold Established")))
      }

      return(df)
    })

    # Table ------------------------------------------------------------------
    output$table <- reactable::renderReactable({
      reactable_table(df_default,
        hide_cols = c("Year", "Site_ID", "score_typ", "Latitude", "Longitude"))
    })

    # Update table
    observe({ reactable::updateReactable("table", data = df_map()) }) %>%
      bindEvent(df_map())

  })
}

## To be copied in the UI
# mod_map_ui("map_1")

## To be copied in the server
# mod_map_server("map_1")

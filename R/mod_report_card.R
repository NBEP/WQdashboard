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
      if (nrow(df) == 0 | length(param) == 1) { return(df_default[0,]) }

      # Update dataframe
      df <- df %>%
        dplyr::select(!dplyr::all_of(drop_rows)) %>%
        dplyr::filter(Parameter %in% param)

      if(!selected_var$score()){
        df <- dplyr::filter(df,
          !(score_str %in% c("No Data Available", "No Threshold Established")))
      }

      return(df)
    })

    # Table ------------------------------------------------------------------
    output$table <- reactable::renderReactable({
      reactable::reactable(
        df_default,
        highlight = TRUE,
        defaultSorted = c("Site_Name", "Parameter"),
        defaultColDef = reactable::colDef(
          header = function(value) gsub("_", " ", value, fixed = TRUE),
          headerStyle = list(background = "#f7f7f8")
        ),
        columns = list(
          Site_Name = reactable::colDef(
            rowHeader = TRUE,
            sticky = "left"),
          score_str = reactable::colDef(
            name = "Score"#,
            # style = function(score) {
            #   background <- NA
            #   fontStyle <- "Normal"
            #   if (score == "Excellent") {
            #     background <- "#afccec"
            #   } else if (score == "Good") {
            #     background <- "#cbe4e7"
            #   } else if (score == "Fair") {
            #     background <- "#ffffe0"
            #   } else if (score == "Poor") {
            #     background <- "#f9cfb4"
            #   } else if (score %in% c("No Data Available",
            #                           "No Threshold Established")) {
            #     fontStyle <- "italic"
            #   }
            #   list(background = background, fontStyle = fontStyle)
            # }
          ))
        )
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

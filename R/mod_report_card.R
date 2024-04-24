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
      reactable::reactableOutput(ns("table"))
    )
  )
}

#' report_card Server Functions
#'
#' @noRd
mod_report_card_server <- function(id, selected_var){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    df_filter <- reactive({
      req(selected_var$param_all())

      df <- selected_var$df_score_f()

      col_order <- c("Site_Name", "Site_ID", "Town", "County", "State",
                     "Watershed", "Group", "Depth", "Parameter")
      col_order <- intersect(col_order, colnames(df))

      df <- df %>%
        dplyr::filter(Parameter %in% selected_var$param_all() |
                        is.na(Parameter)) %>%
        dplyr::mutate(Score = case_when(
          !is.na(score_str) ~ score_str,
          !is.na(score_num) ~ "No Threshold Established",
          TRUE ~ "No Data")) %>%
        dplyr::mutate(Parameter = dplyr::if_else(
          is.na(Parameter), "-", Parameter)) %>%
        dplyr::select(c(dplyr::all_of(col_order), "Score")) %>%
        dplyr::arrange(Site_Name, Parameter)

      return(df)
    })

    df_table <- reactive({
      df <- df_filter()

      if(!selected_var$score()){
       df <- df %>%
         dplyr::filter(!(Score %in% c("No Data", "No Threshold Established")))
      }

      return(df)
    })

    # Table ------------------------------------------------------------------
    output$table <- reactable::renderReactable({
      reactable::reactable(
        df_table(),
        highlight = TRUE,
        defaultColDef = reactable::colDef(
          header = function(value) gsub("_", " ", value, fixed = TRUE),
          headerStyle = list(background = "#f7f7f8")
        ),
        columns = list(
          Score = reactable::colDef(
            style = function(score) {
              background <- NA
              fontStyle <- "Normal"
              if (score == "Excellent") {
                background <- "#afccec"
              } else if (score == "Good") {
                background <- "#cbe4e7"
              } else if (score == "Fair") {
                background <- "#ffffe0"
              } else if (score == "Poor") {
                background <- "#f9cfb4"
              } else if (score %in% c("No Data", "No Threshold Established")) {
                fontStyle <- "italic"
              }
              list(background = background, fontStyle = fontStyle)
            }
          ))
        )
    })

  })
}

## To be copied in the UI
# mod_report_card_ui("report_card_1")

## To be copied in the server
# mod_report_card_server("report_card_1")

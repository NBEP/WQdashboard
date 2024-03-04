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
 
  )
}
    
#' report_card Server Functions
#'
#' @noRd 
mod_report_card_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_report_card_ui("report_card_1")
    
## To be copied in the server
# mod_report_card_server("report_card_1")

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
 
  )
}
    
#' graphs Server Functions
#'
#' @noRd 
mod_graphs_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_graphs_ui("graphs_1")
    
## To be copied in the server
# mod_graphs_server("graphs_1")

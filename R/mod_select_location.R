#' select_location UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_select_location_ui <- function(id){
  ns <- NS(id)
  tagList(
    "???!?",
    textOutput(ns("argh")),
    "????"
    # select_dropdown(ns("select_state"),
    #                 label = h3("Select States"),
    #                 choices = df_sites$State,
    #                 choice_names = state.name[match(df_sites$State, state.abb)]),
    # select_dropdown(ns("select_town"),
    #                 label = h3("Select Towns"),
    #                 choices = df_sites$Town_Code),
    # select_dropdown(ns("select_watershed"),
    #                 label = h3("Select Watersheds"),
    #                 choices = df_sites$Watershed),
    # conditionalPanel(
    #   condition = paste0("output['", ns("selected_tab"), "'] != 'graphs'"),
    #   select_dropdown(ns("select_site"),
    #                   label = h3("Select Sites"),
    #                   choices = df_sites$Site_ID,
    #                   choice_names = df_sites$Site_Name)
    # ),
    # conditionalPanel(
    #   condition = paste0("output['", ns("selected_tab"), "'] == 'graphs'"),
    #   select_dropdown(ns("select_site"),
    #                   label = HTML(paste(h3("Select Sites"),
    #                                      "Select up to three sites")),
    #                   choices = df_sites$Site_ID,
    #                   choice_names = df_sites$Site_Name,
    #                   maxOptions = 3)
    # )
  )
}

#' select_location Server Functions
#'
#' @noRd
mod_select_location_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Send vars to ui
    output$argh <- renderText("Hello world")
    outputOptions(output, "argh", suspendWhenHidden = FALSE)

  })
}

## To be copied in the UI
# mod_select_location_ui("select_location_1")

## To be copied in the server
# mod_select_location_server("select_location_1")

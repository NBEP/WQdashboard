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

  # define vars
  loc_choices <- list_loc_choices()

  tagList(
    tabsetPanel(
      id = ns("tabset_toggle"),
      type = "hidden",
      selected = set_loc_tab(loc_choices),
      tabPanelBody(
        "toggle",
        radioButtons(
          ns("loc_type"),
          label = h3("Filter Sites"),
          choices = loc_choices)),
      tabPanelBody(
        "notoggle",
        h3("Filter Sites")),
      tabPanelBody("blank")
    ),
    tabsetPanel(
      id = ns("tabset_loc"),
      type = "hidden",
      selected = loc_choices[1],
      tabPanelBody(
        "town",
        conditionalPanel(
          condition = paste(length(df_sites$State), "> 0"),
          select_dropdown(
            ns("select_state"),
            label = h4("Select State"),
            choices = df_sites$State,
            choice_names = state.name[match(df_sites$State, state.abb)])),
        conditionalPanel(
          condition = paste(length(df_sites$Town_Code), "> 0"),
          select_dropdown(
            ns("select_town"),
            label = h4("Select Town"),
            choices = df_sites$Town_Code))),
      tabPanelBody(
        "watershed",
        select_dropdown(
          ns("select_watershed"),
          label = h4("Select Watershed"),
          choices = df_sites$Watershed)),
      tabPanelBody("blank")
    ),
    tabsetPanel(
      id = ns("tabset_sites"),
      type = "hidden",
      tabPanelBody(
        "sites_all",
        select_dropdown(
          ns("select_sites_all"),
          label = h3("Select Sites"),
          choices = df_sites$Site_ID,
          choice_names = df_sites$Site_Name)),
      tabPanelBody(
        "sites_n",
        select_dropdown(
          ns("select_sites_n"),
          label = h3("Select Site"),
          choices = df_sites$Site_ID,
          choice_names = df_sites$Site_Name,
          multiple = FALSE))
    )
  )
}

# -----------------------------------------------------------------------------

#' select_location Server Functions
#'
#' @noRd
mod_select_location_server <- function(id, selected_tab){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      updateTabsetPanel(
        inputId = "tabset_loc",
        selected = input$loc_type)
    }) %>%
      bindEvent(input$loc_type)

    observe({
      if (selected_tab() == "graphs") {
        updateTabsetPanel(inputId = "tabset_sites", selected = "sites_n")
      } else {
        updateTabsetPanel(inputId = "tabset_sites", selected = "sites_all")
      }
    }) %>%
      bindEvent(selected_tab())

    # Default site list for towns, watersheds
    default_sites <- create_site_list(df_sites)
    locval <- reactiveValues(
      town_sites = default_sites,
      watershed_sites = default_sites)

    # Update select_town OR town_sites when select state
    observe({
      if (!is.null(df_sites$Town_Code)) {
        choices <- update_town_list(state_list = input$select_state)

        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "select_town",
          choices = choices,
          selected = choices)
      } else {
        choices <- update_site_list(
          filter_col = "State",
          filter_list = input$select_state)
        locval$town_sites <- choices
      }
    }) %>%
      bindEvent(input$select_state)

    # Update town_sites when select town
    observe({
      choices <- update_site_list(
        filter_col = "Town_Code",
        filter_list = input$select_town)
      locval$town_sites <- choices
    }) %>%
      bindEvent(input$select_town)

    # Update watershed_sites when select watershed
    observe({
      choices <- update_site_list(
        filter_col = "Watershed",
        filter_list = input$select_watershed)
      locval$watershed_sites <- choices
    }) %>%
      bindEvent(input$select_watershed)

    # Update select_sites_all and select_sites_n
    observe({
      if (input$loc_type == "town") {
        choices = locval$town_sites
      } else {
        choices = locval$watershed_sites
      }
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "select_sites_all",
        choices = choices,
        selected = choices)
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "select_sites_n",
        choices = choices,
        selected = choices[1])
    }) %>%
      bindEvent(c(locval$town_sites, locval$watershed_sites, input$loc_type))

    return(
      list(
        sites_all = reactive({ input$select_sites_all }),
        sites_n = reactive({ input$select_sites_n })
      )
    )

  })
}

## To be copied in the UI
# mod_select_location_ui("select_location_1")

## To be copied in the server
# mod_select_location_server("select_location_1")

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
        leaflet::leafletOutput(ns('map')),
        type = 5
      ),
      h2("Table"),
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
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    df_default <- df_score %>%
      dplyr::filter(Year == max(Year))

    df_param <- reactive({
      req(selected_var$param_n())

      # Define var
      df <- selected_var$df_score_f()
      param <- c(selected_var$param_n(), "-")

      # Update dataframe
      df <- dplyr::filter(df, Parameter %in% param)

      if ("Depth" %in% colnames(df_score)) {
        df <- dplyr::filter(df, Depth == selected_var$depth_n())
      }

      return(df)
    })

    map_type <- reactive({
      chk <- "No Threshold Established" %in% df_param()$score_str
      if (chk) { return("score_num") } else { return("score_str") }
    })

    df_map <- reactive({
      req(selected_var$sites_all())

      # Define var
      df <- df_param()
      sites <- selected_var$sites_all()

      # Update dataframe
      df <- dplyr::filter(df, Site_ID %in% sites)

      if(!selected_var$score()) {
        df <- dplyr::filter(df, !is.na(score_num))
      }

      return(df)
    })

    # Map --------------------------------------------------------------------
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::fitBounds(
          min(df_sites$Longitude), # Lon min
          min(df_sites$Latitude), # Lat min
          max(df_sites$Longitude), # Lon max
          max(df_sites$Latitude) # Lat max
          ) %>%
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldTopoMap) %>%
        leaflet.extras2::addSpinner() %>%
        leaflet::addScaleBar(position='bottomleft')
    })

    # * Add sites ----
    observe({
      if (nrow(df_map()) == 0) {
        leaflet::leafletProxy("map") %>%
          leaflet::clearMarkers()
      } else if (map_type() == "score_num") {
        leaflet::leafletProxy("map") %>%
          leaflet::clearMarkers() %>%
          leaflet::addMarkers(
            data = df_map(),
            lng = ~Longitude,
            lat = ~Latitude,
            # Icons
            icon = ~leaflet::icons(
              iconUrl = num_symbols(df_param(), df_map()),
              iconWidth = 20,
              iconHeight = 20),
            # Label
            label = ~Site_Name,
            labelOptions = leaflet::labelOptions(textsize = "15px"),
            # Popup
            popup = ~paste0(
              popup_loc,
              "<br/><br/><b>", selected_var$param_n(), "</b>",
              popup_score),
            # Accessibility
            options = leaflet::markerOptions(alt = ~alt))
      } else {
        leaflet::leafletProxy("map") %>%
          leaflet::clearMarkers() %>%
          leaflet::addMarkers(
            data = df_map(),
            lng = ~Longitude,
            lat = ~Latitude,
            # Icons
            icon = ~leaflet::icons(
              iconUrl = cat_pal(df_param())[score_str],
              iconWidth = 20,
              iconHeight = 20),
            # Label
            label = ~Site_Name,
            labelOptions = leaflet::labelOptions(textsize = "15px"),
            # Popup
            popup = ~paste0(
              popup_loc,
              "<br/><br/><b>", selected_var$param_n(), "</b>",
              popup_score),
            # Accessibility
            options = leaflet::markerOptions(alt = ~alt))
      }
    }) %>%
      bindEvent(df_map())

    # * Add legend ----
    observe({
      if (map_type() == "score_num") {
        leaflet::leafletProxy("map") %>%
          leaflet::clearControls() %>%
          leaflegend::addLegendNumeric(
            pal = num_pal(df_param()),
            values = df_param()$score_num,
            title = htmltools::tags$div(
              paste0(selected_var$param_n(), " (", selected_var$year(), ")"),
              style = 'font-size: 18px'),
            shape = "rect",
            orientation = "vertical",
            bins = 5,
            numberFormat = function(x) {
              paste(
                prettyNum(x, format = "f", big.mark = ",", scientific = FALSE),
                param_unit(selected_var$param_n()))
            },
            naLabel = "No Data Available",
            labelStyle = 'font-size: 14px;',
            position = 'topright'
          )
      } else {
        leaflet::leafletProxy("map") %>%
          leaflet::clearControls() %>%
          leaflegend::addLegendImage(
            images = cat_pal(df_param(), TRUE),
            labels = cat_labels(df_param()),
            width = 20,
            height = 20,
            orientation = 'vertical',
            title = htmltools::tags$div(
              paste0(selected_var$param_n(), " (", selected_var$year(), ")"),
              style = 'font-size: 18px'),
            labelStyle = 'font-size: 14px;',
            position = 'topright'
          )
      }
    }) %>%
      bindEvent(df_param())

    # Table ------------------------------------------------------------------
    output$table <- reactable::renderReactable({
      reactable_table(df_default,
        hide_cols = c("Year", "Site_ID", "Parameter", "Unit", "score_typ",
                      "Latitude", "Longitude", "popup_loc", "popup_score",
                      "alt"))
    })

    # Update table
    observe({ reactable::updateReactable("table", data = df_map()) }) %>%
      bindEvent(df_map())

    # To update score_num name --- have to use "meta" attribute

  })
}

## To be copied in the UI
# mod_map_ui("map_1")

## To be copied in the server
# mod_map_server("map_1")

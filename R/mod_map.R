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
    bslib::navset_card_tab(
      id = ns("tabset"),
      title = h2(textOutput(ns("title"))),
      full_screen = TRUE,
      bslib::nav_panel(
        "Map",
        leaflet::leafletOutput(ns('map'))
        ),
      bslib::nav_panel(
        "Table",
        reactable::reactableOutput(ns("table"))
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

    # Set title ----
    output$title <- renderText({
      paste0(selected_var$param_n(), " (", selected_var$year(), ")")
    })

    # Static variables ----
    drop_col <- c("Year", "Site_ID", "Parameter", "Unit", "score_typ",
                  "Latitude", "Longitude", "popup_loc", "popup_score", "alt")

    df_default <- df_score %>%
      dplyr::filter(Year == max(Year)) %>%
      dplyr::select(!any_of(drop_col))

    # Reactive variables ----
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
      layer_list <- NA

      map <- leaflet::leaflet() %>%
        leaflet::fitBounds(
          min(df_sites$Longitude), # Lon min
          min(df_sites$Latitude), # Lat min
          max(df_sites$Longitude), # Lon max
          max(df_sites$Latitude) # Lat max
        ) %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap) %>%
        leaflet::addScaleBar(position="bottomleft")

      # * Add watershed ----
      if (exists("shp_watershed")) {
        map <- map %>%
          leaflet::addPolygons(
            data = shp_watershed,
            layerId = shp_watershed,
            # Label
            label = ~Name,
            labelOptions = leaflet::labelOptions(textsize = "15px"),
            # Stroke
            color = '#6B8091',
            weight = 0.5,
            smoothFactor = 0.5,
            opacity = 0.9,
            # Fill
            fillOpacity = 0.4,
            fillColor = '#C6D9EC',
            # Highlight
            highlightOptions = leaflet::highlightOptions(
              fillColor = '#EEF4F9',
              weight = 1.2,
              bringToFront = FALSE),
            # misc
            group = 'Watersheds')
        layer_list <- "Watersheds"
      }

      # * Add rivers ----
      if (exists("shp_river")) {
        map <- map %>%
          leaflet::addMapPane("river_pane", zIndex = 420) %>%
          leaflet::addPolylines(
            data = shp_river,
            layerId = shp_river,
            # Label
            label = ~Label,
            labelOptions = leaflet::labelOptions(textsize = "15px"),
            popup = ~Popup,
            # Stroke
            color = "#6B8091",
            weight = 2,
            smoothFactor = 1,
            opacity = 1,
            # Highlight
            highlightOptions = leaflet::highlightOptions(
              color = "#3e576c",
              weight = 3,
              bringToFront = TRUE),
            # misc
            options = leaflet::pathOptions(pane = "river_pane"),
            group = "Rivers")
        layer_list <- c(layer_list, "Rivers")
        layer_list <- layer_list[!is.na(layer_list)]
      }

      # * Add layer toggle ----
      if (!all(is.na(layer_list))) {
        map <- map %>%
          leaflet::addLayersControl(
            overlayGroups = layer_list,
            position='topleft')
      }

      return(map)
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
            layerId = ~Site_ID,
            # Icons
            icon = ~leaflet::icons(
              iconUrl = num_symbols(df_param(), df_map()),
              iconWidth = 20,
              iconHeight = 20),
            # Label
            label = ~alt,
            labelOptions = leaflet::labelOptions(textsize = "15px"),
            # Popup
            popup = ~paste0(
              popup_loc,
              "<br><br><b>", selected_var$param_n(), "</b>",
              popup_score, "<br>",
              actionLink(
                ns("graph_link"),
                label = "View Trends",
                onclick = paste0(
                  'Shiny.setInputValue("', ns("graph_link"),
                  '", (Math.random() * 1000) + 1);')
                )
            ),
            # Accessibility
            options = leaflet::markerOptions(
              alt = ~alt,
              riseOnHover = TRUE)
          )
      } else {
        leaflet::leafletProxy("map") %>%
          leaflet::clearMarkers() %>%
          leaflet::addMarkers(
            data = df_map(),
            lng = ~Longitude,
            lat = ~Latitude,
            layerId = ~Site_ID,
            # Icons
            icon = ~leaflet::icons(
              iconUrl = cat_pal(df_param())[score_str],
              iconWidth = 20,
              iconHeight = 20),
            # Label
            label = ~alt,
            labelOptions = leaflet::labelOptions(textsize = "15px"),
            # Popup
            popup = ~paste0(
              popup_loc,
              "<br><br><b>", selected_var$param_n(), "</b>",
              popup_score, "<br>",
              actionLink(
                ns("graph_link2"),
                label = "View Trends",
                onclick = paste0(
                  'Shiny.setInputValue("', ns("graph_link"),
                  '", (Math.random() * 1000) + 1);')
              )
            ),
            # Accessibility
            options = leaflet::markerOptions(
              alt = ~alt,
              riseOnHover = TRUE)
          )
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
              paste(selected_var$param_n(),
                find_unit(selected_var$param_n())),
              style = 'font-size: 18px'),
            shape = "rect",
            orientation = "vertical",
            bins = 5,
            naLabel = "No Data Available",
            labelStyle = 'font-size: 14px;',
            position = 'topright',
            group = "Legend"
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
              selected_var$param_n(),
              style = 'font-size: 18px'),
            labelStyle = 'font-size: 14px;',
            position = 'topright',
            group = "Legend"
          )
      }
    }) %>%
      bindEvent(df_param())

    # Table ------------------------------------------------------------------
    val <- reactiveValues(
      df = df_default,
      show_score = TRUE,
      col_title = "Average",
      count = 0)

    col_title <- reactive({
      df <- df_param() %>%
        dplyr::filter(!is.na(score_num))

      if (nrow(df) == 0) { return("Average") }

      par_type <- df$score_typ[1]
      par_unit <- df$Unit[1]

      col_title <- pretty_unit(par_type, par_unit)

      return(col_title)
    })

    observe({
      if (val$count < 2) {
        val$count <- val$count + 1
        val$df <- dplyr::select(df_map(), !dplyr::any_of(drop_col))
        val$col_title <- col_title()
        if (map_type() == "score_str") {
          val$show_score <- TRUE
        } else {
          val$show_score <- FALSE
        }
      }
    }) %>%
      bindEvent(input$tabset)

    output$table <- reactable::renderReactable({
      reactable_table(
        val$df,
        show_score = val$show_score,
        col_title = val$col_title)
    })

    # * Update table ----
    observe({reactable::updateReactable("table",
        data = dplyr::select(df_map(), !dplyr::any_of(drop_col)),
        meta = list(col_title = col_title())
      )
    }) %>%
      bindEvent(df_map())

    observe({
      if (map_type() == "score_str") {
        hideCols(ns("table"), as.list(""))
      } else {
        hideCols(ns("table"), as.list("score_str"))
      }
    }) %>% bindEvent(map_type())

    # Return data -------------------------------------------------------------
    selected_site <- reactive({ input$map_marker_click$id }) %>%
      bindEvent(input$graph_link)

    return(
      list(
        graph_link = reactive({ input$graph_link }),
        site = reactive({ selected_site() })
      )
    )

  })
}

## To be copied in the UI
# mod_map_ui("map_1")

## To be copied in the server
# mod_map_server("map_1")

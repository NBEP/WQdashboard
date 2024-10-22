#' download UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_download_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::card(
      h2("Suggested Citation"),
      textOutput(ns("citation")),
      h2("Download Data"),
      select_dropdown(
        id = ns("dl_format"),
        label = "Select Format",
        choices = c("xls", "csv", "tsv"),
        choice_names = c("Excel", "csv", "tsv"),
        sort_choices = FALSE,
        multiple = FALSE),
      downloadButton(
        ns("dl"),
        "Download",
        style = "width:fit-content")
    )
  )
}

#' download Server Functions
#'
#' @noRd
mod_download_server <- function(id, selected_var){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Citation ----
    output$citation <- renderText({
        paste0(
          org_info$name, ", ", format(Sys.time(), "%Y"), ", ",
          org_info$program_name,
          " data available on the world wide web, accessed [",
          format(Sys.time(), "%B %d, %Y"), "], at URL [", org_info$url, "]."
        )
    })

    # Format data ----
    # * Site data ----
    sites <- reactive({
      req(selected_var$sites_all)

      df <- df_sites_all %>%
        replace(is.na(.), -999999) %>%
        dplyr::filter(Site_ID %in% selected_var$sites_all)

      old_colnames <- c("Site_ID", "Site_Name", "Latitude", "Longitude",
        "County", "State", "Location_Type")
      new_colnames <- c("Monitoring Location ID", "Monitoring Location Name",
        "Monitoring Location Latitude (DD.DDDD)",
        "Monitoring Location Longitude (-DDD.DDDD)",
        "Monitoring Location County Code", "State Code",
        "Monitoring Location Type")
      names(new_colnames) <- old_colnames
      field_subs <- new_colnames[intersect(colnames(df), names(new_colnames))]

      df <- dplyr::rename_with(df, ~ field_subs, names(field_subs))

      return(df)
    })

    # * Numeric data ----
    data_num <- reactive({
      req(selected_var$year_range())
      req(selected_var$param_all())

      df <- df_data_all %>%
        dplyr::filter(
          Year >= selected_var$year_range()[1] &
            Year <= selected_var$year_range()[2]) %>%
        dplyr::filter(Parameter %in% selected_var$param_all)

      if ("Depth" %in% colnames(df_data_all)) {
        df <- dplyr::filter(df, Depth %in% selected_var$depth_all())
      }

      df2 <- sites() %>%
        dplyr::select("Site_ID", "Site_Name")

      # Merge data
      df_merge <- merge(df, df2, by="Site_ID")

      return(df_merge)
    })

    data_text <- reactive({
      if (exists("df_cat_data")) {
        df <- df_cat_data
      } else {
        df <- NULL
      }
      return(df)
    })

    # List data
    data_list <- reactive({
      data_list <- list(
        sites = sites(),
        data_num = data_num(),
        data_text = data_text()
        )
      data_list <- data_list[lengths(data_list) > 0]

      return(data_list)
    })


  })
}

## To be copied in the UI
# mod_download_ui("download_1")

## To be copied in the server
# mod_download_server("download_1")

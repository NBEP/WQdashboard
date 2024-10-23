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
      cit <- org_info$citation
      cit <- gsub("YEAR", format(Sys.time(), "%Y"), cit)
      cit <- gsub("DATE", format(Sys.time(), "%B %d, %Y"), cit)

      return(cit)
    })

    # Format data ----
    # * Site data ----
    site_col <- c("Site_ID", "Site_Name", "Latitude", "Longitude", "County",
      "State", "Location_Type")
    names(site_col) <- c("Monitoring Location ID", "Monitoring Location Name",
      "Monitoring Location Latitude (DD.DDDD)",
      "Monitoring Location Longitude (-DDD.DDDD)",
      "Monitoring Location County Code", "State Code",
      "Monitoring Location Type")

    sites <- reactive({
      req(selected_var$sites_all)

      df <- df_sites_all %>%
        replace(is.na(.), -999999) %>%
        dplyr::filter(Site_ID %in% selected_var$sites_all)

      field_subs <- site_col[intersect(colnames(df), names(site_col))]
      df <- dplyr::rename_with(df, ~ field_subs, names(field_subs))

      return(df)
    })

    # * Data ----
    data_col <- c("Site_ID", "Site_Name", "Activity_Type", "Date", "Depth",
      "Depth_Unit", "Depth_Category", "Parameter", "Result", "Result_Unit",
      "Qualifier")
    names(data_col) <- c("Monitoring Location ID", "Monitoring Location Name",
      "Activity Type", "Activity Start Date", "Activity Depth/Height Measure",
      "Activity Depth/Height Unit", "Activity Relative Depth Name",
      "Characteristic Name", "Result Value", "Result Unit",
      "Result Measure Qualifier")

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

      # Rename columns
      field_subs <- data_col[intersect(colnames(df_merge), names(data_col))]
      df_merge <- dplyr::rename_with(df_merge, ~ field_subs, names(field_subs))

      return(df_merge)
    })

    data_text <- reactive({
      req(selected_var$year_range())
      req(selected_var$param_all())

      if (!exists("df_data_extra")) { return(NULL) }

      df <- df_data_extra %>%
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

      # Rename columns
      field_subs <- data_col[intersect(colnames(df_merge), names(data_col))]
      df_merge <- dplyr::rename_with(df_merge, ~ field_subs, names(field_subs))

      return(df_merge)
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

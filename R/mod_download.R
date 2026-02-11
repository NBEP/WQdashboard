#' download UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_download_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::card(
      h2("Suggested Citation"),
      textOutput(ns("citation")),
      downloadButton(
        ns("dl"),
        "Download (csv)",
        style = "width:fit-content"
      )
    )
  )
}

#' download Server Functions
#'
#' @noRd
mod_download_server <- function(
    id, sites, results, txt_citation, selected_var
  ) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Citation ----
    output$citation <- renderText({
      cit <- gsub("YEAR", format(Sys.time(), "%Y"), txt_citation)
      gsub("DATE", format(Sys.time(), "%B %d, %Y"), cit)
    })

    # Download ----
    output$dl <- downloadHandler(
      filename = function() {"water_quality.zip"},

      content = function(file) {
        # Download progress notification
        id <- showNotification(
          "Downloading data...",
          duration = NULL,
          closeButton = FALSE
        )
        on.exit(removeNotification(id), add = TRUE)

        # Set temp directory
        temp_dir <- file.path(tempdir(), as.integer(Sys.time()))
        dir.create(temp_dir)

        # Filter data
        site_list <- selected_var$sites_all()
        param <- selected_var$param_all()
        year <- selected_var$year_all()
        depth_list <- c(NA, selected_var$depth_all())

        sites_filter <- sites |>
          dplyr::filter(.data$Site_ID %in% !!site_list) |>
          importwqd:::prep_pdf(na_sub="")

        results_filter <- results |>
          dplyr::filter(
            .data$Site_ID %in% !!site_list,
            .data$Parameter %in% !!param,
            .data$Year %in% !!year
          ) |>
          dplyr::select(!"Year")

        if (isTruthy(depth_list)) {
          results_filter <- results_filter |>
            dplyr::filter(.data$Depth_Category %in% !!depth_list)
        }

        results_filter <- results_filter |>
          importwqd:::prep_pdf(na_sub="")

        # Create file
        temp_sites <- paste0(temp_dir, "/site_metadata.csv")
        temp_results <- paste0(temp_dir, "/result_data.csv")

        readr::write_excel_csv(sites_filter, temp_sites)
        readr::write_excel_csv(results_filter, temp_results)

        zip::zip(
          zipfile = file,
          files = dir(temp_dir),
          root = temp_dir
        )
      }
    )


  })
}

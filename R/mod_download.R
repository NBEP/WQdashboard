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
      h2("Download Data"),
      importwqd::dropdown(
        id = ns("dl_format"),
        label = "Select Format",
        choices = c("xls", "csv", "tsv"),
        choice_names = c("Excel", "csv", "tsv"),
        sorted = FALSE,
        multiple = FALSE
      ),
      downloadButton(
        ns("dl"),
        "Download",
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

    # Filter data ----
    df_sites_filter <- reactive({
      req(selected_var$sites_all)

      site_list <- selected_var$sites_all()

      dplyr::filter(sites, .data$Site_ID %in% !!site_list)
    })

    df_data_filter <- reactive({
      req(selected_var$sites_all)
      req(selected_var$param_all())
      req(selected_var$year_all())

      site_list <- selected_var$sites_all()
      param <- selected_var$param_all()
      year <- selected_var$year_all()
      depth_list <- c(NA, selected_var$depth_all())

      dat <- results |>
        dplyr::filter(
          .data$Site_ID %in% !!site_list,
          .data$Parameter %in% !!param,
          .data$Year %in% !!year
        ) |>
        dplyr::select(!"Year")

      if (isTruthy(depth_list)) {
        dat <- dplyr::filter(dat, .data[["Depth Category"]] %in% !!depth_list)
      }

      dat
    })
  })
}

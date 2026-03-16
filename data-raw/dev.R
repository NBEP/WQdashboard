#' Dev
#'
#' @description This script generates the files in the folder "custom_format".
#' It DOES NOT need to be run again unless those files are lost or modified and
#' you want to generate fresh files. IT WILL OVERWRITE EXISTING FILES IN
#' "custom_format".
#'
#' @noRd

# CODE -------------------------------------------------------------------------
devtools::load_all()
library("readr")

# Set temp wd ----
wd <- getwd()
setwd(paste0(getwd(), "/data-raw"))
if (!dir.exists("custom_format")) {
  dir.create("custom_format")
}
setwd(paste0(getwd(), "/custom_format"))

# Add varname templates to data-raw ----
df_param <- wqformat:::varnames_parameters |>
  dplyr::filter(!is.na(.data$wqdashboard)) |>
  dplyr::select("wqdashboard") |>
  dplyr::mutate(
    "wqdashboard" = dplyr::if_else(
      grepl("|", .data$wqdashboard, fixed = TRUE),
      stringr::str_split_i(.data$wqdashboard, "\\|", 1),
      .data$wqdashboard
    )
  ) |>
  dplyr::mutate("Custom" = NA) |>
  dplyr::arrange(.data$wqdashboard)

readr::write_csv(df_param, "varnames_parameters.csv", na = "")

df_units <- wqformat:::varnames_units |>
  dplyr::filter(!is.na(.data$wqdashboard)) |>
  dplyr::select("wqdashboard") |>
  dplyr::mutate(
    "wqdashboard" = dplyr::if_else(
      grepl("|", .data$wqdashboard, fixed = TRUE),
      stringr::str_split_i(.data$wqdashboard, "\\|", 1),
      .data$wqdashboard
    )
  ) |>
  dplyr::mutate("Custom" = NA) |>
  dplyr::arrange(.data$wqdashboard)

readr::write_csv(df_units, "varnames_units.csv", na = "")

df_activity <- wqformat:::varnames_activity |>
  dplyr::filter(!is.na(.data$wqdashboard)) |>
  dplyr::select("wqdashboard") |>
  dplyr::mutate(
    "wqdashboard" = dplyr::if_else(
      grepl("|", .data$wqdashboard, fixed = TRUE),
      stringr::str_split_i(.data$wqdashboard, "\\|", 1),
      .data$wqdashboard
    )
  ) |>
  dplyr::mutate("Custom" = NA) |>
  dplyr::arrange(.data$wqdashboard)

readr::write_csv(df_activity, "varnames_activity.csv", na = "")

df_qual <- system.file(
  "extdata",
  "varnames_qualifiers.csv",
  package = "wqformat"
)

df_qual <- readr::read_csv(df_qual, show_col_types = FALSE) |>
  dplyr::filter(!is.na(.data$wqdashboard)) |>
  dplyr::mutate("Custom" = NA) |>
  dplyr::select("wqdashboard", "Custom", "Description") |>
  dplyr::arrange(.data$wqdashboard)

readr::write_csv(df_qual, "varnames_qualifiers.csv", na = "")

# Add state, EPA thresholds to inst/extdata/ ----
setwd(wd)
thresholds <- importwqd:::dat_thresholds

readr::write_csv(thresholds, "inst/extdata/default_thresholds.csv", na = "")

# Clean up ----
rm(list = ls())

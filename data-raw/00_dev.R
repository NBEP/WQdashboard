# Dev script.
#
# README: This script defines a series of universal variables necessary for the
#  app to run. DOES NOT NEED TO BE RUN UNLESS VARIABLES CHANGE.

library("readr")
library("dplyr")
remotes::install_github("massbays-tech/wqformat")
remotes::install_github("nbep/importwqd")

# Add varname templates to data-raw ----
df_param <- wqformat:::varnames_parameters %>%
  dplyr::filter(!is.na(.data$wqdashboard)) %>%
  dplyr::select("wqdashboard") %>%
  dplyr::mutate(
    "wqdashboard" = dplyr::if_else(
      grepl("|", .data$wqdashboard, fixed = TRUE),
      stringr::str_split_i(.data$wqdashboard, "\\|", 1),
      .data$wqdashboard
    )
  ) %>%
  dplyr::mutate("Custom" = NA) %>%
  dplyr::arrange(.data$wqdashboard)

readr::write_csv(df_param, "data-raw/varnames_parameters.csv", na = "")

df_units <- wqformat:::varnames_units %>%
  dplyr::filter(!is.na(.data$wqdashboard)) %>%
  dplyr::select("wqdashboard") %>%
  dplyr::mutate(
    "wqdashboard" = dplyr::if_else(
      grepl("|", .data$wqdashboard, fixed = TRUE),
      stringr::str_split_i(.data$wqdashboard, "\\|", 1),
      .data$wqdashboard
    )
  ) %>%
  dplyr::mutate("Custom" = NA) %>%
  dplyr::arrange(.data$wqdashboard)

readr::write_csv(df_units, "data-raw/varnames_units.csv", na = "")

# Upload state, epa threshold metadata ----
df_state <- readr::read_csv(
  "inst/extdata/state_thresholds.csv",
  show_col_types = FALSE
) %>%
  importwqd::qaqc_thresholds() %>%
  importwqd::format_thresholds()

df_epa <- readr::read_csv(
  "inst/extdata/epa_thresholds.csv",
  show_col_types = FALSE
) %>%
  importwqd::qaqc_thresholds() %>%
  importwqd::format_thresholds()

official_thresholds <- dplyr::bind_rows(df_state, df_epa)
usethis::use_data(official_thresholds, overwrite = TRUE)

# Clean up ----
rm(list=ls())

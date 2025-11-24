#' Upload thresholds - OPTIONAL
#'
#' @description Run this script to upload custom threshold metadata. Custom
#' thresholds take priority over the state and federal thresholds included in
#' the base app.
#'
#' @param threshold_csv Path to csv file containing threshold metadata.
#' @param in_format Format used for parameters and units. Accepted formats
#' include wqdashboard, WQX, MassWateR,  RI_DEM (Rhode Island DEM), RI_WW
#' (RI Watershed Watch), MA_BRC (Blackstone River Coalition), ME_DEP (Maine
#' DEP), and ME_FOCB (Friends of Casco Bay). To use a custom format, set
#' `in_format` to "custom" and update the files `varnames_results.csv` and
#' `varnames_units.csv` in the `data-raw` folder.
#'
#' @noRd

# Custom thresholds:
threshold_csv <- "data-raw/test_threshold_BRC.csv"
in_format <- "MA_BRC"

# CODE ------------------------------------------------------------------------
library("readr")
library("dplyr")
remotes::install_github("massbays-tech/wqformat")
remotes::install_github("nbep/importwqd")

message("Uploading thresholds")
custom_thresholds <- readr::read_csv(threshold_csv, show_col_types = FALSE)

if (nrow(custom_thresholds) == 0) {
  stop("Empty dataframe, no thresholds found")
}

if (in_format == "custom") {
  df_param <- readr::read_csv(
    "data-raw/varnames_parameters.csv",
    show_col_types = FALSE
  )
  df_unit <- readr::read_csv(
    "data-raw/varnames_units.csv",
    show_col_types = FALSE
  )

  custom_thresholds <- custom_thresholds %>%
    wqformat::update_var("Parameter", df_param$Custom, df_param$wqdashboard) %>%
    wqformat::update_var("Unit", df_unit$Custom, df_unit$wqdashboard)
}

# QAQC data, save to inst/extdata
custom_thresholds <- importwqd::qaqc_thresholds(custom_thresholds, in_format)
message("Exporting data to inst/extdata/custom_thresholds.csv")
readr::write_csv(custom_thresholds, "inst/extdata/custom_thresholds.csv")

# Format data, save to data
custom_thresholds <- importwqd::format_thresholds(custom_thresholds)
usethis::use_data(custom_thresholds, overwrite = TRUE)
message("Finished processing data")

rm(list=ls())

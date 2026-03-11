#' Upload thresholds - OPTIONAL
#'
#' @description If you want to assign custom parameter thresholds for a specific
#' site or group, fill out the template `inst/extdata/template_results.csv`
#' then run this script.
#'
#' @param threshold_csv Path to csv file containing threshold metadata.
#'
#' @param in_format Format used for parameters and units. Accepted formats:
#'
#' * wqdashboard
#' * WQX
#' * MassWateR
#' * RI_DEM (Rhode Island DEM)
#' * RI_WW (RI Watershed Watch)
#' * MA_BRC (Blackstone River Coalition)
#' * ME_DEP (Maine DEP)
#' * ME_FOCB (Friends of Casco Bay)
#'
#' To use a custom format, set `in_format` to "custom" and update the following
#' files:
#' * `data-raw/custom_format/varnames_parameters.csv`
#' * `data-raw/custom_format/varnames_units.csv`
#'
#' @noRd

threshold_csv <- "thresholds.csv"
in_format <- "wqdashboard"

# CODE ------------------------------------------------------------------------
devtools::load_all()
library("readr")

message("Uploading thresholds")
df_raw <- readr::read_csv(
  paste0("data-raw/", threshold_csv),
  show_col_types = FALSE
)

if (nrow(df_raw) == 0) {
  stop("Empty dataframe, no thresholds found")
}

if (in_format == "custom") {
  df_param <- readr::read_csv(
    "data-raw/custom_format/varnames_parameters.csv",
    show_col_types = FALSE
  )
  df_unit <- readr::read_csv(
    "data-raw/custom_format/varnames_units.csv",
    show_col_types = FALSE
  )

  df_raw <- importwqd::prep_thresholds(df_raw, df_param, df_unit)

  in_format <- "wqdashboard"
}

# QAQC data, save to inst/extdata
custom_thresholds <- importwqd::qaqc_thresholds(df_raw, in_format)
message("Exporting thresholds to inst/extdata/custom_thresholds.csv")
readr::write_csv(custom_thresholds, "inst/extdata/custom_thresholds.csv")

# Format data, save to data
df_thresholds <- importwqd::format_thresholds(custom_thresholds)
usethis::use_data(df_thresholds, overwrite = TRUE)
message("Done")

rm(list = ls(all.names = TRUE))

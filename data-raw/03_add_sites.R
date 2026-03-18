#' Upload site metadata
#'
#' @description Run this script to upload site metadata. Site metadata must be
#' saved as a csv in the `data-raw` folder.
#'
#' @param sites_csv Path to csv file containing site metadata.
#'
#' @param in_format Input format. Accepted formats:
#'
#' * wqdashboard
#' * WQX
#' * MassWateR
#' * RI_WW (RI Watershed Watch)
#' * MA_BRC (Blackstone River Coalition)
#' * ME_FOCB (Friends of Casco Bay)
#'
#' To use a custom format, set `in_format` to "custom" and update
#' `data-raw/custom_format/colnames_sites.csv`
#'
#' @param default_state State name or abbreviation. Blank rows in column "State"
#' will be set to `default_state`. Set to `NA` to leave blank rows as-is.
#' as-is.
#'
#' @noRd

sites_csv <- "demo_ww_sites.csv"
in_format <- "ri_ww"
default_state <- "Rhode Island"

# CODE ------------------------------------------------------------------------
devtools::load_all()
library("readr")

# Import data
df_raw <- readr::read_csv(
  paste0("data-raw/", sites_csv),
  show_col_types = FALSE
)

# Process data
if (tolower(in_format) == "custom") {
  message("Reformatting data...")
  df_colnames <- readr::read_csv(
    "data-raw/custom_format/colnames_sites.csv",
    show_col_types = FALSE
  )

  df_raw <- importwqd::prep_sites(df_raw, df_colnames)
} else if (tolower(in_format) != "wqdashboard") {
  df_raw <- wqformat::format_sites(
    df_raw,
    in_format,
    "wqdashboard",
    drop_extra_col = FALSE
  )
}

df_sites_all <- importwqd::qaqc_sites(df_raw, default_state)
usethis::use_data(df_sites_all, overwrite = TRUE)

df_sites <- importwqd::format_sites(df_sites_all)
usethis::use_data(df_sites, overwrite = TRUE)
message("Done")

rm(list = ls(all.names = TRUE))

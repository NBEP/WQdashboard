#' Upload site metadata
#'
#' @description Run this script to upload site metadata. Site metadata must be
#' saved as a csv in the `data-raw` folder.
#'
#' @param sites_csv Path to csv file containing site metadata.
#' @param in_format Input format. Accepted formats include wqdashboard,
#' WQX, MassWateR,  RI_WW (RI Watershed Watch), MA_BRC (Blackstone River
#' Coalition), and ME_FOCB (Friends of Casco Bay). To use a custom format, set
#' `in_format` to "custom" and update the file `colnames_sites.csv` in the
#' `data-raw` folder.
#' @param default_state State name or abbreviation. Blank values in the column
#' "State" will be replaced with `default_state`. Set to `NA` to ignore this
#' feature.
#'
#' @noRd

# Site data:
sites_csv <- "data-raw/test_sites_ww_saltponds.csv"
in_format <- "RI_WW"
default_state <- "Rhode Island"

# CODE ------------------------------------------------------------------------
library("readr")
remotes::install_github("massbays-tech/wqformat")
remotes::install_github("nbep/importwqd")

# Import data
df <- readr::read_csv(sites_csv, show_col_types = FALSE)

# Process data
supported_formats <- c("ma_brc", "masswater", "me_focb", "ri_ww", "wqx")

if (tolower(in_format) %in% supported_formats) {
  df_sites_all <- wqformat::format_sites(
    df,
    in_format,
    "wqdashboard",
    drop_extra_col = FALSE
  )
} else if (tolower(in_format) != "wqdashboard") {
  message("Reformatting data...")
  df_colnames <- readr::read_csv(
    "data-raw/colnames_sites.csv",
    show_col_types = FALSE
  )
  old_col <- df_colnames[["Former Column Names"]]
  new_col <- df_colnames[["wqdashboard Column Names"]]

  df_sites_all <- wqformat::rename_col(df_sites_all, old_col, new_col)
}

df_sites_all <- importwqd::qaqc_sites(df_sites_all, default_state)
usethis::use_data(df_sites_all, overwrite = TRUE)

df_sites <- importwqd::format_sites(df_sites_all)
usethis::use_data(df_sites, overwrite = TRUE)
message("Finished processing data")

rm(list=ls())

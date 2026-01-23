# Add Categorical Results
#
# README: Run this script to add or update categorical water quality data. This
#  data will be available to download but will NOT be included on any maps,
#  report cards, or graphs.
#
# Date abbreviations:
#  b Abbreviated month name
#  B Full month name
#  d Day of the month
#  H Hour
#  m Month
#  M Minute
#  p AM/PM
#  S Second
#  z Timezone

# parameter data:
results_csv <- "test_cat_data_brc.csv"
in_format <- "Blackstone_River_Coalition"
date_format <- "Y-m-d H:M"
timezone <- Sys.timezone()

categorical <- TRUE

overwrite_existing <- TRUE

# CODE ------------------------------------------------------------------------
library("readr")
library("dplyr")
library("remotes")
remotes::install_github("massbays-tech/wqformat")
remotes::install_github("nbep/importwqd")

source("R/utils_import_data.R")
load("data/df_sites_all.rda")
load("data/df_sites.rda")

if (!overwrite_existing) {
  load("data/df_data_extra.rda")
}


# Import, format data ----
df_raw <- readr::read_csv(
  paste0("data-raw/", results_csv),
  show_col_types = FALSE,
  guess_max = Inf
)

if (in_format == "custom") {
  df_colnames <- readr::read_csv(
    "data-raw/colnames_results.csv",
    show_col_types = FALSE
  )
  df_param <- readr::read_csv(
    "data-raw/varnames_parameters.csv",
    show_col_types = FALSE
  )
  df_unit <- readr::read_csv(
    "data-raw/varnames_units.csv",
    show_col_types = FALSE
  )
  df_qual <- readr::read_csv(
    "data-raw/varnames_qualifiers.csv",
    show_col_types = FALSE
  )
  df_activity <- readr::read_csv(
    "data-raw/varnames_activity.csv",
    show_col_types = FALSE
  )

  df_raw <- df_raw %>%
    importwqd::prep_results(
      df_colnames, df_param, df_unit, df_qual, df_activity
    ) %>%
    wqformat::format_wqd_results(date_format, categorical)
} else if (in_format == "wqdashboard") {
  df_raw <- df_raw %>%
    wqformat::format_wqd_results(date_format, categorical)
} else {
  df_raw <- df_raw %>%
    wqformat::format_results(
      in_format, "wqdashboard", date_format, timezone,
      drop_extra_col = FALSE
    )
}

# QAQC data ----
df_qaqc <- importwqd::qaqc_cat_results(df_raw, df_sites_all)

# Combine datasets (if overwrite_existing is FALSE)
if (!overwrite_existing) {
  df_qaqc <- dplyr::bind_rows(df_data_extra, df_qaqc) %>%
    unique()

  if (!categorical) {
    df_qaqc <- df_qaqc %>%
      wqformat::standardize_units(
        "Parameter",
        "Result",
        "Result_Unit",
        warn_only = FALSE
      ) %>%
      wqformat::standardize_units_across(
        "Result_Unit",
        "Detection_Limit_Unit",
        c("Lower_Detection_Limit", "Upper_Detection_Limit"),
        warn_only = FALSE
      ) %>%
      unique()
  }
}

# Upload df_data_extra
df_data_extra <- df_qaqc
usethis::use_data(df_data_extra, overwrite = TRUE)
message("Saved df_data_extra")

# Set sidebar variables ----
message("Setting sidebar dropdown lists")
varlist <- importwqd::sidebar_var(df_sites, df_data, df_score, df_data_extra)

usethis::use_data(varlist, internal = TRUE, overwrite = TRUE)
message("Done")

rm(list = ls())

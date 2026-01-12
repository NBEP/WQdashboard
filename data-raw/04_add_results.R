# Update Data
#
# README: Run this script to add or update water quality data.
#
# Date abbreviations:
#  b Abbreviated month name (eg Feb)
#  B Full month name (eg February)
#  d Day of the month
#  y Year without century (eg 24)
#  Y Year with century (eg 2024)
#  H Hour
#  m Month
#  M Minute
#  p AM/PM
#  S Second
#  z Timezone

# parameter data:
results_csv <- "test_data_ww_clean.csv"
in_format <- "RI_WW"
date_format <- "m/d/Y"
timezone <- Sys.timezone()

overwrite_existing <- TRUE
recalculate_score <- FALSE

# CODE ------------------------------------------------------------------------
library("readr")
library("dplyr")
library("remotes")
remotes::install_github("massbays-tech/wqformat")
remotes::install_github("nbep/importwqd")

source("R/utils_import_data.R")
load("data/df_sites_all.rda")
load("data/df_sites.rda")
load("data/official_thresholds.rda")
load("data/custom_thresholds.rda")

if (!overwrite_existing) {
  load("data/df_data_all.rda")
  load("data/df_score.rda")
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
    wqformat::format_wqd_results(date_format)
} else if (in_format == "wqdashboard") {
  df_raw <- wqformat::format_wqd_results(df_raw, date_format)
} else {
  df_raw <- df_raw %>%
    wqformat::format_results(
      in_format, "wqdashboard", date_format, timezone,
      drop_extra_col = FALSE
    )
}

# QAQC data ----
df_qaqc <- importwqd::qaqc_results(df_raw, df_sites_all)
chk_years <- unique(df_qaqc$Year)

# Combine datasets (if overwrite_existing is FALSE)
if (!overwrite_existing) {
  df_qaqc <- dplyr::bind_rows(df_data_all, df_qaqc) %>%
    unique() %>%
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

if (nrow(df_qaqc) > 250000) {
  warning(
    "Large dataset. Website may experience performance issues.",
    call. = FALSE
  )
}

# Upload df_data_all
df_data_all <- df_qaqc
usethis::use_data(df_data_all, overwrite = TRUE)
message("Saved df_data_all")

# Format data ----
df_thresh <- official_thresholds
if (exists("custom_thresholds")) {
  df_thresh <- dplyr::bind_rows(custom_thresholds, official_thresholds)
}

df_temp <- importwqd::format_results(df_data_all, df_sites_all, df_thresh)

df_data <- df_temp %>%
  dplyr::select(!c("Calculation", "Good", "Fair"))

usethis::use_data(df_data, overwrite = TRUE)
message("Saved df_data")

# Update df_sites -----
chk <- c(
  setdiff(df_sites$Site_ID, unique(df_data$Site_ID)),
  setdiff(unique(df_data$Site_ID), df_sites$Site_ID)
)
if (length(chk) > 0) {
  message("Updating df_sites")
  df_sites <- importwqd::format_sites(df_sites_all, unique(df_data$Site_ID))
  usethis::use_data(df_sites, overwrite = TRUE)
}

# Calculate scores ----
chk <- !overwrite_existing & !recalculate_score & exists("df_score")
if (chk) {
  df_temp <- dplyr::filter(df_temp, .data$Year %in% chk_years)
  df_old <- dplyr::filter(df_score, !.data$Year %in% chk_years)
}

df_score <- importwqd::score_results(df_temp, df_sites)

if (chk) {
  df_score <- rbind(df_old, df_score)
}

usethis::use_data(df_score, overwrite = TRUE)
message("Saved df_score")

# Set sidebar variables ----
message("Setting sidebar dropdown lists")
state <- NULL
town <- NULL
watershed <- NULL

if ("State" %in% colnames(df_sites)) {
  state <- unique(df_sites$State)
}
if ("Town" %in% colnames(df_sites)) {
  town <- unique(df_sites$Town)
}
if ("Watershed" %in% colnames(df_sites)) {
  watershed <- unique(df_sites$Watershed)
}

param_short <- df_score %>%
  dplyr::filter(
    !.data$score_str %in% c("No Data Available", "No Threshold Established")
  )
param_short <- sort(unique(param_short$Parameter))

depth <- NULL
if ("Depth" %in% colnames(df_data)) {
  depth <- sort_depth(df_data$Depth)
}

loc_list <- list(
  state = state,
  town = town,
  watershed = watershed
)
dat_list <- list(
  param = sort(unique(df_data$Parameter)),
  param_short = param_short,
  depth = depth,
  year = sort(unique(df_data$Year)),
  month = sort_months(df_data$Month)
)

usethis::use_data(loc_list, dat_list, internal = TRUE, overwrite = TRUE)
message("Done")

rm(list = ls())

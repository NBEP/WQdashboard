# Update Data
#
# README: Run this script to add or update water quality data.
#
# Date abbreviations:
#  b Abbreviated month name
#  B Full month name
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
results_csv <- "test_data_ww_all.csv"
in_format <- "RI_WW"
date_format = "m/d/Y"

overwrite_existing = TRUE

# CODE ------------------------------------------------------------------------
devtools::load_all()

df <- readr::read_csv(
  paste0("data-raw/", results_csv),
  show_col_types = FALSE,
  guess_max = Inf
)

if (in_format != "WQdashboard") {
  df <- preformat_results(df, in_format)
}

# QAQC data
df <- qaqc_results(df, date_format)
chk_years <- unique(df$Year)

# Combine datasets (if not overwriting)
if (!overwrite_existing) {
  keep_cols <- intersect(colnames(df), colnames(df_data_all))
  df <- dplyr::select(df, dplyr::all_of(keep_cols))
  df2 <- dplyr::select(df_data_all, dplyr::all_of(keep_cols))
  df <- rbind(df, df2) %>% unique()
  df <- standardize_units(df)
}

# Upload data (all)
df_data_all <- df
usethis::use_data(df_data_all, overwrite = TRUE)
message("Saved df_data_all")

# Format, upload data (short)
df <- format_results(df)

df_data <- df
usethis::use_data(df_data, overwrite = TRUE)
message("Saved df_data")

# Calculate, upload scores
if (overwrite_existing) {
  df_score <- format_score(df)
} else {
  score_old <- dplyr::filter(df_score, !Year %in% chk_years)
  df_new <- dplyr::filter(df, Year %in% chk_years)
  score_new <- format_score(df_new)
  df_score <- rbind(score_old, score_new)
}
usethis::use_data(df_score, overwrite = TRUE)
message("Saved df_score \n\nFinished processing data")


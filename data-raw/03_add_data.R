# Update Data
#
# README: Run this script to add or update water quality data.
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
#  Z Timezone

# parameter data:
wq_data <- "test_data_brc.csv"
date_format = "m/d/Y H:M"
default_state = "MA"

# CODE ------------------------------------------------------------------------
devtools::load_all()

df <- read.csv(paste0("data-raw/", wq_data),
    na.strings=c("","NA"),
    check.names=FALSE) %>%
  dplyr::mutate_if(is.character, trimws)

# Check results
df <- qaqc_results(df, date_format)

# Import results
df_data_all <- df
usethis::use_data(df_data_all, overwrite = TRUE)
message("Saved df_data_all")

df_data <- format_df_data(df_data_all)
usethis::use_data(df_data, overwrite = TRUE)
message("Saved df_data")

df_score <- format_df_score(df_data, default_state)
usethis::use_data(df_score, overwrite = TRUE)
message("Saved df_score \n\nFinished processing data")


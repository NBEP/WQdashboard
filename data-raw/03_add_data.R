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
wq_data <- "test_data_masswater.csv"
date_format = "m/d/Y"
default_state = "MA"

# CODE ------------------------------------------------------------------------
devtools::load_all()

df_results <- read.csv(paste0("data-raw/", wq_data),
    na.strings=c("","NA"),
    check.names=FALSE) %>%
  dplyr::mutate_if(is.character, trimws)
df_results <- QAQC_results(df_results, date_format)
format_results(df_results, default_state)

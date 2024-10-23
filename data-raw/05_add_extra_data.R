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

# Set variables:
wq_data <- "data-raw/test_cat_data_brc.csv"
in_format <- "Blackstone_River_Coalition"
date_format = "Y-m-d H:M"

colnames_extra <- "data-raw/colnames_extra.csv"

# CODE ------------------------------------------------------------------------
devtools::load_all()
library('readr')

df <- readr::read_csv(wq_data, show_col_types = FALSE)

colnames_extra <- readr::read_csv(colnames_extra, show_col_types = FALSE) %>%
  dplyr::relocate(WQdashboard) %>%
  dplyr::relocate(WQdashboard_short, .after = last_col())

# Import results
df_data_extra <- qaqc_results(df, colnames_extra, in_format, date_format)

usethis::use_data(df_data_extra, overwrite = TRUE)
message("Saved df_data_extra")


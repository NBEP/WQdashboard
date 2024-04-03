# Update Data
#
# README: Run this script to add or update water quality data.

# parameter data:
wq_data <- "test_data_ww.csv"
date_format = "Y/m/d H:M:S + z"

# CODE ------------------------------------------------------------------------
devtools::load_all()

df_results <- read.csv(paste0("data-raw/", wq_data),
    na.strings=c("","NA"),
    check.names=FALSE) %>%
  dplyr::mutate_if(is.character, trimws)

# Check data
df_results <- QAQC_results(df_results, date_format)
format_results(df_results)

# Check for match between data, sites

# Calculate scores

# Update Data
#
# README: Run this script to add or update water quality data.

# parameter data:
wq_data <- "test_data_brc.csv"
date_format = "m/d/Y H:M"

# CODE ------------------------------------------------------------------------
devtools::load_all()

df_results <- read.csv(paste0("data-raw/", wq_data), check.names=FALSE) %>%
  dplyr::mutate_if(is.character, trimws)

# Check data
df_results <- QAQC_results(df_results, date_format)

# Check for match between data, sites

# Calculate scores

# Save data
msg <- "Uploading data..."
usethis::use_data(df_results, overwrite = TRUE)
message(msg, "OK")

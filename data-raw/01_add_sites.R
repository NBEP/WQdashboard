# Update Site Data
#
# README: Run this script once to add site data. This script does not need to
#  be rerun unless you add, move, or remove a site. Please ensure sites.csv is
#  up to date before running script.

# Site data:
sites <- "sites.csv"

# CODE ------------------------------------------------------------------------
library(dplyr)
source('R/fct_QAQC_sites.R')
source('R/utils_QAQC.R')

# Define vars
df_sites <- read.csv(paste0("data-raw/", sites), check.names=FALSE)
df_cols <- read.csv("data-raw/column_substitutions.csv") %>%
  filter(File == "sites")

# Process data
df_sites <- QAQC_sites(df_sites, df_cols$Old_Column, df_cols$New_Column)

msg <- "Uploading data..."
usethis::use_data(df_sites, overwrite = TRUE)
message(msg, "OK")

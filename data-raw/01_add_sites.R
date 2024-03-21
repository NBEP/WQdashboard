# Update Site Data
#
# README: Run this script once to add site data. This script does not need to
#  be rerun unless you add, move, or remove a site. Please ensure sites.csv is
#  up to date before running script.

# Site data:
sites <- "sites.csv"

# CODE ------------------------------------------------------------------------
devtools::load_all()

# Import data
df_sites <- read.csv(paste0("data-raw/", sites), check.names=FALSE) %>%
  dplyr::mutate_if(is.character, trimws) %>%
  head()  # DELETE LATER WHEN DONE TESTING

# Check data
df_sites <- QAQC_sites(df_sites)

# Save data
msg <- "Uploading data..."
usethis::use_data(df_sites, overwrite = TRUE)
message(msg, "OK")

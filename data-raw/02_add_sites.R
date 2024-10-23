# Update Site Data
#
# README: Run this script once to add site data. This script does not need to
#  be rerun unless you add, move, or remove a site. Please ensure sites.csv is
#  up to date before running script.

# Site data:
sites <- "test_sites_brc.csv"

# CODE ------------------------------------------------------------------------
devtools::load_all()

df <- read.csv(paste0("data-raw/", sites),
    na.strings=c("","NA"),
    check.names=FALSE) %>%
  dplyr::mutate_if(is.character, trimws)

# Check for errors
df <- qaqc_sites(df)

df_sites_all <- df
usethis::use_data(df_sites_all, overwrite = TRUE)

df_sites <- format_sites(df)
usethis::use_data(df_sites, overwrite = TRUE)
message("\nFinished processing data")

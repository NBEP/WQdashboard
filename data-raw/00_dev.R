# Dev script.
#
# README: This script defines a series of universal variables necessary for the
#  app to run. DOES NOT NEED TO BE RERUN UNLESS VARIABLES CHANGE, DO NOT EDIT.

library(dplyr)

colnames_sites <- read.csv("data-raw/colnames_sites.csv") %>%
  dplyr::mutate_if(is.character, trimws) %>%
  dplyr::relocate(WQdashboard) %>%
  dplyr::relocate(WQdashboard_short, .after = last_col())
usethis::use_data(colnames_sites, overwrite = TRUE)

colnames_results <- read.csv("data-raw/colnames_results.csv") %>%
  dplyr::mutate_if(is.character, trimws) %>%
  dplyr::relocate(WQX) %>%
  dplyr::relocate(WQX_short, .after = last_col())
usethis::use_data(colnames_results, overwrite = TRUE)

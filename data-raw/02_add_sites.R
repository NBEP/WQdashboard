# Update Site Data
#
# README:
# Run this script to add site data. Site data must be saved as a csv and saved
# in the data-raw folder.
#
# FORMATTING
# Standard formats: WQdashboard, WQX, MassWateR, RI_WW
#
# TO USE CUSTOM FORMAT, add a column to data-raw/colnames_sites.csv

# Site data:
sites_csv <- "test_sites_ww_saltponds.csv"
in_format <- "RI_WW"

# CODE ------------------------------------------------------------------------
devtools::load_all()

# Import data
df <- readr::read_csv(paste0("data-raw/", sites_csv), show_col_types = FALSE)

# Process data
if (in_format != "WQdashboard") {
  df_colnames <- readr::read_csv(
    "data-raw/colnames_sites.csv",
    show_col_types = FALSE
  ) %>%
    dplyr::filter(!is.na(WQdashboard)) %>%
    dplyr::select_if(function(x) !(all(is.na(x))))  # drop empty columns


  message("Prepping site data...\n")
  var_sub <- find_var_names(df_colnames, in_format, "WQdashboard")
  df <- rename_col(df, var_sub$old_names, var_sub$new_names)
  message("\t", length(var_sub$old_names), " columns renamed")

  rm(df_colnames)
}

df <- qaqc_sites(df)

df_sites_all <- df
usethis::use_data(df_sites_all, overwrite = TRUE)

df_sites <- format_sites(df)
usethis::use_data(df_sites, overwrite = TRUE)
message("\nFinished processing data")

# Add thresholds
#
# README: (add detailed instructions)

# Custom thresholds:
thresholds_csv <- "test_threshold_brc.csv"
parameter_format <- "Blackstone_River_Coalition"

# CODE ------------------------------------------------------------------------
devtools::load_all()
library("readr")

# Import data
df <- readr::read_csv(
  paste0("data-raw/", thresholds_csv),
  show_col_types = FALSE)

if (nrow(df) == 0) { stop("Empty dataframe") }

# Process data
if (!parameter_format %in% c("WQX", "WQdashboard")) {
  df_varnames <- readr::read_csv(
    "data-raw/varnames_parameters.csv",
    show_col_types = FALSE
  ) %>%
    dplyr::filter(!is.na(WQX)) %>%
    dplyr::select_if(function(x) !(all(is.na(x))))  # drop empty columns

  message("Renaming parameters...\n")
  var_sub <- find_var_names(df_varnames, parameter_format, "WQX")
  df <- rename_all_var(df, "Parameter", var_sub$old_names, var_sub$new_names)
  message("\t", length(var_sub$old_names), " parameters renamed")

  rm(df_varnames)
}

custom_thresholds <- qaqc_thresholds(df)

usethis::use_data(custom_thresholds, overwrite = TRUE)
message("\nFinished processing data")

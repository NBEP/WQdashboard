#' format_sites
#'
#' @description Preps site data for use in the app, saves df_sites and
#'   df_sites_all.
#'
#' @param df Input dataframe.
#'
#' @return The return value, if any, from executing the function.
format_sites <- function(df){
  # Prep data for download ----------------------------------------------------
  message("Formatting df_sites_all...\n")
  df_sites_all <- df
  usethis::use_data(df_sites_all, overwrite = TRUE)
  message("df_sites_all saved")

  # Prep data for app ----------------------------------------------------------
  message("\nFormatting df_sites...\n")
  # Drop extra columns
  field_all <- colnames_sites$WQdashboard
  field_keep <- intersect(field_all, colnames(df))
  chk <- length(df) - length(field_keep)
  if (chk > 0) {
    df <- dplyr::select(df, all_of(field_keep))  # Drop extra columns
    message("\t", toString(chk), " columns removed")
  }
  df <- check_val_count(df, "State")
  df <- check_val_count(df, "Town")
  df <- check_val_count(df, "Watershed")
  # Add new columns
  if ("Town" %in% colnames(df)) {
    if ("State" %in% colnames(df)) {
      df <- dplyr::mutate(df, Town_Code = paste0(Town, ", ", State))
    } else {
      df <- dplyr:: mutate(df, Town_Code = Town)
    }
    message("\tAdded column Town_Code")
  } else if ("County" %in% colnames(df)) {
    if ("State" %in% colnames(df)) {
      df <- dplyr::mutate(df, County_Code = paste(County, "County,", State))
    } else {
      df <- dplyr:: mutate(df, County_Code = paste(County, "County"))
    }
    message("\tAdded column County_Code")
  }
  df_sites <- df
  usethis::use_data(df_sites, overwrite = TRUE)
  message("df_sites saved")
}

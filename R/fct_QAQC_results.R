#' QAQC_results
#'
#' @description Run quick quality control check on water quality data. Runs the
#'   following checks:
#'   * Checks column names, renames columns as needed
#'   * Checks all mandatory columns are present
#'   * Checks for missing values
#'   * Ensures date column in properly formatted
#'   * Renames parameters to match WQX standard
#'
#' @param df Input dataframe.
#' @param date_format Date format as string.
#'
#' @return Updated dataframe.
QAQC_results <- function(df, date_format=NULL){
  # Define variables ----------------------------------------------------------
  field_all <- colnames_results$WQdashboard
  field_need <- colnames_results$WQdashboard_short
  field_need <- field_need[!field_need == ""]
  field_optional <- dplyr::setdiff(field_all, field_need)

  # QAQC columns --------------------------------------------------------------
  message("Checking data...\n")
  df <- update_column_names(df, colnames_results)
  check_column_missing(df, field_need)

  # QAQC column values ---------------------------------------------------------
  # Check missing data
  for (field in field_need) {
    check_val_missing(df, field = field)
  }
  field_check <- intersect(field_optional, colnames(df))
  field_check <- field_check[field_check != "Qualifier"]
  for (field in field_check) {
    check_val_missing(df, field = field, is_stop = FALSE)
  }
  # Check if all sites valid
  site_sites <- list_sites(df_sites)
  data_sites <- list_sites(df)

  chk <- data_sites %in% site_sites
  if(any(!chk)){
    extra_sites <- data_sites[!chk]
    stop("Site not in df_sites: ",
         paste(data_sites[!chk], collapse = ", "), call. = FALSE)
  }
  # Check column format
  check_val_numeric(df, field = "Result", exceptions="BDL")
  df <- format_date_col(df, date_format)

  # Check parameters, units
  df <- df %>%
    dplyr::mutate(
      Parameter = sapply(Parameter, function(x) rename_param(x))) %>%
    dplyr::mutate(
      Result_Unit = sapply(Result_Unit, function(x) rename_unit(x))
    )
  check_units(df)

  if(all(c("Depth", "Depth_Unit") %in% colnames(df))) {
    ok_units <- c("in", "ft", "cm", "m")
    chk <- df$Depth_Unit %in% c(ok_units, NA)
    chk <- skip_dq_rows(df, chk)
    chk <- skip_qc_rows(df, chk)
    if (any(!chk)) {
      rws <- which(!chk)
      stop("Invalid Depth_Unit. Acceptable values: ",
           paste(ok_units, collapse = ", "), ". Check rows ",
           paste(rws, collapse = ", "), call. = FALSE)
    }
  }

  message("\nQAQC complete")
  return(df)
}

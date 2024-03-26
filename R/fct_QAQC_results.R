#' QAQC_results
#'
#' @description Run quick quality control check on water quality data. Runs the
#'   following checks:
#'   * Checks column names, renames columns as needed
#'   * Checks all mandatory columns are present
#'   * Checks for missing values
#'
#' @param df Input dataframe.
#' @param date_format Date format as string.
#'
#' @return The return value, if any, from executing the function.
QAQC_results <- function(df, date_format=NULL){
  # Define variables ----------------------------------------------------------
  field_all <- colnames_results$WQdashboard
  field_need <- colnames_results$WQdashboard_short
  field_need <- field_need[!field_need == ""]
  field_optional <- dplyr::setdiff(field_all, field_need)

  # QAQC columns --------------------------------------------------------------
  message("Checking data...\n")
  df <- update_column_format(df, colnames_results)
  check_column_missing(df, field_need)
  # Drop extra columns
  field_keep <- intersect(field_all, colnames(df))
  chk <- length(df) - length(field_keep)
  if (chk > 0) {
    df <- dplyr::select(df, all_of(field_keep))
    message("\t", toString(chk), " columns removed")
  }

  # Drop rows ------------------------------------------------------------------
  if("Qualifier" %in% colnames(df)) {
    chk <- df$Qualifier %in% qaqc_fail

    if(any(chk)){
      df <- dplyr::filter(df, !Qualifier %in% qaqc_fail)
      message("\tDropped flagged data")
    }
    df <- dplyr::select(df, !Qualifier)
  }
  if("Activity_Type" %in% colnames(df)) {
    chk <- stringr::str_detect(df$Activity_Type, "Quality Control")

    if(any(chk)){
      df <- dplyr::filter(df,
        !stringr::str_detect(Activity_Type, "Quality Control"))
      message("\tDropped replicate, blank data")
    }
    df <- dplyr::select(df, !Activity_Type)
  }

  # QAQC column values ---------------------------------------------------------
  # Check missing data
  for (field in field_need) {
    check_val_missing(df, field = field)
  }
  field_check <- intersect(field_optional, colnames(df))
  for (field in field_check) {
    check_val_missing(df, field = field, is_stop = FALSE)
  }
  # Check sites
  site_sites <- df_sites$Site_ID
  data_sites <- unique(df$Site_ID)
  chk <- data_sites %in% site_sites
  if(any(!chk)){
    extra_sites <- data_sites[!chk]
    stop("Site not in df_sites: ",
         paste(data_sites[!chk], collapse = ", "), call. = FALSE)
  }
  # Check date format
  check_val_numeric(df, field = "Result")
  df <- format_date_col(df, date_format) %>%
    dplyr::mutate(Year = lubridate::year(Date)) %>%
    dplyr::mutate(Month = strftime(Date, "%B"))

  # Update parameter names
  df <- dplyr::mutate(df,
    Parameter = sapply(Parameter, function(x) rename_param(x)))

  # Check units
  # Check 1: One unit type per parameter

  #Check 2: Acceptable unit type

  return(df)
}

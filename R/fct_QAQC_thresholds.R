#' QAQC_thresholds
#'
#' @description Runs quality control on threshold dataframe.
#'
#' @param df Input dataframe.
#' @param extra_col Optional. Surplus that should be kept instead of deleted.
#'   Default NULL.
#'
#' @return Updated dataframe.
#'
#' @noRd
QAQC_thresholds <- function(df, extra_col = NULL){
  # Define variables ----------------------------------------------------------
  field_all <- c("Group", "Site_ID", "Depth_Category", "Parameter", "Unit",
    "Score", "Threshold_Min", "Threshold_Max", "Excellent", "Good", "Fair")
  field_need <- c("Parameter", "Unit", "Score")
  field_optional <- dplyr::setdiff(field_all, field_need)

  # QAQC columns --------------------------------------------------------------
  message("Checking thresholds...\n")
  check_column_missing(df, field_need)
  # Drop extra columns
  field_keep <- intersect(field_all, colnames(df))
  if (!is.null(extra_col)) {
    field_keep <- c(field_keep, extra_col)
  }
  chk <- length(df) - length(field_keep)
  if (chk > 0) {
    df <- dplyr::select(df, all_of(field_keep))  # Drop extra columns
    message("\t", toString(chk), " columns removed")
  }
  # Add missing columns...
  field_missing <- setdiff(field_optional, colnames(df))
  if (length(field_missing) > 0) {
    warning("Missing columns: ", paste(field_missing, collapse = ", "))
    for (field in field_missing) {
      df <- dplyr::mutate(df, {{field}} := NA)
    }
  }
  # QAQC values --------------------------------------------------------------
  for (field in field_need) { check_val_missing(df, field = field) }
  check_val_duplicate(df, field = c("Site_ID", "Group", "Parameter"))
  chk <- (is.na(df$Group) | is.na(df$Site_ID))
  if (any(!chk)) {
    rws <- which(!chk)
    stop("Group and Site_ID must be on seperate rows. Check rows:",
         paste(rws, collapse = ", "), call. = FALSE)
  }
  field_numeric = c("Threshold_Min", "Threshold_Max", "Excellent", "Good",
    "Fair")
  for (field in field_numeric) {
    check_val_numeric(df, field = field, exceptions =  NA)
  }
  # Update data format---------------------------------------------------------
  df <- df %>%
    dplyr::mutate(
      Parameter = sapply(Parameter, function(x) rename_param(x))) %>%
    dplyr::mutate(
      Unit = sapply(Unit, function(x) rename_unit(x)))

  message("\nQAQC complete")
  return(df)
}

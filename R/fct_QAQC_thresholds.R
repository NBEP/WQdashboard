#' QAQC_thresholds
#'
#' @description Runs quality control on threshold dataframe.
#'
#' @param df Input dataframe.
#' @param extra_col Optional. Surplus columns that should be kept instead of
#'   deleted. Default NULL.
#'
#' @return Updated dataframe.
#'
#' @noRd
QAQC_thresholds <- function(df, extra_col = NULL){
  # Define variables ----------------------------------------------------------
  field_need <- c("Parameter", "Unit")
  field_optional <- c("State", "Group", "Site_ID", "Depth_Category")
  field_tvalues <- c("Threshold_Min", "Threshold_Max", "Excellent", "Good",
    "Fair")
  field_all <- c(field_optional, field_need, "Min_Max_Mean", field_tvalues)

  # QAQC columns --------------------------------------------------------------
  message("Checking thresholds...\n")
  check_column_missing(df, field_need)
  chk <- intersect(field_tvalues, colnames(df))
  if (length(chk) < 1) {
    stop("Must include at least one threshold column. (eg ",
         paste(field_tvalues, collapse =", "), ")", call. = FALSE)
  }

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
  chk <- intersect(field_optional, colnames(df))
  for (field in chk) {
    chk <- is.na(df[field])
    if (all(chk)) {
      df <- dplyr::select(df, !{{field}})
      message("\tDropped empty column: ", field )
    }
  }
  # Add missing columns (need threshold values for calculate_score)
  field_missing <- setdiff(field_tvalues, colnames(df))
  if (length(field_missing) > 0) {
    warning("Replacing missing columns: ",
      paste(field_missing, collapse = ", "), call. = FALSE)
    for (field in field_missing) {
      df <- dplyr::mutate(df, {{field}} := NA)
    }
  }
  # QAQC values --------------------------------------------------------------
  for (field in field_need) { check_val_missing(df, field = field) }
  if (all(c("Group", "Site_ID") %in% colnames(df))) {
    chk <- (!is.na(df$Site_ID) & !is.na(df$Group))
    if (any(chk)) {
      rws <- which(chk)
      stop("Group and site thresholds must be on seperate rows. Check rows:",
           paste(rws, collapse = ", "), call. = FALSE)
    }
  }
  col_id <- c("State", "Group", "Site_ID", "Parameter")
  col_id <- intersect(col_id, colnames(df))
  check_val_duplicate(df, field = col_id)
  if ("State" %in% colnames(df)) {
    check_val_missing(df, "State", is_stop = FALSE)
  }
  if ("Min_Max_Mean" %in% colnames(df)) {
    check_val_missing(df, "Min_Max_Mean", is_stop = FALSE)
  }
  for (field in field_tvalues) {
    check_val_numeric(df, field = field)
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

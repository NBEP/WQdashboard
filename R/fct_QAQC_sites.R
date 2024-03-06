#' QAQC_sites
#'
#' @description Run quick quality control check on site data. Runs the
#'   following checks:
#'   * Checks column names, renames columns as needed
#'   * Checks all mandatory columns are present (Site_ID, Site_Name, Latitude,
#'     Longitude)
#'   * Checks for missing values
#'   * Checks for duplicate Site_ID, Latitude/Longitude
#'   * Checks that Latitude, Longitude are numeric
#'   * If present, checks that entries in State column are correctly spelled
#'   * Adds State_Abbr, State_Name, and Town_Code columns if State, Town columns
#'     included
#'
#' @param df Input dataframe.
#' @param old_fields List of old column names to replace.
#' @param new_fields List of new column names.
#'
#' @return The return value, if any, from executing the function.
QAQC_sites <- function(df, old_fields = "", new_fields = ""){
  # Define variables ----------------------------------------------------------
  field_need <- c("Site_ID", "Site_Name", "Latitude", "Longitude")
  field_optional <- c("Town", "State", "Watershed_Name", "Group")
  field_all <- c(field_need, field_optional)

  # QAQC columns --------------------------------------------------------------
  message("Running checks on site data...\n")
  check_column_duplicate(df)
  df <- check_column_name(df, old_fields, new_fields)
  check_column_missing(df, field_need)
  # Drop extra columns
  field_keep <- intersect(field_all, colnames(df))
  msg <- "\tChecking for extra columns..."
  chk <- length(df) - length(field_keep)
  if (chk > 0) {
    df <- select(df, all_of(field_keep))  # Drop extra columns
    message(msg, toString(chk), " columns removed")
  } else {
    message(msg, "OK")
  }
  # QAQC column values ---------------------------------------------------------
  for (field in field_need) {
    check_val_missing(df, field = field)
  }
  field_check <- intersect(field_optional, colnames(df))
  for (field in field_check) {
    check_val_missing(df, field = field, is_stop = FALSE)
  }
  check_val_duplicate(df, field = "Site_ID")
  check_val_duplicate(df, field = c("Site_Name"), is_stop = FALSE)
  check_val_duplicate(df, field = c("Latitude", "Longitude"),
                      is_stop = FALSE)
  check_val_numeric(df, field = "Latitude")
  check_val_numeric(df, field = "Longitude")
  # Update data format---------------------------------------------------------
  if ("State" %in% colnames(df)) {
    msg <- "\tChecking for valid entries in State..."
    chk <- df$State %in% c(state.name, state.abb)
    if (any(!chk)) {
      rws <- which(!chk)
      stop(msg, "\n\tInvalid State in row ",
           paste(rws, collapse = ", "), call. = FALSE)
    }
    message(msg, "OK")
    msg <- "\tChecking State abbreviations..."
    chk <- df$State %in% state.name
    if (any(chk)) {
      df <- df %>%
        mutate(State = case_when(
          State %in% state.name ~ state.abb[match(State, state.name)],
          TRUE ~ State))
      message(msg, sum(chk, na.rm = TRUE), " values converted to abbreviation")
    } else {
      message(msg, "OK")
    }
    if ("Town" %in% colnames(df)) {
      msg <- "\tAdding column Town_Code..."
      df <- df %>%
        mutate(Town_Code = paste0(Town, ", ", State))
      message(msg, "OK")
    }
  }
  return(df)
}

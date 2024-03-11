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
#'   * Adds Town_Code column if Town column included
#'
#' @param df Input dataframe.
#'
#' @return The return value, if any, from executing the function.
QAQC_sites <- function(df){
  # Define variables ----------------------------------------------------------
  field_need <- c("Site_ID", "Site_Name", "Latitude", "Longitude")
  field_optional <- c("Town", "State", "Watershed", "Group")
  field_all <- c(field_need, field_optional)

  # QAQC columns --------------------------------------------------------------
  message("Checking site data...\n")
  df <- update_column_format(df, "site")
  check_column_missing(df, field_need)
  # Drop extra columns
  field_keep <- intersect(field_all, colnames(df))
  chk <- length(df) - length(field_keep)
  if (chk > 0) {
    df <- dplyr::select(df, all_of(field_keep))  # Drop extra columns
    message("\t", toString(chk), " columns removed")
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
    chk <- df$State %in% c(state.name, state.abb)
    if (any(!chk)) {
      rws <- which(!chk)
      stop("Invalid entry for State in rows ",
           paste(rws, collapse = ", "), call. = FALSE)
    }
    chk <- df$State %in% state.name
    if (any(chk)) {
      df <- df %>%
        dplyr::mutate(State = dplyr::case_when(
          State %in% state.name ~ state.abb[match(State, state.name)],
          TRUE ~ State))
      message("\t", sum(chk, na.rm = TRUE), " state names converted to
              abbreviation")
    }
  }
  df <- check_val_count(df, "State")
  df <- check_val_count(df, "Town")
  df <- check_val_count(df, "Watershed")
  if ("Town" %in% colnames(df)) {
    if ("State" %in% colnames(df)) {
      df <- dplyr::mutate(df, Town_Code = paste0(Town, ", ", State))
    } else {
      df <- dplyr:: mutate(df, Town_Code = Town)
    }
    message("\tAdded column Town_Code")
  }
  return(df)
}

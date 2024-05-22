#' qaqc_sites
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
#'
#' @param df Input dataframe.
#'
#' @return The return value, if any, from executing the function.
qaqc_sites <- function(df){
  # Define variables
  field_all <- colnames_sites$WQdashboard
  field_need <- colnames_sites$WQdashboard_short
  field_need <- field_need[!field_need == ""]
  field_optional <- dplyr::setdiff(field_all, field_need)

  # Check columns
  message("Checking site data...\n")
  df <- update_column_names(df, colnames_sites)
  check_column_missing(df, field_need)

  # Check values
  for (field in field_need) {
    check_val_missing(df, field = field)
  }
  field_check <- intersect(field_optional, colnames(df))
  for (field in field_check) {
    check_val_missing(df, field = field, is_stop = FALSE)
  }
  check_val_duplicate(df, field = "Site_ID")
  check_val_duplicate(df, field = "Site_Name", is_stop = FALSE)
  df <- dplyr::mutate(df, Site_Name = make.unique(Site_Name, sep = " "))
  check_val_duplicate(df, field = c("Latitude", "Longitude"),
                      is_stop = FALSE)
  field_numeric <- c("Latitude", "Longitude", "Max_Depth_Surface",
    "Max_Depth_Midwater", "Max_Depth_Near_Bottom")
  field_numeric <- intersect(field_numeric, colnames(df))
  for (field in field_numeric) {
    check_val_numeric(df, field)
  }

  # Update data
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
  if ("Location_Type" %in% colnames(df)) {
    chk <- stringr::str_detect(df$Location_Type, "Ocean")
    # chk <- (chk | stringr::str_detect(df$Location_Type, "Lake"))
    if (any(chk)) {
      if (!"Group" %in% colnames(df)) {
        df <- dplyr::mutate(df, Group = NA)
        message("\tAdded column Group")
      }
      df <- df %>%
        dplyr::mutate(Group = dplyr::case_when(
          !is.na(Group) ~ Group,
          stringr::str_detect(Location_Type, "Ocean") ~ "Saltwater",
          # stringr::str_detect(Location_Type, "Lake") ~ "Lake",
          TRUE ~ NA))
    }
  }
  return(df)
}

#' format_sites
#'
#' @description Formats site data for use in the app.
#'
#' @param df Input dataframe.
#'
#' @return Updated dataframe.
format_sites <- function(df){
  message("\nFormatting site data...\n")
  # Drop extra columns
  field_all <- colnames_sites$WQdashboard
  field_keep <- intersect(field_all, colnames(df))
  chk <- length(df) - length(field_keep)
  if (chk > 0) {
    df <- dplyr::select(df, all_of(field_keep))  # Drop extra columns
    message("\t", toString(chk), " columns removed")
  }
  df <- check_val_count(df, "State")
  df <- check_val_count(df, "Watershed")
  if (!"State" %in% colnames(df)) {
    df <- check_val_count(df, "Town")
    df <- check_val_count(df, "County")
  }
  # Add new columns
  if ("Town" %in% colnames(df)) {
    if ("State" %in% colnames(df)) {
      df <- dplyr::mutate(df, Town_Code = paste0(Town, ", ", State))
    } else {
      df <- dplyr::mutate(df, Town_Code = Town)
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

  return(df)
}

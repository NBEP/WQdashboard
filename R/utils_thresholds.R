#' filter_threshold_depth
#'
#' @description A helper function for `filter_threshold`. Filters input
#'   dataframe by matching depth category, else NA depth category, else returns
#'   NULL.
#'
#' @param df Input dataframe.
#' @param depth_cat Desired depth category.
#'
#' @return Updated dataframe.
#'
#' @noRd
filter_threshold_depth <- function(df, depth_category,
    depth_column = "Depth_Category") {

  if (nrow(df) == 0 | is.null(df)) {
    return(NULL)
  } else if (!depth_column %in% colnames(df)) {
    return(df)
  }

  chk <- depth_category %in% df[[depth_column]]
  if (any(chk)){
    df <- dplyr::filter(df, .data[[depth_column]] == depth_category)
    return(df)
  }

  chk <- is.na(df[[depth_column]])
  if (any(chk)) {
    df <- dplyr::filter(df, is.na(.data[[depth_column]]))
    return(df)
  }

  return(NULL)
}

#' filter_threshold
#'
#' @description A helper function for `find_threshold`. Searches input threshold
#'   table for row that matches parameter, site, depth, group, and state
#'   criteria. Returns NULL if no match found.
#'
#' @param threshold_table Input dataframe with threshold values. (Acceptable
#'   entries: custom_thresholds, state_thresholds, epa_thresholds.)
#' @param site_id Site ID.
#' @param depth_category Depth category. (Shallow, Deep, NA) Default value NA.
#' @param depth_column Column where depth category is stored. Default value
#'    "Depth_Category"
#' @param group Group. Default value NA.
#' @param state State. Default value NA.
#' @param param Parameter
#'
#' @return Dataframe row with appropriate threshold values, if any. Returns NULL
#'   if no match found.
#'
#' @noRd
filter_threshold <- function(threshold_table, site_id, depth_category = NA,
    depth_column = "Depth_Category", group = NA, state = NA, parameter) {
  # Filter table
  df <- threshold_table
  if (is.na(depth_category) & !is.null(df[[depth_column]])) {
    df <- df %>%
      dplyr::filter(is.na(.data[[depth_column]])) %>%
      dplyr::select(!any_of(depth_column))
  }
  if ("State" %in% colnames(df)) {
    df <- dplyr::filter(df, State == state)
  }
  df <- dplyr::filter(df, Parameter == parameter)
  # Run checks
  if (nrow(df) == 0) { return (NULL) }
  if ("Site_ID" %in% colnames(df)) {
    chk <- site_id %in% df$Site_ID
    if (any(chk)) {
      df <- dplyr::filter(df, Site_ID == site_id)
      chk <- filter_threshold_depth(df, depth_category, depth_column)
      if (!is.null(chk)) { return(chk) }
    }
    df <- dplyr::filter(df, is.na(Site_ID))
  }
  if ("Group" %in% colnames(df)) {
    chk <- group %in% df$Group
    if (any(chk) & !is.na(group)) {
      df <- dplyr::filter(df, Group == group)
      chk <- filter_threshold_depth(df, depth_category, depth_column)
      if (!is.null(chk)) { return(chk) }
    }
    df <- dplyr::filter(df, is.na(Group))
    chk <- filter_threshold_depth(df, depth_category, depth_column)
    if (!is.null(chk)) { return(chk) }
  }
  if (nrow(df) > 0) {
    return(df)
  } else {
    return(NULL)
  }
}

#' find_threshold
#'
#' @description Iterates through custom_thresholds, state_thresholds, and
#'   epa_thresholds to find appropriate threshold values for given site,
#'   parameter. If no match found, returns NULL.
#'
#' @param site_id String. Site ID.
#' @param parameter String. Parameter.
#' @param state String. Overrides the state listed for each site.
#'
#' @return One row dataframe with list of thresholds for site, parameter pair.
#'   If no available thresholds, returns NULL.
#'
#' @noRd
find_threshold <- function(site_id, parameter, depth_column = "Depth_Category",
    depth_cat = NA) {

  # Define vars
  group <- NA
  state <- NA
  if ("Group" %in% colnames(df_sites)) {
    df <- dplyr::filter(df_sites, Site_ID == site_id)
    group <- df$Group[1]
  }
  if ("State" %in% colnames(df_sites_all)) {
    df <- dplyr::filter(df_sites_all, Site_ID == site_id)
    state <- df$State[1]
  }

  if (exists("custom_thresholds")) {
    df <- filter_threshold(
      threshold_table = custom_thresholds,
      site_id = site_id,
      depth_category = depth_cat,
      depth_column = depth_column,
      group = group,
      state = state,
      parameter = parameter)
    if (!is.null(df)) { return(df) }
  }

  df <- filter_threshold(
    threshold_table = state_thresholds,
    site_id = site_id,
    depth_category = depth_cat,
    depth_column = depth_column,
    group = group,
    state = state,
    parameter = parameter)
  if (!is.null(df)) { return(df) }

  df <- filter_threshold(
    threshold_table = epa_thresholds,
    site_id = site_id,
    depth_category = depth_cat,
    depth_column = depth_column,
    group = group,
    state = state,
    parameter = parameter)
  return(df)
}

#' convert_threshold_unit
#'
#' @description Converts threshold value to new unit. NA values returned as
#'  -999999
#'
#' @param x Numeric. Value to convert.
#' @param old_unit String. Current unit.
#' @param new_unit String. New unit.
#'
#' @return Updated value
#'
#' @noRd
convert_threshold_unit <- function(x, old_unit, new_unit) {
  if (is.na(x)) { return(-999999) }
  new_thresh <- convert_unit(x, old_unit, new_unit, FALSE)
  if (is.na(new_thresh)) { return(-999999) }
  return(new_thresh)
}

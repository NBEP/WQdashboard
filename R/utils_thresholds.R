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
filter_threshold_depth <- function(df, depth_category) {
  if (nrow(df) == 0 | is.null(df)) {
    return(NULL)
  } else if (is.null(df$Depth_Category)) {
    return(df)
  }
  chk <- depth_category %in% df$Depth_Category
  if (any(chk)){
    df <- dplyr::filter(df, Depth_Category == depth_category)
    return(df)
  }
  chk <- is.na(df$Depth_Category)
  if (any(chk)) {
    df <- dplyr::filter(df, is.na(Depth_Category))
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
#'   entries: df_thresholds, state_thresholds, epa_thresholds.)
#' @param site_id Site ID.
#' @param depth_category Depth category. (Shallow, Deep, NA) Default value NA.
#' @param group Group. Default value NA.
#' @param state State. Default value NA.
#' @param param Parameter
#'
#' @return Dataframe row with appropriate threshold values, if any. Returns NULL
#'   if no match found.
#'
#' @noRd
filter_threshold <- function(threshold_table, site_id, depth_category = NA,
    group = NA, state = NA, parameter) {
  # Filter table
  df <- threshold_table
  if (is.na(depth_category) & !is.null(df$Depth_Category)) {
    df <- df %>%
      dplyr::filter(is.na(Depth_Category)) %>%
      dplyr::select(!Depth_Category)
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
      chk <- filter_threshold_depth(df, depth_category)
      if (!is.null(chk)) { return(chk) }
    }
    df <- dplyr::filter(df, is.na(Site_ID))
  }
  if ("Group" %in% colnames(df)) {
    chk <- group %in% df$Group
    if (any(chk) & !is.na(group)) {
      df <- dplyr::filter(df, Group == group)
      chk <- filter_threshold_depth(df, depth_category)
      if (!is.null(chk)) { return(chk) }
    }
    df <- dplyr::filter(df, is.na(Group))
    chk <- filter_threshold_depth(df, depth_category)
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
#' @description Iterates through df_thresholds, state_thresholds, and
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
find_threshold <- function(site_id, parameter, depth_cat = NA, state = NA) {
  # Define vars
  group <- NA
  if ("Group" %in% colnames(df_sites)) {
    df <- dplyr::filter(df_sites, Site_ID == site_id)
    group <- df$Group[1]
  }
  if (is.na(state) & "State" %in% colnames(df_sites_all)) {
    df <- dplyr::filter(df_sites_all, Site_ID == site_id)
    state <- df$State[1]
  }
  if (exists("df_thresholds")) {
    df <- filter_threshold(
      threshold_table = df_thresholds,
      site_id = site_id,
      depth_category = depth_cat,
      group = group,
      state = state,
      parameter = parameter)
    if (!is.null(df)) { return(df) }
  }
  if (exists("state_thresholds")) {
    df <- filter_threshold(
      threshold_table = state_thresholds,
      site_id = site_id,
      depth_category = depth_cat,
      group = group,
      state = state,
      parameter = parameter)
    if (!is.null(df)) { return(df) }
  }
  if (exists("epa_thresholds")) {
    df <- filter_threshold(
      threshold_table = epa_thresholds,
      site_id = site_id,
      depth_category = depth_cat,
      group = group,
      state = state,
      parameter = parameter)
    if (!is.null(df)) { return(df) }
  }
  return(NULL)
}

#' threshold_max
#'
#' @description Find maximum threshold for selected site, parameter.
#'
#' @param site_id String. Site ID.
#' @param parameter String. Parameter.
#' @param unit String. Parameter unit.
#'
#' @return Maximum threshold for given site, parameter.
#'
#' @noRd
threshold_max <- function(site_id, parameter, unit, depth = NA) {
  # Define vars
  df <- find_threshold(site_id, parameter, depth_cat = depth)
  if (is.null(df)) {
    return(NA)
  }
  threshold_max <- convert_unit(df$Threshold_Max, df$Unit, unit, FALSE)
  return(threshold_max)
}

#' threshold_min
#'
#' @description Find minimum threshold for selected site, parameter.
#'
#' @param site_id String. Site ID.
#' @param parameter String. Parameter.
#' @param unit String. Parameter unit.
#'
#' @return Minimum threshold for given site, parameter.
#'
#' @noRd
threshold_min <- function(site_id, parameter, unit, depth = NA) {
  # Define vars
  df <- find_threshold(site_id, parameter, depth_cat = depth)
  if (is.null(df)) {
    return(NA)
  }
  threshold_min <- convert_unit(df$Threshold_Min, df$Unit, unit, FALSE)
  return(threshold_min)
}

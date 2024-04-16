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
  if (is.null(df$Depth_Category)) {
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
#'
#' @return One row dataframe with list of thresholds for site, parameter pair.
#'   If no available thresholds, returns NULL.
#'
#' @noRd
find_threshold <- function(site_id, parameter) {
  # Define vars
  depth_cat <- NA
  group <- NA
  state <- NA
  if ("Depth_Category" %in% colnames(df_sites)) {
    df <- dplyr::filter(df_sites, Site_ID == site_id)
    depth_cat <- df$Depth_Category[1]
  }
  if ("Group" %in% colnames(df_sites)) {
    df <- dplyr::filter(df_sites, Site_ID == site_id)
    group <- df$Group[1]
  }
  if ("State" %in% colnames(df_sites_all)) {
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

#' calculate_score
#'
#' @description Calculates numeric and category scores for given site,
#'   parameter, and values.
#'
#' @param site_id Site ID.
#' @param parameter Parameter.
#' @param unit Parameter unit.
#' @param score_max Maximum score.
#' @param score_min Minimum score.
#' @param score_mean Average score.
#' @param score_median Median score.
#'
#' @return Two value list with numeric, category scores.
#'
#' @noRd
calculate_score <- function(site_id, parameter, unit, score_max, score_min,
                            score_mean, score_median) {
  # Find thresholds
  df <- find_threshold(site_id, parameter)
  if (is.null(df)) {
    return(list(score_num = score_mean, score_str = NA))
  }
  # Select numeric score
  if (df$Min_Max_Mean == "max") {
    score <- score_max
  } else if (df$Min_Max_Mean == "min") {
    score <- score_min
  } else if (df$Min_Max_Mean == "median") {
    score <- score_median
  } else {
    score <- score_mean
  }
  # Convert to new units
  new_score <- convert_unit(score, unit, df$Unit, FALSE)
  if (is.na(new_score)) {
    return(list(score_num = score, score_str = NA))
  }
  # Find category score
  score_excellent <- "Excellent"
  score_good <- "Good"
  score_fair <- "Fair"
  score_poor <- "Poor"
  if (!is.na(df$Excellent) & !is.na(df$Good) & !is.na(df$Fair)) {
    if (df$Excellent > df$Good & df$Good > df$Fair) {
      if (new_score >= df$Excellent) {
        score2 <- score_excellent
      } else if (new_score >= df$Good) {
        score2 <- score_good
      } else if (new_score >= df$Fair) {
        score2 <- score_fair
      } else {
        score2 <- score_poor
      }
      return(list(score_num = score, score_str = score2))
    } else if (df$Excellent < df$Good & df$Good < df$Fair) {
      if (new_score <= df$Excellent) {
        score2 <- score_excellent
      } else if (new_score <= df$Good) {
        score2 <- score_good
      } else if (new_score <= df$Fair) {
        score2 <- score_fair
      } else {
        score2 <- score_poor
      }
      return(list(score_num = score, score_str = score2))
    }
  }
  if (!is.na(df$Threshold_Min) | !is.na(df$Threshold_Max)) {
    if (!is.na(df$Threshold_Min) & new_score < df$Threshold_Min) {
      score2 <- "Does Not Meet Criteria"
    } else if (!is.na(df$Threshold_Max) & new_score > df$Threshold_Max) {
      score2 <- "Does Not Meet Criteria"
    } else {
      score2 <- "Meets Criteria"
    }
    return(list(score_num = score, score_str = score2))
  }
  return(list(score_num = score, score_str = NA))
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
threshold_max <- function(site_id, parameter, unit) {
  # Define vars
  df <- find_threshold(site_id, parameter)
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
threshold_min <- function(site_id, parameter, unit) {
  # Define vars
  df <- find_threshold(site_id, parameter)
  if (is.null(df)) {
    return(NA)
  }
  threshold_min <- convert_unit(df$Threshold_Min, df$Unit, unit, FALSE)
  return(threshold_min)
}

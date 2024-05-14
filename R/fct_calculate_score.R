#' calculate_score
#'
#' @description Calculates numeric and category scores for given site,
#'   parameter, and values.
#'
#' @param site_id Site ID.
#' @param parameter Parameter.
#' @param unit Parameter unit.
#' @param depth Depth category. Default value NA.
#' @param default_state Which state thresholds to use. Overrides state listed
#'   for Site_ID. Default value NA.
#' @param score_max Maximum score.
#' @param score_min Minimum score.
#' @param score_mean Average score.
#' @param score_median Median score.
#'
#' @return Three value list with type of score type (eg minimum, maximum,
#'   average), numeric score, and category score.
#'
#' @noRd
calculate_score <- function(site_id, parameter, unit, depth = NA,
    default_state = NA, score_max, score_min, score_mean, score_median) {
  # Find thresholds
  df <- find_threshold(site_id, parameter, depth, default_state)
  if (is.null(df)) {
    return(list(score_typ = "Average", score_num = score_mean, score_str = NA))
  }
  # Select numeric score
  if (is.na(df$Min_Max_Mean)) {
    score <- score_mean
    typ <- "Average"
  } else if (df$Min_Max_Mean == "max") {
    score <- score_max
    typ <- "Maximum"
  } else if (df$Min_Max_Mean == "min") {
    score <- score_min
    typ <- "Minimum"
  } else if (df$Min_Max_Mean == "median") {
    score <- score_median
    typ <- "Median"
  } else {
    score <- score_mean
    typ <- "Average"
  }
  # Convert to new units
  new_score <- convert_unit(score, unit, df$Unit, FALSE)
  if (is.na(new_score)) {
    return(list(score_typ = typ, score_num = score, score_str = NA))
  }
  # Find categorgy score
  if (!is.na(df$Excellent) & !is.na(df$Good) & !is.na(df$Fair)) {
    if (df$Excellent > df$Good & df$Good > df$Fair) {
      if (new_score >= df$Excellent) {
        score2 <- "Excellent"
      } else if (new_score >= df$Good) {
        score2 <- "Good"
      } else if (new_score >= df$Fair) {
        score2 <- "Fair"
      } else {
        score2 <- "Poor"
      }
      return(list(score_typ = typ, score_num = score, score_str = score2))
    } else if (df$Excellent < df$Good & df$Good < df$Fair) {
      if (new_score <= df$Excellent) {
        score2 <- "Excellent"
      } else if (new_score <= df$Good) {
        score2 <- "Good"
      } else if (new_score <= df$Fair) {
        score2 <- "Fair"
      } else {
        score2 <- "Poor"
      }
      return(list(score_typ = typ, score_num = score, score_str = score2))
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
    return(list(score_typ = typ, score_num = score, score_str = score2))
  }
  return(list(score_typ = typ, score_num = score, score_str = NA))
}

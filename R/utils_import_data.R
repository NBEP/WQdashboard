# Set sidebar variables -----

#' Sort months
#'
#' @description `sort_months()` sorts months chronologically and removes
#' duplicate values.
#'
#' @param month_list Unsorted list of months
#'
#' @return Chronological list of months
#'
#' @noRd
sort_months <- function(month_list) {
  month_list <- unique(month_list)

  if (length(month_list) == 1) {
    return(month_list)
  }

  month_list <- month_list[month_list %in% month.name]
  month_list <- as.integer(factor(month_list, levels = month.name))

  month_range <- seq(min(month_list), max(month_list))
  month_range <- month.name[month_range]
}

#' Sort depth
#'
#' @description `sort_depth()` sorts depths from shallow to deep.
#'
#' @param depth_list Unsorted depth list
#'
#' @return Sorted depth list
#'
#' @noRd
sort_depth <- function(depth_list) {
  depth_list <- unique(depth_list)

  default_depths <- c("Surface", "Midwater", "Near Bottom", "Bottom")
  default_depths <- default_depths[default_depths %in% depth_list]

  extra_depths <- setdiff(depth_list, default_depths)
  extra_depths <- sort(extra_depths, na.last = TRUE)

  c(default_depths, extra_depths)
}

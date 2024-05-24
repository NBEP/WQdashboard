#' Prep plot data
#'
#' @description Preps data for use in graph functions.
#'
#' @param df Dataframe.
#' @param site_id List. Site_ID names.
#' @param parameter String. Parameter name.
#'
#' @return Updated dataframe.
#'
#' @noRd
prep_plot_df <- function(df, site_id, parameter, depth = NA){

  df <- dplyr::filter(df, Site_ID %in% site_id & Parameter %in% parameter)
  if (!is.na(depth) & "Depth" %in% colnames(df)) {
    df <- dplyr::filter(df, Depth %in% depth)
  }

  return(df)
}

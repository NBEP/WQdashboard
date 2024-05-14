#' scatter_plot
#'
#' @description Creates a scatterplot using `plotly`.
#'
#' @param df Dataframe.
#' @param site_id List. Site_ID names.
#' @param parameter String. Parameter name.
#'
#' @return Scatterplot.
#'
#' @noRd

scatter_plot <- function(df, site_id, parameter, depth = NA){
  df <- prep_plot_df(df, site_id, parameter)
  thresh <- NA

  # Calculate threshold
  if (length(c(site_id, parameter, depth)) == 3) {
    thresh <- find_threshold(site_id, parameter, depth_cat = depth)
  }



}

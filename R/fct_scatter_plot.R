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

scatter_plot <- function(df, site_id, parameter){
  df <- prep_plot_df(df, site_id, parameter) %>%
    dplyr::select(!Year)

  # Calculate threshold



}

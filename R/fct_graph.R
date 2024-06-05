#' graph_one_var
#'
#' @description Creates a scatterplot using `plotly`.
#'
#' @param df Dataframe.
#' @param thresholds Boolean. If TRUE, adds red bar to indicate values outside
#'  acceptable range. Default FALSE.
#'
#' @return Scatterplot.
#'
#' @noRd

graph_one_var <- function(df, thresholds = FALSE) {
  if (nrow(df) == 0) { return(NULL) }

  if ("Depth" %in% colnames(df)) {
    len_depth <- length(unique(df$Depth))
  } else {
    len_depth <- 0
  }

  df_new <- add_line_breaks(df)

  if (len_depth < 2) {
    fig <- plotly::plot_ly(
      data = df_new,
      type = "scatter",
      mode = "lines+markers",
      x = ~Date,
      y = ~Result,
      color = ~Site_Name,
      hoverinfo = "text",
      hovertext = ~Description)
  } else {
    fig <- plotly::plot_ly(
      data = df_new,
      type = "scatter",
      mode = "lines+markers",
      x = ~Date,
      y = ~Result,
      color = ~Depth,
      hoverinfo = "text",
      hovertext = ~Description)
  }

  if (thresholds) {
    fig <- add_thresholds(fig, df)
  }

  # Calculate axes, style plot
  years <- difftime(max(df$Date), min(df$Date), units = "days")
  years <- as.numeric(years)/365

  min_val <- min(df$Result) * .8
  if (min_val > 0) { min_val <- 0 }
  max_val <- max(df$Result) * 1.2
  if (max_val == min_val) { max_val <- min_val + 1 }

  fig <- graph_style(fig,
    fig_title = df$Parameter[1],
    y_title = paste(df$Parameter[1], param_unit(df$Parameter[1])),
    y_range = list(min_val, max_val),
    years = years)

  return(fig)
}

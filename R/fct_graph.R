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

graph_one_var <- function(df, fig_title, group = "Site_Name",
    thresholds = FALSE) {
  if (nrow(df) == 0) { return(NULL) }

  df_new <- add_line_breaks(df)

  pal <- palette.colors(palette = "R4")
  # pal <- c("#2daebe", "#6f3d61", "#f5b400", "#cf4e13", "#2c2c2c")
  # pal_len <- length(unique(df[[group]]))
  # pal <- pal[1:pal_len]

  if (group == "Site_Name") {
    fig <- plotly::plot_ly(
      data = df_new,
      type = "scatter",
      mode = "lines+markers",
      x = ~Date,
      y = ~Result,
      color = ~Site_Name,
      colors = pal,
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
      colors = pal,
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
    fig_title = fig_title,
    y_title = paste(df$Parameter[1], param_unit(df$Parameter[1])),
    y_range = list(min_val, max_val),
    years = years)

  return(fig)
}

#' graph_two_var
#'
#' @description Creates a scatterplot with two y-axises using `plotly`.
#'
#' @param df Dataframe.
#' @param fig_title String. Title of graph.
#'
#' @return Scatterplot.
#'
#' @noRd

graph_two_var <- function(df, fig_title, par1) {
  if (nrow(df) == 0) { return(NULL) }
  if (is.na(par1)) { par1 <- df$Parameter[1] }

  df_new <- add_line_breaks(df)
  df1 <- dplyr::filter(df_new, Parameter == par1)
  df2 <- dplyr::filter(df_new, Parameter != par1)

  pal <- palette.colors(palette = "R4")
  # pal <- c("#2daebe", "#6f3d61", "#f5b400", "#cf4e13", "#2c2c2c")
  # pal_len <- length(unique(df[[group]]))
  # pal <- pal[1:pal_len]

  # Create graph with first parameter
  fig <- plotly::plot_ly(
    data = df1,
    type = "scatter",
    mode = "lines+markers",
    x = ~Date,
    y = ~Result,
    color = ~Parameter,
    colors = pal,
    hoverinfo = "text",
    hovertext = ~Description)

  # Add second parameter
  if (nrow(df2) > 0) {
    par2 <- df2$Parameter[1]

    fig <- fig %>%
      plotly::add_trace(
        data = df2,
        type = "scatter",
        mode = "lines+markers",
        yaxis = "y2",
        x = ~Date,
        y = ~Result,
        color = ~Parameter,
        colors = pal,
        hoverinfo = "text",
        hovertext = ~Description) %>%
      plotly::layout(
        yaxis2 = list(
          title = paste(par2, param_unit(par2)),
          overlaying = "y",
          side = "right",
          rangemode = "tozero",
          fixedrange = TRUE,
          titlefont = list(size = 16),
          tickfont = list(size = 16),
          linecolor = "black",
          showgrid = FALSE,
          tickcolor = "black"),
        legend = list(
          x = 1.05))
  }

  # Calculate axes, style plot
  years <- difftime(max(df$Date), min(df$Date), units = "days")
  years <- as.numeric(years)/365

  fig <- graph_style(
    fig,
    fig_title = fig_title,
    y_title = paste(par1, param_unit(par1)),
    y_range = NA,
    years = years)

  return(fig)
}

#' graph_style
#'
#' @description Set style options for graph.
#'
#' @param fig Plotly object.
#'
#' @return Updated graph.
#'
#' @noRd

graph_style <- function(fig, fig_title, y_title, y_range, years) {
  fig <- fig %>%
    plotly::config(displayModeBar = FALSE) %>%
    plotly::layout(
      title = fig_title,
      yaxis = list(
        title = y_title,
        rangemode = "tozero",
        fixedrange = TRUE,
        range = y_range,
        titlefont = list(size = 16),
        tickfont = list(size = 16),
        linecolor = "black",
        showgrid = FALSE,
        tickcolor = "black"),
      xaxis = list(
        title = "Date",
        rangemode = "tozero",
        # fixedrange = TRUE,
        rangeslider = list(type = "date"),
        titlefont = list(size = 16),
        tickfont = list(size = 16),
        linecolor = "black",
        showgrid = FALSE,
        tickcolor = "black"),
      hoverlabel = list(bgcolor = "white"),
      margin = list(
        l = 50, r = 20,
        b = 20, t = 55,
        pad = 0))

  if (years > 5) {
    fig <- fig %>%
      plotly::layout(
        xaxis = list(
          rangeselector = list(
            buttons = list(
              list(
                count = 1,
                label = "1 yr",
                step = "year"),
              list(
                count = 2,
                label = "2 yr",
                step = "year"),
              list(
                count = 5,
                label = "5 yr",
                step = "year"),
              list(step = "all")))))
  } else if (years > 2) {
    fig <- fig %>%
      plotly::layout(
        xaxis = list(
          rangeselector = list(
            buttons = list(
              list(
                count = 1,
                label = "1 yr",
                step = "year"),
              list(
                count = 2,
                label = "2 yr",
                step = "year"),
              list(step = "all")))))
  } else if (years > 1) {
    fig <- fig %>%
      plotly::layout(
        xaxis = list(
          rangeselector = list(
            buttons = list(
              list(
                count = 1,
                label = "1 yr",
                step = "year"),
              list(step = "all")))))
  } else {
    fig <- fig %>%
      plotly::layout(
        xaxis = list(
          rangeselector = list(
            buttons = list(
              list(step = "all")))))
  }

  return(fig)
}

#' add_line_breaks
#'
#' @description Add line breaks by adding null row for each
#'   site/parameter/year combination.
#'
#' @param df Dataframe.
#'
#' @return Updated dataframe.
#'
#' @noRd
add_line_breaks <- function(df) {
  df_null <- expand.grid(
      Site_Name = unique(df$Site_Name),
      Parameter = unique(df$Parameter),
      Year = unique(df$Year)) %>%
    dplyr::mutate(Date = as.Date(paste0(Year, "-1-1")))

  df <- bind_rows(df, df_null) %>%
    dplyr::arrange(Date)

  return(df)
}

#' Add thresholds
#'
#' @description Add colored bars to plot to indicate threshold values.
#'
#' @param fig Graph.
#' @param df Dataframe.
#' @param parameter Parameter.
#' @param unit Parameter unit.
#' @param depth Depth.
#'
#' @return Updated graph.
#'
#' @noRd
add_thresholds <- function(fig, df) {
  site = df$Site_ID[1]
  parameter = df$Parameter[1]
  unit = df$Unit[1]
  depth = df$Depth[1]

  min_val <- min(df$Result) * .8
  if (min_val > 0) {
    min_val <- 0
  }
  max_val <- max(df$Result) * 1.2
  min_date <- min(df$Date) - lubridate::years(1)
  max_date <- max(df$Date) + lubridate::years(1)

  thresh_min <- threshold_min(site, parameter, unit, depth)
  thresh_max <- threshold_max(site, parameter, unit, depth)

  if (thresh_min != -999999 & thresh_max != -999999 & min_val < thresh_min &
      max_val > thresh_max) {
    fig <- fig %>%
      plotly::layout(shapes = list(
        list(
          type = "rect",
          fillcolor = "red",
          line = list(color = "red"),
          opacity = 0.2,
          y0 = min_val,
          y1 = thresh_min,
          x0 = min_date,
          x1 = max_date),
        list(
          type = "rect",
          fillcolor = "red",
          line = list(color = "red"),
          opacity = 0.2,
          y0 = thresh_max,
          y1 = max_val,
          x0 = min_date,
          x1 = max_date)))
  } else if (thresh_min != -999999 & min_val < thresh_min) {
    fig <- fig %>%
      plotly::layout(shapes = list(
        list(
          type = "rect",
          fillcolor = "red",
          line = list(color = "red"),
          opacity = 0.2,
          y0 = min_val,
          y1 = thresh_min,
          x0 = min_date,
          x1 = max_date)))
  } else if (thresh_max != -999999 & max_val > thresh_max) {
    fig <- fig %>%
      plotly::layout(shapes = list(
        list(
          type = "rect",
          fillcolor = "red",
          line = list(color = "red"),
          opacity = 0.2,
          y0 = thresh_max,
          y1 = max_val,
          x0 = min_date,
          x1 = max_date)))
  }

  return(fig)
}

#' format_graph_table
#'
#' @description Format graph data as table.
#'
#' @param df Input dataframe.
#' @param group How to group data. Options: Site_Name, Parameter, Depth.
#'
#' @return Updated dataframe.
#'
#' @noRd
format_graph_table <- function(df, group) {
  col_select <- c("Date", "Result", group)

  df <- df %>%
    dplyr::mutate(Result = dplyr::if_else(
      Unit %in% c(NA, "None"),
      as.character(Result),
      paste(Result, Unit))) %>%
    dplyr::select(dplyr::all_of(col_select))

  var_count <- length(unique(df[[group]]))

  if (var_count == 1) {
    var_name <- df[[group]][1]

    df_wide <- df %>%
      dplyr::select(Date, Result) %>%
      dplyr::rename({{var_name}} := Result)
  } else {
    df_wide <- tidyr::spread(df, {{group}}, Result)
  }

  df_wide <- reactable_table(df_wide, graph_table = TRUE)

  return(df_wide)
}

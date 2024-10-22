#' graph_style
#'
#' @description Set style options for graph.
#'
#' @param fig Plotly object.
#'
#' @return Updated graph.
#'
#' @noRd

graph_style <- function(fig, fig_title, y_title, y_range) {
  fig <- fig %>%
    plotly::config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c(
        'sendDataToCloud',
        'zoom2d',
        'pan2d',
        'select2d',
        'lasso2d',
        'autoScale2d',
        'resetScale2d',
        'zoomIn2d',
        'zoomOut2d',
        'hoverClosestCartesian',
        'hoverCompareCartesian'),
      toImageButtonOptions = list(height = 400, width = 800)
      ) %>%
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
        fixedrange = TRUE,
        titlefont = list(size = 16),
        tickfont = list(size = 16),
        linecolor = "black",
        showgrid = FALSE,
        tickcolor = "black"),
      showlegend = TRUE,
      hoverlabel = list(bgcolor = "white"),
      margin = list(
        l = 50, r = 20,
        b = 20, t = 55,
        pad = 0)
      )

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

  df <- dplyr::bind_rows(df, df_null) %>%
    dplyr::arrange(Date)

  return(df)
}

#' Add thresholds
#'
#' @description Add colored bars to plot to indicate threshold values.
#'
#' @param fig Graph.
#' @param thresh Dataframe with threshold values.
#' @param date_range Minimum, maximum dates for x-axis.
#' @param y_range Minimum, maximum values for y-axis.
#' @param unit Unit used for y-axis.
#'
#' @return Updated graph.
#'
#' @noRd
add_thresholds <- function(thresh, visible = TRUE, date_range, y_range,
                           unit) {
  min_date <- date_range[1]
  max_date <- date_range[2]
  min_val <- y_range[1]
  max_val <- y_range[2]
  old_unit <- thresh$Unit

  thresh_min <- convert_threshold_unit(thresh$Threshold_Min, old_unit, unit)
  thresh_max <- convert_threshold_unit(thresh$Threshold_Max, old_unit, unit)
  thresh_excellent <- convert_threshold_unit(thresh$Excellent, old_unit, unit)
  thresh_good <- convert_threshold_unit(thresh$Good, old_unit, unit)

  fig <- plotly::plot_ly()

  if (thresh_min != -999999 & min_val < thresh_min) {
    fig <- fig %>%
      plotly::add_polygons(
        x=c(min_date, max_date, max_date, min_date),
        y=c(thresh_min, thresh_min, min_val, min_val),
        line=list(width=0),
        fillcolor= "#f6c0c0",
        visible = visible,
        hoverinfo = "text",
        hovertext = "Does Not Meet Criteria",
        inherit = FALSE,
        name = "Does Not Meet\nCriteria",
        legendrank = 1003)
  }

  if (thresh_max != -999999 & max_val > thresh_max) {
    fig <- fig %>%
      plotly::add_polygons(
        x=c(min_date, max_date, max_date, min_date),
        y=c(thresh_max, thresh_max, max_val, max_val),
        line=list(width=0),
        fillcolor= "#f6c0c0",
        visible = visible,
        hoverinfo = "text",
        hovertext = "Does Not Meet Criteria",
        inherit = FALSE,
        name = "Does Not\nMeet Criteria",
        legendrank = 1002)
  }

  if (thresh_excellent != -999999 & thresh_excellent < thresh_good &
      thresh_excellent > min_val) {
    fig <- fig %>%
      plotly::add_polygons(
        x=c(min_date, max_date, max_date, min_date),
        y=c(thresh_excellent, thresh_excellent, min_val, min_val),
        line=list(width=0),
        fillcolor= "#dde8fe",
        visible = visible,
        hoverinfo = "text",
        hovertext = "Excellent",
        inherit = FALSE,
        name = "Excellent",
        legendrank = 1001)
  } else if (thresh_good != -999999 & thresh_good < thresh_excellent &
             thresh_excellent < max_val) {
    fig <- fig %>%
      plotly::add_polygons(
        x=c(min_date, max_date, max_date, min_date),
        y=c(thresh_excellent, thresh_excellent, max_val, max_val),
        line=list(width=0),
        fillcolor= "#dde8fe",
        visible = visible,
        hoverinfo = "text",
        hovertext = "Excellent",
        inherit = FALSE,
        name = "Excellent",
        legendrank = 1001)
  }

  return(fig)
}

#' Add Trend Line (GAM)
#'
#' @description Calculates GAM and adds smooth trend line with 95% confidence
#'   interval.
#'
#' @param fig Plotly graph.
#' @param df Dataframe.
#'
#' @return Updated plotly graph.
#'
#' @noRd
add_gam <- function(fig, df, visible = TRUE) {

  df <- dplyr::mutate(df, Dec_Date = lubridate::decimal_date(Date))

  # Code from Carmen Chan
  # https://www.displayr.com/how-to-add-trend-lines-in-r-using-plotly/

  df_gam <- mgcv::gam(df$Result~s(df$Dec_Date))
  df_pred <- predict(df_gam, type="response", se.fit=TRUE)
  df_new <- data.frame(
      x = df_gam$model[,2],
      y = df_pred$fit,
      lb = as.numeric(df_pred$fit - (1.96 * df_pred$se.fit)),
      ub = as.numeric(df_pred$fit + (1.96 * df_pred$se.fit))) %>%
    dplyr::mutate(x = lubridate::date_decimal(x))
  df_new <- df_new[order(df_new$x),]

  fig <- fig %>%
    plotly::add_trace(
      data = df_new,
      x = ~x,
      y = ~y,
      type = "scatter",
      mode = "lines",
      line = list(
        color = "#2c2c2c",
        width = 2,
        dash = "dash"),
      visible = visible,
      inherit = FALSE,
      name = "Trend Line (GAM)") %>%
    plotly::add_ribbons(
      data = df_new,
      x = ~x,
      ymin = ~lb,
      ymax = ~ub,
      line=list(
        color="#818181",
        opacity=0.4,
        width=0),
      fillcolor = list(
        color="#818181",
        opacity=0.4),
      visible = visible,
      inherit = FALSE,
      name = "95% Confidence\nInterval"
    )

  return(fig)
}

#' Caption Graph
#'
#' @description Generate list of threshold values.
#'
#' @param df Input dataframe.
#' @param thresh Dataframe with threshold values.
#'
#' @return List of threshold values.
#'
#' @noRd
caption_graph <- function(df, thresh = NULL) {
  if (is.null(thresh)) {
    return("")
  }

  parameter <- df$Parameter[1]
  old_unit <- thresh$Unit
  new_unit <- df$Unit[1]

  thresh_min <- convert_threshold_unit(thresh$Threshold_Min, old_unit, new_unit)
  thresh_max <- convert_threshold_unit(thresh$Threshold_Max, old_unit, new_unit)
  thresh_excellent <- convert_threshold_unit(thresh$Excellent, old_unit, new_unit)
  thresh_good <- convert_threshold_unit(thresh$Good, old_unit, new_unit)

  chk <- thresh_min == -999999 & thresh_max == -999999 &
    (thresh_excellent == -999999 | thresh_good == -999999)
  if (chk) {
    return("")
  }

  thresh_text <- "<hr><h3>Thresholds</h3>"

  if (new_unit %in% c(NA, "None")) { new_unit <- NULL }

  if (thresh_min != -999999 & thresh_max != -999999) {
    thresh_text <- paste0(
      thresh_text, "<b>Acceptable:</b> ", pretty_number(thresh_min), " - ",
      pretty_number(thresh_max), " ", new_unit)
  } else if (thresh_min != -999999) {
    thresh_text <- paste0(
      thresh_text, "<b>Acceptable:</b> &gt; ", pretty_number(thresh_min),
      " ", new_unit)
  } else if (thresh_max != -999999) {
    thresh_text <- paste0(
      thresh_text, "<b>Acceptable:</b> &lt; ", pretty_number(thresh_max),
      " ", new_unit)
  }

  if (nchar(thresh_text) > 19) {
    thresh_text <- paste0(trimws(thresh_text), "<br>")
  }

  if (thresh_excellent != -999999 & thresh_excellent < thresh_good) {
    thresh_text <- paste0(
      thresh_text, "<b>Excellent:</b> &lt; ", pretty_number(thresh_excellent),
      " ", new_unit)
  } else if (thresh_good != -999999 & thresh_good < thresh_excellent) {
    thresh_text <- paste0(
      thresh_text, "<b>Excellent:</b> &gt; ", pretty_number(thresh_excellent),
      " ", new_unit)
  }

  thresh_text <- trimws(thresh_text)

  return(thresh_text)
}

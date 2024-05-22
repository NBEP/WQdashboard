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

scatter_plot <- function(df, site_id, parameter, depth = NA) {
  df <- prep_plot_df(df, site_id, parameter, depth)
  thresh_min <- NA
  thresh_max <- NA

  # Calculate threshold
  if (length(c(site_id, parameter)) == 2) {
    unit <- param_unit(parameter)
    thresh_min <- threshold_min(site_id, parameter, unit, depth)
    thresh_max <- threshold_max(site_id, parameter, unit, depth)
  }

  # Add gaps between years
  list_sites <- unique(df$Site_Name)
  list_years <- unique(df$Year)
  list_param <- unique(df$Parameter)

  df_null <- expand.grid(list_sites, list_years)
  colnames(df_null) <- c("Site_Name", "Year")
  df_null <- merge(df_null, list_param, by = NULL) %>%
    dplyr::rename(Parameter = y) %>%
    dplyr::mutate(Date = as.Date(paste0(Year, "-1-1")))

  df <- bind_rows(df, df_null) %>%
    dplyr::arrange(Date)

  # Create plot
  fig <- plotly::plot_ly(
    data = df,
    type = "scatter",
    mode = "lines+markers",
    x = ~Date,
    y = ~Result,
    color = ~Site_Name)

  fig <- fig %>%
    plotly::config(displayModeBar = FALSE) %>%
    plotly::layout(
      title = "Title",
      yaxis = list(
        title = "Parameter (Unit)",
        rangemode = "tozero",
        fixedrange = TRUE),
      xaxis = list(
        showgrid = FALSE,
        fixedrange = TRUE,
        rangeslider = list(type = "date")))


  chk <- difftime(max(df$Date), min(df$Date), units="days")
  if (as.numeric(chk) > 2000) {
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
                lable = "5 yr",
                step = "year"),
              list(step = "all")))))
  } else if (as.numeric(chk) > 1000) {
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
  } else if (as.numeric(chk) > 365) {
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

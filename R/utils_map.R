#' Leaflet popup text
#'
#' `add_popup_text()` adds column with pre-formatted popup, alt text for
#'   leaflet map.
#'
#' @param df A dataframe.
#'
#' @returns Updated dataframe.
#'
#' @noRd
add_popup_text <- function(df) {

  df <- df %>%
    dplyr::mutate(popup_loc = paste0("<b>", Site_Name, "</b>")) %>%
    dplyr::mutate(popup_score = "") %>%
    dplyr::mutate(alt = paste0(Site_Name, ","))

  if ("Town" %in% colnames(df)) {
    df <- dplyr::mutate(df,
      popup_loc = paste(popup_loc, "<br/>Town:", Town))
  } else if ("County" %in% colnames (df)) {
    df <- dplyr::mutate(df,
      popup_loc = paste(popup_loc, "<br/>County:", County))
  } else if ("State" %in% colnames (df)) {
    df <- dplyr::mutate(df,
      popup_loc = paste(popup_loc, "<br/>State:", State))
  }

  if ("Watershed" %in% colnames(df)) {
    df <- dplyr::mutate(df,
      popup_loc = paste(popup_loc, "<br/>Watershed:", Watershed))
  }

  if ("Group" %in% colnames(df)) {
    df <- dplyr::mutate(df,
      popup_loc = paste(popup_loc, "<br/>Group:", Group))
  }

  if ("Depth" %in% colnames(df)) {
    df <- dplyr::mutate(df,
      popup_score = paste(popup_score, "<br/>Depth:", Depth))
  }

  df <- df %>%
    dplyr::mutate(popup_score = dplyr::if_else(
      is.na(score_num),
      paste(popup_score, "<br/><i>No data</i>"),
      paste0(popup_score,
        "<br/>", score_typ, ": ", score_num, " ", Unit))) %>%
    dplyr::mutate(popup_score = dplyr::if_else(
      is.na(score_num) | score_str == "No Threshold Established",
      popup_score,
      paste(popup_score, "<br/>Score:", score_str))) %>%
    dplyr::mutate(alt = dplyr::case_when(
      is.na(score_num) ~ paste(alt, "No data"),
      score_str == "No Threshold Established" ~ paste(alt, score_num, Unit),
      TRUE ~ paste(alt, score_str)
    ))


  return(df)
}

#' param_unit
#'
#' Finds unit for parameter.
#'
#' @param param Paramter.
#'
#' @returns Icon code.
#'
#' @noRd
param_unit <- function(param) {
  df <- df_score %>%
    dplyr::filter(!is.na(Unit) & Parameter == param)

  if (length(df) > 0) {
    return(df$Unit[1])
  } else {
    return("")
  }
}

#' num_pal
#'
#' Creates palette for continuous data.
#'
#' @param df_param df_score filtered by year and parameter.
#'
#' @returns Icon code.
#'
#' @noRd
num_pal <- function(df_param) {
  chk <- is.na(df_param$score_num)

  if (all(chk)) {
    par_min <- 0
    par_max <- 1
  } else {
    par_min <- min(df_param$score_num, na.rm = TRUE)
    par_max <- max(df_param$score_num, na.rm = TRUE)
  }

  if (par_min == par_max) {
    if (par_min > 1) { par_min <- par_min - 1 }
    par_max <- par_max + 1
  }

  pal <- leaflet::colorNumeric(
    palette = c("#cdcef1", "#aca8d3", "#8b83b6", "#6c5f9a", "#4d3d7f"),
    domain = c(par_min, par_max),
    #bins = 5,
    na.color = "#f4f4f4")

  return(pal)
}

#' num_shape
#'
#' Helper function for `num_symbols`. Sets all icons as circles, except NA
#'   values which are cross.
#'
#' @param x List of values.
#'
#' @returns List of shapes.
#'
#' @noRd
num_shape <- function(x) {
  if (is.na(x)) { "cross" } else { "circle" }
}

#' num_symbols
#'
#' Creates `leaflet` icons for continuous data.
#'
#' @param df_param df_score filtered by year and parameter.
#' @param df Data to map.
#'
#' @returns Icon code.
#'
#' @noRd
num_symbols <- function(df_param, df) {
  pal <- num_pal(df_param)

  icon_symbols <- Map(f = leaflegend::makeSymbol,
                      shape = lapply(df$score_num, num_shape),
                      fillColor = pal(df$score_num),
                      color = "#444444",
                      opacity = 1,
                      width = 24,
                      "stroke-width" = 1.5)

  return(icon_symbols)
}

#' pal_cat
#'
#' Creates `leaflet` icons for categorical data.
#'
#' @param df_param df_score filtered by year and parameter.
#' @param is_legend Boolean. If TRUE, formats symbols legend. If FALSE, formats
#'   symbols for map. Default FALSE.
#'
#' @returns Icon code.
#'
#' @noRd
cat_pal <- function(df_param, is_legend = FALSE) {
  param_score <- unique(df_param$score_str)
  x <- c("Excellent", "Good", "Fair", "Poor")
  y <- c("Meets Criteria", "Does Not Meet Criteria")

  icon_color <- "#f4f4f4"
  icon_shape <- "cross"
  icon_names <- "No Data Available"

  if (any(param_score %in% x)) {
    icon_color <- c("#347bc0", "#62c2dd", "#f3d56f", "#db7363", icon_color)
    icon_shape <- c("circle", "rect", "triangle", "diamond", icon_shape)
    icon_names <- c(x, icon_names)
  }

  if (any(param_score %in% y)) {
    icon_color <- c("#62c2dd", "#db7363", icon_color)
    icon_shape <- c("rect", "diamond", icon_shape)
    icon_names <- c(y, icon_names)
  }

  if (is_legend) {
    icon_color <- unique(icon_color)
    icon_shape <- unique(icon_shape)
  }

  icon_symbols <- Map(f = leaflegend::makeSymbol,
                      shape = icon_shape,
                      fillColor= icon_color,
                      color = "#444444",
                      opacity = 1,
                      width = 24,
                      "stroke-width" = 1.5)
  if (!is_legend) {
    icon_symbols <- setNames(icon_symbols, nm = icon_names)
  }

  return(icon_symbols)
}

#' cat_labels
#'
#' Creates legend labels to accompany `pal_cat`.
#'
#' @param df_param df_score filtered by year and parameter.
#'
#' @returns List of labels.
#'
#' @noRd
cat_labels <- function(df_param) {
  param_score <- unique(df_param$score_str)
  x <- c("Excellent", "Good", "Fair", "Poor")
  y <- c("Meets Criteria", "Does Not Meet Criteria")

  label_list <- "No Data Available"

  if (any(param_score %in% x) & any(param_score %in% y)) {
    label_list <- c("Excellent", "Good / Meets Criteria", "Fair",
      "Poor / Does Not Meet Criteria", label_list)
  } else if (any(param_score %in% x)) {
    label_list <- c(x, label_list)
  } else if (any(param_score %in% y)) {
    label_list <- c(y, label_list)
  }

  return(label_list)
}

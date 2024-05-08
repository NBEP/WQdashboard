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

#' Icon Shape
#'
#' Helper function for `pal_num`. Sets all icons as circles, except NA values
#'   are cross.
#'
#' @param x List of values.
#'
#' @returns List of shapes.
#'
#' @noRd
icon_shape <- function(x) {
  if (is.na(x)) { "cross" } else { "circle" }
}

#' pal_num
#'
#' Creates `leaflet` icons for continuous data.
#'
#' @param df_param df_score filtered by year and parameter.
#' @param df Data to map.
#'
#' @returns Icon code.
#'
#' @noRd
pal_num <- function(df_param, df) {
  chk <- is.na(df_param$score_num)

  if (all(chk)) {
    par_min <- 0
    par_max <- 1
  } else {
    par_min <- min(df_param$score_num, na.rm = TRUE)
    par_max <- max(df_param$score_num, na.rm = TRUE)
  }

  pal <- leaflet::colorNumeric(
    palette = c("#cdcef1", "#aca8d3", "#8b83b6", "#6c5f9a", "#4d3d7f"),
    reverse = TRUE,
    domain = c(par_min, par_max),
    #bins = 5,
    na.color = "#f4f4f4")

  icon_symbols <- Map(f = leaflegend::makeSymbol,
                      shape = lapply(df$score_num, icon_shape),
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
#'
#' @returns Icon code.
#'
#' @noRd
pal_cat <- function(df_param) {
  icon_color <- c("#347bc0", "#62c2dd", "#62c2dd", "#f3d56f", "#db7363",
                  "#db7363", "#f4f4f4")
  icon_shape <- c("circle", "rect", "rect", "triangle", "diamond", "diamond",
                  "cross")
  icon_names <- c("Excellent", "Good", "Meets Criteria", "Fair", "Poor",
                  "Does Not Meet Criteria", "No Data Available")

  icon_symbols <- Map(f = leaflegend::makeSymbol,
                      shape = icon_shape,
                      fillColor= icon_color,
                      color = "#444444",
                      opacity = 1,
                      width = 24,
                      "stroke-width" = 1.5)
  icon_symbols <- setNames(icon_symbols, nm = icon_names)

  return(icon_symbols)
}

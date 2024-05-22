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
  key_col <- c("Site_Name", "Year", "Date", "Parameter", "Result", "Unit")

  df <- dplyr::filter(df, Site_ID %in% site_id & Parameter %in% parameter)
  if (!is.na(depth) & "Depth" %in% colnames(df)) {
    df <- dplyr::filter(df, Depth %in% depth)
    key_col <- c(key_col, "Depth")
  }
  df <- dplyr::left_join(df, df_sites,
                         by = "Site_ID",
                         keep = FALSE)

  df <- dplyr::select(df, dplyr::all_of(key_col))

  return(df)
}

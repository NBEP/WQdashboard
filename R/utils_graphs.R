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
prep_plot_df <- function(df, site_id, parameter){
  key_col <- c("Site_ID", "Site_Name", "Date", "Year", "Parameter", "Result",
               "Result_Unit", "Group", "Depth_Category")

  df <- dplyr::filter(df, Site_ID %in% site_id & Parameter %in% parameter)
  df <- dplyr::left_join(df, df_sites,
                         by = "Site_ID",
                         keep = FALSE)

  key_col <- intersect(key_col, colnames(df))
  df <- dplyr::select(df, dplyr::all_of(key_col))

  return(df)
}

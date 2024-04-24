#' format_results
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
format_score <- function(df){
  # Add null rows for missing data
  list_sites <- unique(df_sites$Site_ID)
  list_years <- unique(df_data$Year)
  list_param <- unique(df_data$Parameter)
  list_depth <- NA

  if("Depth_Category" %in% colnames(df_data)) {
    list_depth <- unique(df_data$Depth_Category)
  } else {
    df <- dplyr::mutate(df, Depth_Category = NA)
  }

  df_present <- df %>%
    dplyr::select(Site_ID, Year, Depth_Category) %>%
    unique()
  df_all <- expand.grid(list_sites, list_years, list_depth)
  colnames(df_all) <- c("Site_ID", "Year", "Depth_Category")
  df_missing <- dplyr::setdiff(df_all, df_present)

  df_join <- merge(df_present, list_param, by = NULL) %>%
    dplyr::rename(Parameter = y)
  df_join <- merge(df_join, list_depth, by = NULL) %>%
    dplyr::rename(Depth_Parameter = y)
  df_join <- merge(df_join, df, all.x = TRUE)

  df <- dplyr::bind_rows(df_join, df_missing)

  # Drop depth column if blank
  chk <- is.na(df$Depth_Category)
  if (all(chk)) {
    df <- dplyr::select(df, !Depth_Category)
  } else {
    df <- dplyr::rename(df, Depth = Depth_Category)
  }

  # Add site data ----
  site_col <- c("Site_ID", "Site_Name", "Latitude", "Longitude", "Town_Code",
                "County_Code", "State", "Watershed", "Group")
  site_col <- intersect(colnames(df_sites), site_col)
  sites_temp <- dplyr::select(df_sites, all_of(site_col))
  if ("Town_Code" %in% colnames(sites_temp)) {
    sites_temp <- sites_temp %>%
      dplyr::select(!State) %>%
      dplyr::rename(Town = Town_Code)
  } else if ("County_Code" %in% colnames(sites_temp)) {
    sites_temp <- sites_temp %>%
      dplyr::select(!State) %>%
      dplyr::rename(County = County_Code)
  }

  df <- merge(df, sites_temp, all.x = TRUE)

  return(df)
}

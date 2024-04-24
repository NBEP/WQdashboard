#' format_results
#'
#' @description Formats water quality data for use in app. Must run
#'   `QAQC_results` first.
#'
#' @param df Input dataframe.
format_results <- function(df){
  # Prep data for download -----------------------------------------------------
  message("Formatting df_data_all...\n")
  df_data_all <- df
  usethis::use_data(df_data_all, overwrite = TRUE)
  message("df_data_all saved")

  # Prep data for app ----------------------------------------------------------
  message("\nFormatting df_data...\n")
  # Drop extra columns
  field_all <- colnames_results$WQdashboard
  field_drop <- c("Depth", "Depth_Unit")
  field_keep <- intersect(field_all, colnames(df))
  field_keep <- field_keep[!field_keep %in% field_drop]

  chk <- length(df) - length(field_keep)
  if (chk > 0) {
    df <- dplyr::select(df, dplyr::all_of(field_keep))
    message("\t", toString(chk), " columns removed")
  }
  # Drop extra rows
  if("Qualifier" %in% colnames(df)) {
    chk <- df$Qualifier %in% qaqc_fail

    if(any(chk)){
      df <- dplyr::filter(df, !Qualifier %in% qaqc_fail)
      message("\tDropped flagged data")
    }
    df <- dplyr::select(df, !Qualifier)
  }
  if("Activity_Type" %in% colnames(df)) {
    chk <- stringr::str_detect(df$Activity_Type, "Quality Control")

    if(any(chk)){
      df <- dplyr::filter(df,
        !stringr::str_detect(Activity_Type, "Quality Control"))
      message("\tDropped replicate, blank data")
    }
    df <- dplyr::select(df, !Activity_Type)
  }
  # Add new columns
  df <- df %>%
    dplyr::mutate(Year = lubridate::year(Date)) %>%
    dplyr::mutate(Month = strftime(Date, "%B"))
  # Replace BDL with 0 -- not ideal, but only way to avoid skewing results
  df$Result[df$Result == "BDL"] <- 0
  df$Result <- as.numeric(df$Result)

  # Save data
  df_data <- df
  usethis::use_data(df_data, overwrite = TRUE)
  message("df_data saved")

  # Calculate scores -----------------------------------------------------------
  message("\nFormatting df_score...\n")
  field_group <- c("Site_ID", "Parameter", "Result_Unit", "Year")
  if ("Depth_Category" %in% colnames(df)) {
    field_group <- c(field_group, "Depth_Category")
  }

  df <- df %>%
    dplyr::group_by_at(dplyr::all_of(field_group)) %>%
    dplyr::summarise(
      score_max = max(Result),
      score_min = min(Result),
      score_mean = mean(Result),
      score_median = median(Result),
      .groups = "drop")
  message("\tGrouped data by year\n\tCalculating scores...")

  df <- df %>%
    dplyr::mutate(
      score = mapply(
        function(id, par, unit, a, b, c, d)
          calculate_score(id, par, unit, a, b, c, d),
        Site_ID, Parameter, Result_Unit,
        score_max, score_min, score_mean, score_median,
        SIMPLIFY = FALSE)) %>%
    tidyr::unnest_wider(score) %>%
    dplyr::select(!score_max:score_median) %>%
    dplyr::mutate(score_num = dplyr::if_else(
      score_num <1,
      signif(score_num, 2),
      round(score_num, 2)))
  message("\t... ok")

  # Add missing site/year/parameter combos ------------------------------------
  list_sites <- unique(df_sites$Site_ID)
  list_years <- unique(df_data$Year)
  list_param <- unique(df_data$Parameter)

  df_present <- df %>%
    select(Site_ID, Year) %>%
    unique()

  df_all <- expand.grid(list_sites, list_years)
  colnames(df_all) <- c("Site_ID", "Year")
  df_missing <- setdiff(df_all, df_present)
  df_join <- merge(df_present, list_param, by = NULL) %>%
    dplyr::rename(Parameter = y)
  df_join <- merge(df_join, df, all.x = TRUE)
  df_score <- dplyr::bind_rows(df_join, df_missing)

  # Add site data
  site_col <- c("Site_ID", "Site_Name", "Latitude", "Longitude", "Town_Code",
                "County_Code", "State", "Watershed", "Group")
  site_col <- intersect(colnames(df_sites), site_col)
  if (any(c("Town_Code", "County_Code") %in% colnames(df_sites))) {
    site_col <- site_col[site_col != "State"]
  }
  sites_temp <- dplyr::select(df_sites, all_of(site_col))

  df_score <- merge(df_score, sites_temp, all.x = TRUE)

  message("Filled in missing data")

  usethis::use_data(df_score, overwrite = TRUE)
  message("df_score saved")
}

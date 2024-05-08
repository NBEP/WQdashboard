#' format_results
#'
#' @description Formats water quality data for use in app. Must run
#'   `QAQC_results` first.
#'
#' @param df Input dataframe.
format_results <- function(df, default_state = NA){
  # Validate data
  chk <- is.na(default_state) | default_state %in% c(state.name, state.abb)
  if (!chk) {
    stop(default_state, " is not a valid state", call. = FALSE)
  }
  if (default_state %in% state.name) {
    default_state <- state.abb[match(default_state, state.name)]
  }

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
  # Tweak columns
  df <- dplyr::rename(df, Unit = Result_Unit)
  if ("Depth_Category" %in% colnames(df)) {
    df <- dplyr::rename(df, Depth = Depth_Category)
  }
  df$Result[df$Result == "BDL"] <- 0
  df$Result <- as.numeric(df$Result)

  # Save data
  df_data <- df %>%
    dplyr::mutate(Parameter = dplyr::if_else(
      Parameter == "Escherichia coli", "E. coli", Parameter))
  usethis::use_data(df_data, overwrite = TRUE)
  message("df_data saved")

  # Calculate scores -----------------------------------------------------------
  message("\nFormatting df_score...\n")
  field_group <- c("Site_ID", "Parameter", "Unit", "Year")
  if ("Depth" %in% colnames(df)) {
    field_group <- c(field_group, "Depth")
  }

  df <- df %>%
    dplyr::group_by_at(field_group) %>%
    dplyr::summarise(
      score_max = max(Result),
      score_min = min(Result),
      score_mean = mean(Result),
      score_median = median(Result),
      .groups = "drop")
  message("\tGrouped data by year\n\tCalculating scores...")

  df <- df %>%
    dplyr::mutate(temp_state = default_state) %>%
    dplyr::mutate(
      score_temp = mapply(
        function(id, par, unit, state, a, b, c, d)
          calculate_score(id, par, unit, state, a, b, c, d),
        Site_ID, Parameter, Unit, temp_state,
        score_max, score_min, score_mean, score_median,
        SIMPLIFY = FALSE)) %>%
    tidyr::unnest_wider(score_temp) %>%
    dplyr::select(!c(score_max:score_median, temp_state)) %>%
    dplyr::mutate(score_num = dplyr::if_else(
      score_num <1,
      signif(score_num, 2),
      round(score_num, 2)))
  message("\t... ok")

  # Format scores ------------------------------------------------------------
  # Generate dataframe of site/year/parameter/depth combinations
  list_sites <- unique(df_sites$Site_ID)
  list_years <- unique(df_data$Year)
  list_param <- unique(df_data$Parameter)

  df_present <- df %>%
    dplyr::select(Site_ID, Year) %>%
    unique()
  df_all <- expand.grid(list_sites, list_years)
  colnames(df_all) <- c("Site_ID", "Year")
  df_missing <- dplyr::setdiff(df_all, df_present)

  df_join <- merge(df_present, list_param, by = NULL) %>%
    dplyr::rename(Parameter = y)
  if("Depth" %in% colnames(df_data)) {
    df_join <- merge(df_join, unique(df_data$Depth), by = NULL) %>%
      dplyr::rename(Depth = y)
  }

  # Join df with dataframe, add rows for missing site/year combos
  df_join <- merge(df_join, df, all.x = TRUE)
  df <- dplyr::bind_rows(df_join, df_missing)

  # Tidy data
  df <- check_val_count(df, "Depth")

  # Add site data
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

  # Final tweaks
  col_order <- c("Year", "Site_Name", "Site_ID", "Town", "County", "State",
                 "Watershed", "Group", "Depth", "Parameter", "Unit",
                 "score_typ", "score_num", "score_str", "Latitude", "Longitude")
  col_order <- intersect(col_order, colnames(df))

  df <- df %>%
    dplyr::select(dplyr::all_of(col_order)) %>%
    dplyr::mutate(score_str = dplyr::case_when(
      !is.na(score_str) ~ score_str,
      !is.na(score_num) ~ "No Threshold Established",
      TRUE ~ "No Data Available")) %>%
    dplyr::mutate(Parameter = dplyr::if_else(
      is.na(Parameter), "-", Parameter)) %>%
    dplyr::mutate(Parameter = dplyr::if_else(
      Parameter == "Escherichia coli", "E. coli", Parameter)) %>%
    dplyr::arrange(Site_Name, Parameter)

  df_score <- add_popup_text(df)

  usethis::use_data(df_score, overwrite = TRUE)
  message("df_score saved")

  # Generate drop down lists --------------------------------------------------
  message("\nGenerating dropdown lists\n")

  # list_sites <- unique(df_data$Site_ID)
  # usethis::use_data(list_sites, overwrite = TRUE)
  list_param <- unique(df_data$Parameter)
  usethis::use_data(list_param, overwrite = TRUE)

  df_temp <- df_score %>%
    dplyr::filter(!score_str %in% c("No Data Available",
                                    "No Threshold Established"))
  list_param_short <- unique(df_temp$Parameter)
  usethis::use_data(list_param_short, overwrite = TRUE)

  message("\nFinished processing data")
}

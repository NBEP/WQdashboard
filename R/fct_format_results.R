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
  df <- df %>%
    dplyr::rename(Unit = Result_Unit) %>%
    dplyr::rename(Depth = Depth_Category)
  df$Result[df$Result == "BDL"] <- 0
  df$Result <- as.numeric(df$Result)

  # Save data
  df_data <- df
  usethis::use_data(df_data, overwrite = TRUE)
  message("df_data saved")

  # Calculate scores -----------------------------------------------------------
  message("\nFormatting df_score...\n")
  field_group <- c("Site_ID", "Parameter", "Unit", "Year")
  if ("Depth" %in% colnames(df)) {
    field_group <- c(field_group, "Depth")
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
    dplyr::mutate(temp_state = default_state) %>%
    dplyr::mutate(
      score = mapply(
        function(id, par, unit, state, a, b, c, d)
          calculate_score(id, par, unit, state, a, b, c, d),
        Site_ID, Parameter, Unit, temp_state,
        score_max, score_min, score_mean, score_median,
        SIMPLIFY = FALSE)) %>%
    tidyr::unnest_wider(score) %>%
    dplyr::select(!score_max:score_median) %>%
    dplyr::select(!temp_state) %>%
    dplyr::mutate(score_num = dplyr::if_else(
      score_num <1,
      signif(score_num, 2),
      round(score_num, 2)))
  message("\t... ok")

  # Format scores ------------------------------------------------------------
  # Add rows for null data
  list_sites <- unique(df_sites$Site_ID)
  list_years <- unique(df_data$Year)
  list_param <- unique(df_data$Parameter)
  list_depth <- NA

  if("Depth" %in% colnames(df_data)) {
    list_depth <- unique(df_data$Depth)
  } else {
    df <- dplyr::mutate(df, Depth = NA)
  }

  df_present <- df %>%
    dplyr::select(Site_ID, Year) %>%
    unique()
  df_all <- expand.grid(list_sites, list_years)
  colnames(df_all) <- c("Site_ID", "Year")
  df_missing <- dplyr::setdiff(df_all, df_present)

  df_join <- merge(df_present, list_param, by = NULL) %>%
    dplyr::rename(Parameter = y)
  df_join <- merge(df_join, list_depth, by = NULL) %>%
    dplyr::rename(Depth = y)
  df_join <- merge(df_join, df, all.x = TRUE)

  df <- dplyr::bind_rows(df_join, df_missing)
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

  df_score <- merge(df, sites_temp, all.x = TRUE)

  usethis::use_data(df_score, overwrite = TRUE)
  message("df_score saved")
}

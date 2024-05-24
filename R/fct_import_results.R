#' QAQC_results
#'
#' @description Run quick quality control check on water quality data. Runs the
#'   following checks:
#'   * Checks column names, renames columns as needed
#'   * Checks all mandatory columns are present
#'   * Checks for missing values
#'   * Ensures date column in properly formatted
#'   * Renames parameters to match WQX standard
#'
#' @param df Input dataframe.
#' @param date_format Date format as string.
#'
#' @return Updated dataframe.
qaqc_results <- function(df, date_format=NULL){
  # Define variables ----------------------------------------------------------
  field_all <- colnames_results$WQdashboard
  field_need <- colnames_results$WQdashboard_short
  field_need <- field_need[!field_need == ""]
  field_optional <- dplyr::setdiff(field_all, field_need)
  field_skip <- c("Qualifier", "Depth", "Depth_Unit", "Depth_Category")

  # QAQC columns --------------------------------------------------------------
  message("Checking data...\n")
  df <- update_column_names(df, colnames_results)
  check_column_missing(df, field_need)

  # QAQC column values ---------------------------------------------------------
  # Check missing data
  for (field in field_need) {
    check_val_missing(df, field = field)
  }
  field_check <- intersect(field_optional, colnames(df))
  field_check <- field_check[!field_check %in% field_skip]
  for (field in field_check) {
    check_val_missing(df, field = field, is_stop = FALSE)
  }
  # Check if all sites valid
  site_sites <- list_sites(df_sites)
  data_sites <- list_sites(df)

  chk <- data_sites %in% site_sites
  if(any(!chk)){
    extra_sites <- data_sites[!chk]
    stop("Site not in df_sites: ",
         paste(data_sites[!chk], collapse = ", "), call. = FALSE)
  }
  # Check column format
  check_val_numeric(df, field = "Result", exceptions="BDL")
  df <- format_date_col(df, date_format)

  # Check parameters, units
  df <- df %>%
    dplyr::mutate(
      Parameter = sapply(Parameter, function(x) rename_param(x))) %>%
    dplyr::mutate(
      Result_Unit = sapply(Result_Unit, function(x) rename_unit(x)))
  check_units(df)

  if("Depth" %in% colnames(df)) {
    df <- depth_to_m(df)
    df <- assign_depth_category(df)
  }

  # Remove surplus name attributes
  df[] <- lapply(df, unname)

  message("\nQAQC complete")
  return(df)
}

#' format_df_data
#'
#' @description Formats water quality data for use in app. Must run
#'   `QAQC_results` first.
#'
#' @param df Input dataframe.
#'
#' @return Updated dataframe.
format_df_data <- function(df){
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
      df <- df %>%
        dplyr::filter(!stringr::str_detect(Activity_Type, "Quality Control"))
      message("\tDropped replicate, blank data")
    }
    df <- dplyr::select(df, !Activity_Type)
  }

  # Tweak columns
  df <- dplyr::rename(df, Unit = Result_Unit)
  if ("Depth_Category" %in% colnames(df)) {
    df <- dplyr::rename(df, Depth = Depth_Category)
  } else {
    df <- dplyr::mutate(df, Depth = NA)
  }
  df$Result[df$Result == "BDL"] <- 0
  df$Result <- as.numeric(df$Result)

  # Add columns for Month, Year
  df <- df %>%
    dplyr::mutate(Year = lubridate::year(Date)) %>%
    dplyr::mutate(Month = strftime(Date, "%B"))

  # Add Site_Name, Description
  field_keep <- c(colnames(df), "Site_Name")
  df <- left_join(df, df_sites, by = "Site_ID") %>%
    dplyr::select(dplyr::all_of(field_keep)) %>%
    dplyr::mutate(Description = paste0(
      "<b>", Site_Name, "</b><br>",
      format(Date, format = "%B %d, %Y"), "<br>",
      Parameter, ": ", Result)) %>%
    dplyr::mutate(Description = dplyr::if_else(
      !Unit %in% c(NA, "None"),
      paste(Description, Unit),
      Description))

  return(df)
}

#' format_results
#'
#' @description Formats water quality data for use in app. Must run
#'   `format_df_data` first.
#'
#' @param df Input dataframe.
#' @param default_state Which state to use when calculating score, thresholds.
#'
#' @return Updated dataframe.
format_df_score <- function(df, default_state = NA){
  # Check if default_state is valid
  chk <- is.na(default_state) | default_state %in% c(state.name, state.abb)
  if (!chk) {
    stop(default_state, " is not a valid state", call. = FALSE)
  }
  if (default_state %in% state.name) {
    default_state <- state.abb[match(default_state, state.name)]
  }

  # Calculate scores, etc
  message("\nFormatting df_score...\n")
  field_group <- c("Site_ID", "Depth", "Parameter", "Unit", "Year")

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
        function(id, par, unit, depth, state, a, b, c, d)
          calculate_score(id, par, unit, depth, state, a, b, c, d),
        Site_ID, Parameter, Unit, Depth, temp_state,
        score_max, score_min, score_mean, score_median,
        SIMPLIFY = FALSE)) %>%
    tidyr::unnest_wider(score_temp) %>%
    dplyr::select(!c(score_max:score_median, temp_state)) %>%
    dplyr::mutate(score_num = dplyr::if_else(
      score_num <1,
      signif(score_num, 2),
      round(score_num, 2)))
  df <- suppressMessages(check_val_count(df, "Depth"))
  message("\t... ok")

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
  if("Depth" %in% colnames(df)) {
    df_join <- merge(df_join, unique(df$Depth), by = NULL) %>%
      dplyr::rename(Depth = y)
  }

  # Join df with dataframe, add rows for missing site/year combos
  df_join <- merge(df_join, df, all.x = TRUE)
  df <- dplyr::bind_rows(df_join, df_missing)

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
    dplyr::arrange(Site_Name, Parameter)

  df <- add_popup_text(df)

  return(df)
}

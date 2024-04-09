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
  field_keep <- intersect(field_all, colnames(df))
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
  # Replace BDL with -999999
  df$Result[df$Result == "BDL"] <- -999999
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

  df_score <- df %>%
    dplyr::mutate(Result = dplyr::if_else(
      Result == -999999,
      0,
      Result)) %>%
    dplyr::group_by_at(dplyr::all_of(field_group)) %>%
    dplyr::summarise(
      score_min = min(Result),
      score_max = max(Result),
      score_mean = mean(Result),
      score_median = median(Result),
      .groups = "drop")

  usethis::use_data(df_score, overwrite = TRUE)
  message("df_score saved")
}

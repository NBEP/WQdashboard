# Update Data
#
# README: Run this script to add or update water quality data.
#
# Date abbreviations:
#  b Abbreviated month name (eg Feb)
#  B Full month name (eg February)
#  d Day of the month
#  y Year without century (eg 24)
#  Y Year with century (eg 2024)
#  H Hour
#  m Month
#  M Minute
#  p AM/PM
#  S Second
#  z Timezone

# parameter data:
results_csv <- "test_data_ww_all.csv"
in_format <- "RI_WW"
date_format <- "m/d/Y"

overwrite_existing <- TRUE

# CODE ------------------------------------------------------------------------
devtools::load_all()

df <- readr::read_csv(
  paste0("data-raw/", results_csv),
  show_col_types = FALSE,
  guess_max = Inf
)

if (in_format != "WQdashboard") {
  df <- preformat_results(df, in_format)
}

# QAQC data
df <- qaqc_results(df, date_format)
chk_years <- unique(df$Year)

# Combine datasets (if not overwriting)
if (!overwrite_existing) {
  keep_cols <- intersect(colnames(df), colnames(df_data_all))
  df <- dplyr::select(df, dplyr::all_of(keep_cols))
  df2 <- dplyr::select(df_data_all, dplyr::all_of(keep_cols))
  df <- rbind(df, df2) %>% unique()
  df <- standardize_units(df)
}

if (nrow(df) > 250000) {
  warning(
    "Dataset contains over 250,000 rows and website is likely to experience performance issues",
    call. = FALSE
  )
}

# Upload data (all)
df_data_all <- df %>%
  dplyr::mutate(
    "Description" = paste0(
      "<b>", .data$Site_Name, "</b><br>",
      format(.data$Date, format = "%B %d, %Y")
    )
  ) %>%
  dplyr::mutate(
    "Description" = dplyr::if_else(
      !is.na(.data$Depth),
      paste0(.data$Description, "<br>Depth: ", Depth),
      .data$Description
    )
  ) %>%
  dplyr::mutate(
    "Description" = paste0(
      .data$Description, "<br>", .data$Parameter, ": ",
      pretty_number(.data$Result)
    )
  ) %>%
  dplyr::mutate(
    "Description" = dplyr::if_else(
      !.data$Unit %in% c(NA, "None"),
      paste(.data$Description, .data$Unit),
      .data$Description
    )
  )

usethis::use_data(df_data_all, overwrite = TRUE)
message("Saved df_data_all")

# Format, upload data (short)
df <- format_results(df)

sites <- dplyr::select(site_data, c("Site_ID", "Site_Name"))

dat <- dplyr::left_join(dat, sites, by = "Site_ID") %>%
  dplyr::mutate(
    "Description" = paste0(
      "<b>", Site_Name, "</b><br>", format(Date, format = "%B %d, %Y"), "<br>"
    )
  ) %>%
  dplyr::mutate(Description = dplyr::if_else(
    !is.na(Depth),
    paste0(Description, "Depth: ", Depth, "<br>"),
    Description
  )) %>%
  dplyr::mutate(Description = paste0(
    Description,
    Parameter, ": ", pretty_number(Result)
  )) %>%
  dplyr::mutate(Description = dplyr::if_else(
    !Unit %in% c(NA, "None"),
    paste(Description, Unit),
    Description
  ))

df_data <- df
usethis::use_data(df_data, overwrite = TRUE)
message("Saved df_data")

# Calculate, upload scores
if (overwrite_existing) {
  df_score <- format_score(df)
} else {
  score_old <- dplyr::filter(df_score, !Year %in% chk_years)
  df_new <- dplyr::filter(df, Year %in% chk_years)
  score_new <- format_score(df_new)
  df_score <- rbind(score_old, score_new)
}
usethis::use_data(df_score, overwrite = TRUE)
message("Saved df_score \n\nFinished processing data")

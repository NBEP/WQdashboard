# Add thresholds
#
# README: (add detailed instructions)

# Custom thresholds:
thresholds <- "test_threshold_brc.csv"

# CODE ------------------------------------------------------------------------
devtools::load_all()

# Import data
df_thresholds <- read.csv(paste0("data-raw/", thresholds),
                     na.strings=c("","NA"),
                     check.names=FALSE) %>%
  dplyr::mutate_if(is.character, trimws)

# Check data
df_thresholds <- qaqc_thresholds(df_thresholds)
usethis::use_data(df_thresholds, overwrite = TRUE)
message("\nFinished processing data")

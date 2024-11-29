# Dev script.
#
# README: This script defines a series of universal variables necessary for the
#  app to run. DOES NOT NEED TO BE RUN UNLESS VARIABLES CHANGE.

devtools::load_all()

# Set unit conversions
varnames_units <- readr::read_csv(
  "data-raw/varnames_units.csv",
  show_col_types = FALSE)
usethis::use_data(varnames_units, overwrite = TRUE)

# Set thresholds
state_thresholds <- readr::read_csv(
    "data-raw/state_thresholds.csv",
    show_col_types = FALSE)
state_thresholds <- qaqc_thresholds(state_thresholds)

epa_thresholds <- readr::read_csv(
    "data-raw/epa_thresholds.csv",
    show_col_types = FALSE)
epa_thresholds <- qaqc_thresholds(epa_thresholds)

official_thresholds <- dplyr::bind_rows(state_thresholds, epa_thresholds)
usethis::use_data(official_thresholds, overwrite = TRUE)

# Set QAQC fails -- use data from EPATADA
if(!"remotes" %in% installed.packages()){ install.packages("remotes") }
remotes::install_github("USEPA/EPATADA", ref = "develop")

tada_qual_flags <- system.file(
  "extdata",
  "WQXMeasureQualifierCodeRef.csv",
  package = "EPATADA"
)

tada_qual_flags <- readr::read_csv(
  tada_qual_flags,
  show_col_types = FALSE
) %>%
  dplyr::select(Code, "TADA.MeasureQualifierCode.Flag") %>%
  dplyr::rename(Flag = "TADA.MeasureQualifierCode.Flag")

qaqc_suspect <- dplyr::filter(tada_qual_flags, Flag == "Suspect")$Code
qaqc_nondetect <- dplyr::filter(tada_qual_flags, Flag == "Non-Detect")$Code

qaqc_flag <- list(suspect = qaqc_suspect, nondetect = qaqc_nondetect)
usethis::use_data(qaqc_flag, overwrite = TRUE)

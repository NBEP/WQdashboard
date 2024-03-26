# Dev script.
#
# README: This script defines a series of universal variables necessary for the
#  app to run. DOES NOT NEED TO BE RERUN UNLESS VARIABLES CHANGE, DO NOT EDIT.

library(dplyr)

colnames_sites <- read.csv("data-raw/colnames_sites.csv") %>%
  dplyr::mutate_if(is.character, trimws) %>%
  dplyr::relocate(WQdashboard) %>%
  dplyr::relocate(WQdashboard_short, .after = last_col())
usethis::use_data(colnames_sites, overwrite = TRUE)

colnames_results <- read.csv("data-raw/colnames_results.csv") %>%
  dplyr::mutate_if(is.character, trimws) %>%
  dplyr::relocate(WQdashboard) %>%
  dplyr::relocate(WQdashboard_short, .after = last_col())
usethis::use_data(colnames_results, overwrite = TRUE)

varnames_parameters <- read.csv("data-raw/varnames_parameters.csv")
param_names <- varnames_parameters$Param_WQX
names(param_names) <- varnames_parameters$Param_Other
usethis::use_data(param_names, overwrite = TRUE)

qaqc_fail <- c("$", "A", "AR", "BVER", "C", "CAN", "CBC", "CSR", "DE", "EER",
  "EFAI", "FDB", "FDC", "FDL", "FEQ", "FFB", "FFD", "FFS", "FFT", "FH", "FIS",
  "FL", "FLC", "FLD", "FLS", "FMD", "FMS", "FPC", "FPP", "FPR", "FQC", "FRS",
  "FSD", "FSL", "FSP", "FUB", "H", "H2", "H3", "HIB", "IQCOL", "IS", "ISAC",
  "ISP", "ISR", "ISR**", "ITNA", "ITNM", "JCW", "KCF", "KCX", "KK", "LAC",
  "LBF", "LL", "LLS", "NAI", "NLBL", "NLRO", "NN", "NPNF", "NRO", "NRR", "NRS",
  "NSQ", "PNQ", "PP", "PPD", "Q", "QC", "QCI", "R", "RNON", "S2", "SCA", "SCF",
  "SCP", "SCX", "SSR", "SUS", "UNC")
usethis::use_data(qaqc_fail, overwrite = TRUE)

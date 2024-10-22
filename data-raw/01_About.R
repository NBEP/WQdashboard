# Add About Page
#
# README: todo

# Organization Name:
org_name <- "Blackstone River Coalition"
program_name <- "Blackstone River Watershed-wide Volunteer Water Quality Monitoring Program"
site_url <- "https://zaptheblackstone.org"

# CODE ------------------------------------------------------------------------
org_info <- list(
  "name" = org_name,
  "program_name" = program_name,
  "url" = site_url)
usethis::use_data(org_info, overwrite = TRUE)

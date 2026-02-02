# Add About Page
#
# README: todo

# Organization Info
org_name <- "Watershed Watch"
suggested_citation <- "Watershed Watch, YEAR, Watershed Watch Water Quality Monitoring Program data available on the world wide web, accessed [DATE]."

# SHAPEFILE - Watershed Boundaries
watershed_shp <- NULL
watershed_name_col <- "HUC12_Name"

# SHAPEFILE - Rivers
river_shp <- NULL
river_name_col <- "gnis_name"
river_group_col <- "Group"

# CODE ------------------------------------------------------------------------
library(sf)
library(dplyr)
library(stringr)
library(tidyr)

# Import org info ----
org_info <- list(
  "name" = org_name,
  "citation" = suggested_citation
)
usethis::use_data(org_info, overwrite = TRUE)

# Import shapefiles ----
if (is.null(watershed_shp)) {
  shp_watershed <- NULL
} else {
  shp_watershed <- read_sf(dsn = "data-raw", layer = watershed_shp) %>%
    dplyr::select(dplyr::all_of(watershed_name_col))
  colnames(shp_watershed)[1] <- "Name"
  shp_watershed <- shp_watershed %>%
    dplyr::mutate(
      "Name" = dplyr::if_else(
        stringr::str_detect(.data$Name, "Watershed"),
        .data$Name,
        paste(.data$Name, "Watershed")
      )
    )
}
usethis::use_data(shp_watershed, overwrite = TRUE)

if (is.null(river_shp)) {
  shp_river <- NULL
} else {
  shp_river <- read_sf(dsn = "data-raw", layer = river_shp)

  old_field <- c(river_name_col, river_group_col)
  keep_field <- intersect(colnames(shp_river), old_field)
  new_field <- c("Name", "Group")
  names(new_field) <- old_field
  new_field <- new_field[keep_field]

  shp_river <- shp_river %>%
    dplyr::select(dplyr::all_of(keep_field)) %>%
    dplyr::rename_with(~new_field, names(new_field)) %>%
    dplyr::mutate("Label" = "Waterbody")

  if (all(c("Name", "Group") %in% colnames(shp_river))) {
    shp_river <- shp_river %>%
      dplyr::mutate(
        "Name" = tidyr::replace_na(.data$Name, "Unnamed Waterbody")
      ) %>%
      dplyr::mutate(
        "Group" = tidyr::replace_na(.data$Group, "Other")
      ) %>%
      dplyr::mutate(
        "Label" = paste0(.data$Name, ", ", .data$Group)
      ) %>%
      dplyr::mutate(
        "Popup" = paste0("<b>", .data$Name, "</b><br>", .data$Group)
      )
  } else if ("Name" %in% colnames(shp_river)) {
    shp_river <- shp_river %>%
      dplyr::mutate(
        "Name" = tidyr::replace_na(.data$Name, "Unnamed Waterbody")
      ) %>%
      dplyr::mutate("Label" = .data$Name) %>%
      dplyr::mutate("Popup" = paste0("<b>", .data$Name, "</b>"))
  } else if ("Group" %in% colnames(shp_river)) {
    shp_river <- shp_river %>%
      dplyr::mutate("Group" = tidyr::replace_na(.data$Group, "Other")) %>%
      dplyr::mutate("Label" = .data$Group) %>%
      dplyr::mutate("Popup" = paste0("<b>", .data$Group, "</b>"))
  }
}

usethis::use_data(shp_river, overwrite = TRUE)

rm(list = ls())

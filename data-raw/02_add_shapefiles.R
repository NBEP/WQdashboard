# Add Shapefiles
#
# README: todo
# All shapefiles must be projected to WGS 1984.

# Watershed Boundaries
watershed_shp <- "test_watershed_brc"
watershed_name_col <- "HUC12_Name"

# Rivers
river_shp <- "test_rivers_brc"
river_name_col <- "gnis_name"
river_group_col <- "Group"

# CODE ------------------------------------------------------------------------
library(sf)
library(dplyr)
library(stringr)
library(tidyr)

if (!is.na(watershed_shp)) {
  shp_watershed <- read_sf(dsn = "data-raw", layer = watershed_shp) %>%
    dplyr::select(dplyr::all_of(watershed_name_col))
  colnames(shp_watershed)[1] <- "Name"
  shp_watershed <- shp_watershed %>%
    dplyr::mutate(
      Name = dplyr::if_else(
        stringr::str_detect(Name, "Watershed"),
        Name,
        paste(Name, "Watershed")
      )
    )

  usethis::use_data(shp_watershed, overwrite = TRUE)
}

if (!is.na(river_shp)) {
  shp_river <- read_sf(dsn = "data-raw", layer = river_shp)

  old_field <- c(river_name_col, river_group_col)
  keep_field <- intersect(colnames(shp_river), old_field)
  new_field <- c("Name", "Group")
  names(new_field) <- old_field
  new_field <- new_field[keep_field]

  shp_river <- shp_river %>%
    dplyr::select(dplyr::all_of(keep_field)) %>%
    dplyr::rename_with(~ new_field, names(new_field)) %>%
    dplyr::mutate(Label = "Waterbody")

  if (all(c("Name", "Group") %in% colnames(shp_river))) {
    shp_river <- shp_river %>%
      dplyr::mutate(Name = tidyr::replace_na(Name, "Unnamed Waterbody")) %>%
      dplyr::mutate(Group = tidyr::replace_na(Group, "Other")) %>%
      dplyr::mutate(Label = paste0(Name, ", ", Group)) %>%
      dplyr::mutate(Popup = paste0("<b>", Name, "</b><br>", Group))
  } else if ("Name" %in% colnames(shp_river)) {
    shp_river <- shp_river %>%
      dplyr::mutate(Name = tidyr::replace_na(Name, "Unnamed Waterbody")) %>%
      dplyr::mutate(Label = Name) %>%
      dplyr::mutate(Popup = paste0("<b>", Name, "</b>"))
  } else if ("Group" %in% colnames(shp_river)) {
    shp_river <- shp_river %>%
      dplyr::mutate(Group = tidyr::replace_na(Group, "Other")) %>%
      dplyr::mutate(Label = Group) %>%
      dplyr::mutate(Popup = paste0("<b>", Group, "</b>"))
  }

  usethis::use_data(shp_river, overwrite = TRUE)
}

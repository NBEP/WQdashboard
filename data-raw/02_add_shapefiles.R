# Add Shapefiles - OPTIONAL
#
# THIS STEP IS OPTIONAL. If you would like to add custom watershed or river
# layers to the interactive map, use this script to upload shapefiles.
#
# Save shapefiles to folder "data-raw/shp" before running this script.
#
# VARIABLES
# watershed_shp: Name of watershed shapefile. Shapefile must be a polygon layer.
#
# watershed_name_col: Field name for watershed shapefile. This field will be
# used to label each polygon.
#
# river_shp: Name of river shapefile. Shapefile must be a polyline layer.
#
# river_name_col: Field name for river shapefile. This field will be
# used to label each polyline.
#
# To run this script, update variables below and then use CTRL + SHIFT + ENTER


# SHAPEFILE - Watershed Boundaries
watershed_shp <- NA
watershed_name_col <- "Field"

# SHAPEFILE - Rivers
river_shp <- NA
river_name_col <- "Field"

# CODE ------------------------------------------------------------------------
library(sf)
library(dplyr)
library(tidyr)

# Import shapefiles ----
if (!is.null(watershed_shp) && !is.na(watershed_shp)) {
  shp_watershed <- sf::read_sf(dsn = "data-raw/shp", layer = watershed_shp) |>
    dplyr::select(!!watershed_name_col)
  colnames(shp_watershed)[1] <- "Label"

  usethis::use_data(shp_watershed, overwrite = TRUE)
}

if (!is.null(river_shp) && !is.na(river_shp)) {
  shp_river <- sf::read_sf(dsn = "data-raw/shp", layer = river_shp) |>
    dplyr::select(!!river_name_col)
  colnames(shp_river)[1] <- "Label"

  usethis::use_data(shp_river, overwrite = TRUE)
}

rm(list = ls(all.names = TRUE))

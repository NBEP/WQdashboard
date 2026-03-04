# Add Shapefiles - OPTIONAL
#
#' @description Run this script if you would like to add watershed or river
#' shapefiles to the map. Shapefiles should be saved in the folder
#' `data-raw/shp`
#'
#' @param watershed_shp Watershed shapefile. Must be polygons.
#' @param watershed_name_col Watershed shapefile column name used to label each
#' polygon.
#'
#' @param river_shp River shapefile. Must be polylines.
#' @param river_name_col River shapefile column name used to label each
#' polyline.
#'
#' @noRd

# SHAPEFILE - Watershed Boundaries
watershed_shp <- "test_watershed_brc"
watershed_name_col <- "HUC12_Name"

# SHAPEFILE - Rivers
river_shp <- "test_rivers_brc"
river_name_col <- "Label"

# CODE ------------------------------------------------------------------------
library(sf)
library(dplyr)
library(tidyr)
library(shiny, include.only="isTruthy")

# Import shapefiles ----
if (shiny::isTruthy(watershed_shp)) {
  shp_watershed <- sf::read_sf(dsn = "data-raw/shp", layer = watershed_shp) |>
    dplyr::select(!!watershed_name_col)
  colnames(shp_watershed)[1] <- "Label"

  usethis::use_data(shp_watershed, overwrite = TRUE)
}

if (shiny::isTruthy(river_shp)) {
  shp_river <- sf::read_sf(dsn = "data-raw/shp", layer = river_shp) |>
    dplyr::select(!!river_name_col)
  colnames(shp_river)[1] <- "Label"

  usethis::use_data(shp_river, overwrite = TRUE)
}

rm(list = ls())
